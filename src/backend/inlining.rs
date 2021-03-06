//! Function inlining using a max-size heuristic.
//!
//! Based on the provided inlining value (passed from low, medium, high inlining
//! options in the compiler's CLI), functions are checked to determine if they
//! are small enough to be inlined.
//!
//! Strongly connected components such as directly mutually recursive, or
//! recursive functions are not inlined.
//!
//! This operation is very expensive, and is time consuming on very high
//! inlining levels, however it opens many other potential optimisation, namely:
//! - Dead Code removal cannot optimise function calls, inlining calls allows their
//!   dead code to be removed.
//! - Tail Call Optimisation also cannot optimise around potentially
//!   side-effectual function calls, inlining can alleviate this.
//! - Constant propagation cannot be done through function calls (as function
//!   may be called from many locations, and we want the function to be capable
//!   of being linked, so it must respect the arm calling convention). Hence
//!   when the user does not want to link, they can use inlining to allow
//!   constants to be propagated through the function's definition.

use super::data_flow::DataflowNode;
use super::three_code::*;
use crate::{
    graph::{Deleted, Graph},
    intermediate::VarRepr,
};
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet, LinkedList},
    iter::zip,
    sync::Arc,
};

/// Heuristic for how many instructions more or less a [stat type](StatType) is
/// converted to in the assembly conversion.
fn get_stat_size_heuristic(stat: &StatType) -> f64 {
    match stat {
        StatType::Return(_, _) => 4.0,
        StatType::Loop(_) => 1.0,
        StatType::Branch(_, _, _, _) => 1.2,
        StatType::Simple(_, _, _) => 2.7,
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

/// Helper recursive function for [getting the function statistics](get_function_statistics).
fn get_function_statistics_dfs(
    calls: &mut Vec<String>,
    node: &StatNode,
    used: &mut HashSet<StatNode>,
) -> f64 {
    if used.contains(node) {
        return 0.0;
    }
    used.insert(node.clone());
    if let StatType::Simple(_, StatCode::Call(_, fname, _) | StatCode::VoidCall(fname, _), _) =
        &*node.get()
    {
        calls.push(fname.clone());
    }
    let mut total_sum = get_stat_size_heuristic(&*node.get());
    match &*node.get() {
        StatType::Branch(_, _, true_node, false_node) => {
            total_sum += get_function_statistics_dfs(calls, true_node, used);
            total_sum += get_function_statistics_dfs(calls, false_node, used);
        }
        StatType::Simple(_, _, next_node) => {
            total_sum += get_function_statistics_dfs(calls, next_node, used);
        }
        StatType::Return(_, _) | StatType::Loop(_) => {}
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    };
    total_sum
}

/// Returns the function size heuristic and the calls that it makes to other
/// functions.
fn get_function_statistics(
    Function {
        args: _,
        code,
        graph: _,
        read_ref: _,
    }: &Function,
) -> (f64, Vec<String>) {
    let mut calls = Vec::new();
    let total_sum = get_function_statistics_dfs(&mut calls, code, &mut HashSet::new());
    (total_sum, calls)
}

/// Helper recursive function for the [strongly connected subcomponents calculation](get_function_sccs).
fn get_function_sccs_dfs<'l>(
    post_order: &mut Vec<&'l str>,
    fname: &'l str,
    visited: &mut HashSet<&'l str>,
    call_graph: &HashMap<&'l str, Vec<&'l str>>,
) {
    if visited.contains(fname) {
        return;
    }
    visited.insert(fname);
    for &call in &call_graph[fname] {
        get_function_sccs_dfs(post_order, call, visited, call_graph);
    }
    post_order.push(fname);
}

/// Helper recursive function for the [strongly connected subcomponents calculation](get_function_sccs).
fn get_function_sccs_scc<'l>(
    group: &mut HashSet<&'l str>,
    fname: &'l str,
    visited: &mut HashSet<&'l str>,
    transpose_call_graph: &HashMap<&'l str, Vec<&'l str>>,
) {
    if visited.contains(fname) {
        return;
    }
    visited.insert(fname);
    group.insert(fname);
    for &call in &transpose_call_graph[fname] {
        get_function_sccs_scc(group, call, visited, transpose_call_graph);
    }
}

/// Calculates the strongly connected components for all the functions, i.e.
/// groups functions together if there is an indirect possible call path
/// from one function to another and vice versa.
fn get_function_sccs<'l>(
    call_graph: &HashMap<&'l str, Vec<&'l str>>,
    transpose_call_graph: &HashMap<&'l str, Vec<&'l str>>,
) -> HashMap<&'l str, Arc<HashSet<&'l str>>> {
    let mut visited = HashSet::new();
    let mut post_order = Vec::new();
    for &fname in call_graph.keys() {
        get_function_sccs_dfs(&mut post_order, fname, &mut visited, call_graph);
    }
    let mut sccs = HashMap::new();
    let mut visited = HashSet::new();
    for fname in post_order.into_iter().rev() {
        let mut group = HashSet::new();
        get_function_sccs_scc(&mut group, fname, &mut visited, transpose_call_graph);
        let group = Arc::new(group);
        for &group_fname in group.iter() {
            sccs.insert(group_fname, group.clone());
        }
    }
    sccs
}

/// Helper function for creating a mapping of variable identifiers from the old three code graph
/// to the new one.
fn get_mapping(
    var: VarRepr,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> VarRepr {
    if let Some(&mapping) = variable_mappings.get(&var) {
        mapping
    } else {
        let new_mapping = *new_variable;
        variable_mappings.insert(var, new_mapping);
        *new_variable += 1;
        new_mapping
    }
}

/// Substitutes the variable identifier from the old three code graph to the new
/// one for [op source](OpSrc).
fn substitute_op_src(
    op_src: OpSrc,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> OpSrc {
    if let OpSrc::Var(var) = op_src {
        OpSrc::Var(get_mapping(var, new_variable, variable_mappings))
    } else {
        op_src
    }
}

/// Substitutes the variable identifier from the old three code graph to the new
/// one for [statement code](StatCode).
fn substitute_vars(
    stat_code: &StatCode,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> StatCode {
    match stat_code {
        StatCode::Assign(var, op_src) => StatCode::Assign(
            get_mapping(*var, new_variable, variable_mappings),
            substitute_op_src(*op_src, new_variable, variable_mappings),
        ),
        StatCode::AssignOp(var, op_src1, bin_op, op_src2, check) => StatCode::AssignOp(
            get_mapping(*var, new_variable, variable_mappings),
            substitute_op_src(*op_src1, new_variable, variable_mappings),
            *bin_op,
            substitute_op_src(*op_src2, new_variable, variable_mappings),
            *check,
        ),
        StatCode::Load(var, op_src, size) => StatCode::Load(
            get_mapping(*var, new_variable, variable_mappings),
            substitute_op_src(*op_src, new_variable, variable_mappings),
            *size,
        ),
        StatCode::Store(op_src1, op_src2, size) => StatCode::Store(
            substitute_op_src(*op_src1, new_variable, variable_mappings),
            get_mapping(*op_src2, new_variable, variable_mappings),
            *size,
        ),
        StatCode::Call(var, fname, args) => StatCode::Call(
            get_mapping(*var, new_variable, variable_mappings),
            fname.clone(),
            args.iter()
                .map(|op_src| get_mapping(*op_src, new_variable, variable_mappings))
                .collect(),
        ),
        StatCode::VoidCall(fname, args) => StatCode::VoidCall(
            fname.clone(),
            args.iter()
                .map(|op_src| get_mapping(*op_src, new_variable, variable_mappings))
                .collect(),
        ),
    }
}

/// Returns the name of the function being called in a node, provided one exists.
fn get_call_name(node: &StatNode, functions: &HashMap<String, Function>) -> Option<String> {
    if let StatType::Simple(_, StatCode::VoidCall(fname, _) | StatCode::Call(_, fname, _), _) =
        &*node.get() && functions.contains_key(fname.as_str())
    {
        Some(fname.clone())
    } else {
        None
    }
}

#[allow(clippy::too_many_arguments)]
/// Copies a function code graph so that it gets connected into the inlined spot.
fn inline_code_dfs(
    new_call_nodes: &mut LinkedList<StatNode>,
    node: &StatNode,
    mappings: &mut HashMap<StatNode, StatNode>,
    new_graph: &mut Graph<StatType>,
    call_successor: StatNode,
    assigned_var: Option<VarRepr>,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
    functions: &HashMap<String, Function>,
    sccs_group: Arc<HashSet<&str>>,
) -> StatNode {
    if mappings.contains_key(node) {
        return mappings[node].clone();
    };
    match &*node.get() {
        StatType::Simple(_, stat_code, next_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            if let Some(fname) = get_call_name(node, functions) && !sccs_group.contains(fname.as_str()) {
                new_call_nodes.push_back(new_node.clone());
            }
            let next_node = inline_code_dfs(
                new_call_nodes,
                next_node,
                mappings,
                new_graph,
                call_successor,
                assigned_var,
                new_variable,
                variable_mappings,
                functions,
                sccs_group,
            );
            next_node.get_mut().add_incoming(new_node.clone());
            let incoming = (&*new_node.get()).incoming().into_iter().cloned().collect();
            new_node.set(StatType::Simple(
                incoming,
                substitute_vars(stat_code, new_variable, variable_mappings),
                next_node,
            ));
            new_node
        }
        StatType::Branch(_, op_src, true_node, false_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            let true_node = inline_code_dfs(
                new_call_nodes,
                true_node,
                mappings,
                new_graph,
                call_successor.clone(),
                assigned_var,
                new_variable,
                variable_mappings,
                functions,
                sccs_group.clone(),
            );
            let false_node = inline_code_dfs(
                new_call_nodes,
                false_node,
                mappings,
                new_graph,
                call_successor,
                assigned_var,
                new_variable,
                variable_mappings,
                functions,
                sccs_group,
            );
            true_node.get_mut().add_incoming(new_node.clone());
            false_node.get_mut().add_incoming(new_node.clone());
            let incoming = (&*new_node.get()).incoming().into_iter().cloned().collect();
            new_node.set(StatType::Branch(
                incoming,
                substitute_op_src(*op_src, new_variable, variable_mappings),
                true_node,
                false_node,
            ));
            new_node
        }
        StatType::Loop(_) => {
            let new_node = new_graph.new_node(StatType::new_loop());
            mappings.insert(node.clone(), new_node.clone());
            new_node
        }
        StatType::Return(_, op_src) => {
            if let (Some(assigned_var), Some(op_src)) = (assigned_var, op_src) {
                let new_node = new_graph.new_node(StatType::new_simple(
                    StatCode::Assign(
                        assigned_var,
                        substitute_op_src(*op_src, new_variable, variable_mappings),
                    ),
                    call_successor.clone(),
                ));
                mappings.insert(node.clone(), new_node.clone());
                call_successor.get_mut().add_incoming(new_node.clone());
                new_node
            } else {
                call_successor
            }
        }
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

#[allow(clippy::too_many_arguments)]
/// Simply copis a code graph. Returns the root of the new graph and the heuristic for the
/// size of the copied graph.
fn copy_code_dfs(
    new_call_nodes: &mut LinkedList<StatNode>,
    node: &StatNode,
    mappings: &mut HashMap<StatNode, StatNode>,
    new_graph: &mut Graph<StatType>,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
    functions: &HashMap<String, Function>,
    sccs_group: Arc<HashSet<&str>>,
) -> (StatNode, f64) {
    if mappings.contains_key(node) {
        return (mappings[node].clone(), 0.0);
    };
    let node_instruction_heuristic = get_stat_size_heuristic(&*node.get());
    match &*node.get() {
        StatType::Simple(_, stat_code, next_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            if let Some(fname) = get_call_name(node, functions) && !sccs_group.contains(fname.as_str()) {
                new_call_nodes.push_back(new_node.clone());
            }
            let (next_node, instruction_sum) = copy_code_dfs(
                new_call_nodes,
                next_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
                functions,
                sccs_group,
            );
            next_node.get_mut().add_incoming(new_node.clone());
            let incoming = (&*new_node.get()).incoming().into_iter().cloned().collect();
            new_node.set(StatType::Simple(
                incoming,
                substitute_vars(stat_code, new_variable, variable_mappings),
                next_node,
            ));
            (new_node, instruction_sum + node_instruction_heuristic)
        }
        StatType::Branch(_, op_src, true_node, false_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            let (true_node, true_instruction_sum) = copy_code_dfs(
                new_call_nodes,
                true_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
                functions,
                sccs_group.clone(),
            );
            let (false_node, false_instruction_sum) = copy_code_dfs(
                new_call_nodes,
                false_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
                functions,
                sccs_group,
            );
            true_node.get_mut().add_incoming(new_node.clone());
            false_node.get_mut().add_incoming(new_node.clone());
            let incoming = (&*new_node.get()).incoming().into_iter().cloned().collect();
            new_node.set(StatType::Branch(
                incoming,
                substitute_op_src(*op_src, new_variable, variable_mappings),
                true_node,
                false_node,
            ));
            (
                new_node,
                true_instruction_sum + false_instruction_sum + node_instruction_heuristic,
            )
        }
        StatType::Loop(_) => {
            let new_node = new_graph.new_node(StatType::new_loop());
            mappings.insert(node.clone(), new_node.clone());
            (new_node, node_instruction_heuristic)
        }
        StatType::Return(_, op_src) => {
            let new_node = new_graph.new_node(StatType::new_return(
                op_src.map(|op_src| substitute_op_src(op_src, new_variable, variable_mappings)),
            ));
            mappings.insert(node.clone(), new_node.clone());
            (new_node, node_instruction_heuristic)
        }
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

#[allow(clippy::too_many_arguments)]
/// Performs a general breadth-first-search inlining optimization of a code graph.
/// Returns the new code graph and the new argument identifiers after the remapping of
/// internal variables.
fn inline_graph(
    code: &StatNode,
    new_graph: &mut Graph<StatType>,
    instructions_limit: usize,
    args: &[VarRepr],
    functions: &HashMap<String, Function>,
    function_instruction_counts: &HashMap<&str, usize>,
    sccs_group: Arc<HashSet<&str>>,
    sccs_groups: &HashMap<&str, Arc<HashSet<&str>>>,
) -> (StatNode, Vec<VarRepr>, bool) {
    let mut call_nodes = LinkedList::new();
    let mut main_variable_mappings = HashMap::new();
    let mut new_variable = 0;
    let (mut new_code, code_instruction_count) = copy_code_dfs(
        &mut call_nodes,
        code,
        &mut HashMap::new(),
        new_graph,
        &mut new_variable,
        &mut main_variable_mappings,
        functions,
        sccs_group,
    );
    let args = args
        .iter()
        .map(|&arg| get_mapping(arg, &mut new_variable, &mut main_variable_mappings))
        .collect();
    let mut code_instruction_count = code_instruction_count.ceil() as usize;
    let mut read_ref_needed = false;
    while let Some(call_node) = call_nodes.pop_front() {
        let fname = get_call_name(&call_node, functions).expect("Not a call node");
        let function_instruction_count = function_instruction_counts[fname.as_str()];
        if code_instruction_count + function_instruction_count > instructions_limit {
            continue;
        }
        code_instruction_count += function_instruction_count;
        let (incoming, assigned_var, call_successor, call_args, fname) = match call_node
            .set(StatType::deleted())
        {
            StatType::Simple(
                incoming,
                StatCode::Call(assigned_var, fname, call_args),
                call_successor,
            ) => (
                incoming,
                Some(assigned_var),
                call_successor,
                call_args,
                fname,
            ),
            StatType::Simple(incoming, StatCode::VoidCall(fname, call_args), call_successor) => {
                (incoming, None, call_successor, call_args, fname)
            }
            _ => panic!("Not a call node"),
        };
        call_successor.get_mut().set_incoming(vec![]);
        let Function {
            args: function_args,
            code: function_code,
            graph: _,
            read_ref,
        } = &functions[&fname];
        let mut variable_mappings = HashMap::new();
        let mut new_call_node = inline_code_dfs(
            &mut call_nodes,
            function_code,
            &mut HashMap::new(),
            new_graph,
            call_successor.clone(),
            assigned_var,
            &mut new_variable,
            &mut variable_mappings,
            functions,
            sccs_groups[fname.as_str()].clone(),
        );
        for (call_arg, function_arg) in zip(call_args, function_args) {
            let new_function_arg =
                if let Some(&new_function_arg) = variable_mappings.get(function_arg) {
                    new_function_arg
                } else {
                    continue;
                };
            let arg_assign_node = new_graph.new_node(StatType::new_simple(
                StatCode::Assign(new_function_arg, OpSrc::Var(call_arg)),
                new_call_node.clone(),
            ));
            new_call_node
                .get_mut()
                .add_incoming(arg_assign_node.clone());
            new_call_node = arg_assign_node;
        }
        for incoming_node in incoming {
            incoming_node
                .get_mut()
                .substitute_child(&call_node, &new_call_node);
            new_call_node.get_mut().add_incoming(incoming_node);
        }
        if new_code == call_node {
            new_code = new_call_node;
        }
        new_graph.remove_node(call_node);
        read_ref_needed |= read_ref;
    }
    (new_code, args, read_ref_needed)
}

/// Performs the inlining optimization. Converts the old three code graph
/// into a new one, possibly with substituted variable names. The arguments are:
///  - The three code to perform the optimization on.
///  - The instruction limit to which the code size heuristic is compared to.
pub(super) fn inline(
    ThreeCode {
        functions,
        data_refs,
        graph: _,
        read_ref,
        code,
        int_handler,
    }: ThreeCode,
    instructions_limit: usize,
) -> ThreeCode {
    let function_statistics = functions
        .iter()
        .map(|(fname, function)| (fname.as_ref(), get_function_statistics(function)))
        .collect::<HashMap<&str, _>>();

    let function_instruction_counts = function_statistics
        .iter()
        .map(|(&fname, &(heuristic_instruction_count, _))| {
            (fname, heuristic_instruction_count.ceil() as usize)
        })
        .collect();

    let call_graph = function_statistics
        .iter()
        .map(|(&fname, (_, calls))| {
            (
                fname,
                calls
                    .iter()
                    .map(String::as_str)
                    .filter(|&call_fname| functions.contains_key(call_fname))
                    .collect(),
            )
        })
        .collect::<HashMap<&str, Vec<&str>>>();

    let mut transpose_call_graph: HashMap<_, Vec<_>> = functions
        .keys()
        .map(|fname| (fname.as_str(), Vec::new()))
        .collect();
    for (&fname, calls) in &call_graph {
        for &call in calls {
            transpose_call_graph.get_mut(call).unwrap().push(fname);
        }
    }

    let sccs = get_function_sccs(&call_graph, &transpose_call_graph);

    let mut new_graph = Graph::new();
    let (code, _, read_ref_needed) = inline_graph(
        &code,
        &mut new_graph,
        instructions_limit,
        &[],
        &functions,
        &function_instruction_counts,
        Arc::new(HashSet::new()),
        &sccs,
    );

    let new_functions = functions
        .par_iter()
        .map(
            |(
                fname,
                Function {
                    args,
                    code,
                    graph: _,
                    read_ref,
                },
            )| {
                let mut new_function_graph = Graph::new();
                let (code, args, read_ref_needed) = inline_graph(
                    code,
                    &mut new_function_graph,
                    instructions_limit,
                    args,
                    &functions,
                    &function_instruction_counts,
                    sccs[fname.as_str()].clone(),
                    &sccs,
                );
                (
                    fname.clone(),
                    Function {
                        args,
                        code,
                        graph: new_function_graph,
                        read_ref: *read_ref | read_ref_needed,
                    },
                )
            },
        )
        .collect();

    let mut called_functions = HashSet::new();
    if let Some(int_handler) = &int_handler {
        called_functions.insert(int_handler.clone());
    }

    for node in &new_graph {
        if let Some(fname) = get_call_name(&node, &new_functions) {
            called_functions.insert(fname);
        }
    }

    for Function {
        graph: new_function_graph,
        args: _,
        code: _,
        read_ref: _,
    } in new_functions.values()
    {
        for node in new_function_graph {
            if let Some(fname) = get_call_name(&node, &new_functions) {
                called_functions.insert(fname);
            }
        }
    }

    let new_functions: HashMap<_, _> = new_functions
        .into_iter()
        .filter(|(fname, _)| called_functions.contains(fname))
        .collect();

    ThreeCode {
        functions: new_functions,
        data_refs,
        graph: new_graph,
        read_ref: read_ref | read_ref_needed,
        code,
        int_handler,
    }
}
