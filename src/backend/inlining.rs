use std::{
    collections::{HashMap, HashSet, LinkedList},
    intrinsics::ceilf64,
    iter::zip,
    rc::Rc,
};

use crate::{
    graph::{Deleted, Graph},
    intermediate::VarRepr,
};

use super::three_code::*;

fn get_stat_size_heuristic(stat: &StatType) -> f64 {
    match stat {
        StatType::Return(_, _) => 4.0,
        StatType::Loop(_) => 1.0,
        StatType::Branch(_, _, _, _) => 1.2,
        StatType::Simple(_, _, _) => 2.7,
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

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
    return total_sum;
}

fn get_function_statistics(
    Function {
        args: _,
        code,
        read_ref: _,
    }: &Function,
) -> (f64, Vec<String>) {
    let code = if let Some(code) = code {
        code
    } else {
        return (0.0, vec![]);
    };
    let mut calls = Vec::new();
    let total_sum = get_function_statistics_dfs(&mut calls, code, &mut HashSet::new());
    (total_sum, calls)
}

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
        get_function_sccs_dfs(post_order, fname, visited, call_graph);
    }
    post_order.push(fname);
}

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
        get_function_sccs_scc(group, fname, visited, transpose_call_graph);
    }
}

fn get_function_sccs<'l>(
    call_graph: &HashMap<&'l str, Vec<&'l str>>,
    transpose_call_graph: &HashMap<&'l str, Vec<&'l str>>,
) -> HashMap<&'l str, Rc<HashSet<&'l str>>> {
    let mut visited = HashSet::new();
    let mut post_order = Vec::new();
    for &fname in call_graph.keys() {
        get_function_sccs_dfs(&mut post_order, fname, &mut visited, call_graph);
    }
    let mut sccs = HashMap::new();
    let mut visited = HashSet::new();
    for &fname in call_graph.keys() {
        let mut group = HashSet::new();
        get_function_sccs_scc(&mut group, fname, &mut visited, transpose_call_graph);
        let group = Rc::new(group);
        for &group_fname in group.iter() {
            sccs.insert(group_fname, group.clone());
        }
    }
    sccs
}

fn get_mapping(
    var: VarRepr,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> VarRepr {
    let mapping = *variable_mappings.entry(var).or_insert(*new_variable);
    *new_variable += 1;
    mapping
}

fn substitute_op_src(
    op_src: OpSrc,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> OpSrc {
    if let OpSrc::Var(var) = op_src {
        let variable_mappings_size = variable_mappings.len();
        OpSrc::Var(get_mapping(var, new_variable, variable_mappings))
    } else {
        op_src
    }
}

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
        StatCode::AssignOp(var, op_src1, bin_op, op_src2) => StatCode::AssignOp(
            get_mapping(*var, new_variable, variable_mappings),
            substitute_op_src(*op_src1, new_variable, variable_mappings),
            *bin_op,
            substitute_op_src(*op_src2, new_variable, variable_mappings),
        ),
        StatCode::Load(var1, var2, size) => StatCode::Load(
            get_mapping(*var1, new_variable, variable_mappings),
            get_mapping(*var2, new_variable, variable_mappings),
            *size,
        ),
        StatCode::Store(var1, var2, size) => StatCode::Store(
            get_mapping(*var1, new_variable, variable_mappings),
            get_mapping(*var2, new_variable, variable_mappings),
            *size,
        ),
        StatCode::Call(var, fname, args) => StatCode::Call(
            get_mapping(*var, new_variable, variable_mappings),
            fname.clone(),
            args.iter()
                .map(|var| get_mapping(*var, new_variable, variable_mappings))
                .collect(),
        ),
        StatCode::VoidCall(fname, args) => StatCode::VoidCall(
            fname.clone(),
            args.iter()
                .map(|var| get_mapping(*var, new_variable, variable_mappings))
                .collect(),
        ),
    }
}

fn get_call_name(node: &StatNode) -> Option<&str> {
    if let StatType::Simple(_, StatCode::VoidCall(fname, _) | StatCode::Call(_, fname, _), _) =
        &*node.get()
    {
        Some(fname)
    } else {
        None
    }
}

fn inline_code_dfs(
    new_call_nodes: &mut LinkedList<StatNode>,
    node: &StatNode,
    mappings: &mut HashMap<StatNode, StatNode>,
    new_graph: &mut Graph<StatType>,
    call_successor: StatNode,
    assigned_var: Option<VarRepr>,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> StatNode {
    if mappings.contains_key(node) {
        return mappings[node].clone();
    };
    match &*node.get() {
        StatType::Simple(_, stat_code, next_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            if get_call_name(node).is_some() {
                new_call_nodes.push_back(new_node);
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
            );
            next_node.get().add_incoming(new_node.clone());
            let incoming = new_node.get().incoming();
            new_node.set(StatType::Simple(
                incoming,
                substitute_vars(stat_code, new_variable, variable_mappings),
                next_node,
            ));
            new_node
        }
        StatType::Branch(_, var, true_node, false_node) => {
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
            );
            true_node.get().add_incoming(new_node.clone());
            false_node.get().add_incoming(new_node.clone());
            let incoming = new_node.get().incoming();
            new_node.set(StatType::Branch(
                incoming,
                get_mapping(*var, new_variable, variable_mappings),
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
        StatType::Return(_, var) => {
            if let (Some(assigned_var), Some(var)) = (assigned_var, var) {
                let new_node = new_graph.new_node(StatType::new_simple(
                    StatCode::Assign(
                        assigned_var,
                        OpSrc::Var(get_mapping(*var, new_variable, variable_mappings)),
                    ),
                    call_successor,
                ));
                mappings.insert(node.clone(), new_node.clone());
                call_successor.get().add_incoming(new_node.clone());
                new_node
            } else {
                call_successor
            }
        }
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

fn copy_code_dfs(
    new_call_nodes: &mut LinkedList<StatNode>,
    node: &StatNode,
    mappings: &mut HashMap<StatNode, StatNode>,
    new_graph: &mut Graph<StatType>,
    new_variable: &mut VarRepr,
    variable_mappings: &mut HashMap<VarRepr, VarRepr>,
) -> (StatNode, f64) {
    if mappings.contains_key(node) {
        return (mappings[node].clone(), 0.0);
    };
    let node_instruction_heuristic = get_stat_size_heuristic(&*node.get());
    match &*node.get() {
        StatType::Simple(_, stat_code, next_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            if get_call_name(node).is_some() {
                new_call_nodes.push_back(new_node);
            }
            let (next_node, instruction_sum) = copy_code_dfs(
                new_call_nodes,
                next_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
            );
            next_node.get().add_incoming(new_node.clone());
            let incoming = new_node.get().incoming();
            new_node.set(StatType::Simple(
                incoming,
                substitute_vars(stat_code, new_variable, variable_mappings),
                next_node,
            ));
            (new_node, instruction_sum + node_instruction_heuristic)
        }
        StatType::Branch(_, var, true_node, false_node) => {
            let new_node = new_graph.new_node(StatType::deleted());
            mappings.insert(node.clone(), new_node.clone());
            let (true_node, true_instruction_sum) = copy_code_dfs(
                new_call_nodes,
                true_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
            );
            let (false_node, false_instruction_sum) = copy_code_dfs(
                new_call_nodes,
                false_node,
                mappings,
                new_graph,
                new_variable,
                variable_mappings,
            );
            true_node.get().add_incoming(new_node.clone());
            false_node.get().add_incoming(new_node.clone());
            let incoming = new_node.get().incoming();
            new_node.set(StatType::Branch(
                incoming,
                get_mapping(*var, new_variable, variable_mappings),
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
        StatType::Return(_, var) => {
            let new_node = new_graph.new_node(StatType::new_return(
                var.map(|var| get_mapping(var, new_variable, variable_mappings)),
            ));
            mappings.insert(node.clone(), new_node.clone());
            (new_node, node_instruction_heuristic)
        }
        StatType::Dummy(_) => panic!("Dummy nodes not expected at this point"),
    }
}

fn inline_graph(
    code: StatNode,
    new_graph: &mut Graph<StatType>,
    instructions_limit: usize,
    args: &Vec<VarRepr>,
    functions: &HashMap<&str, Function>,
    function_instruction_counts: &HashMap<&str, usize>,
    sccs_group: &HashSet<&str>,
) -> (StatNode, Vec<VarRepr>) {
    let mut call_nodes = LinkedList::new();
    let mut main_variable_mappings = HashMap::new();
    let (new_code, code_instruction_count) = copy_code_dfs(
        &mut call_nodes,
        &code,
        &mut HashMap::new(),
        new_graph,
        &mut 0,
        &mut main_variable_mappings,
    );
    let args = args.iter().map(|arg| main_variable_mappings[arg]).collect();
    let code_instruction_count = code_instruction_count.ceil() as usize;
    while let Some(call_node) = call_nodes.pop_front() {
        let fname = get_call_name(&call_node).expect("Not a call node");
        let function_instruction_count = function_instruction_counts[fname];
        if sccs_group.contains(fname)
            || code_instruction_count + function_instruction_count > instructions_limit
        {
            continue;
        }
        code_instruction_count += function_instruction_count;
        let (incoming, assigned_var, call_successor, call_args) = match call_node
            .set(StatType::deleted())
        {
            StatType::Simple(
                incoming,
                StatCode::Call(assigned_var, fname, call_args),
                call_successor,
            ) => (incoming, Some(assigned_var), call_successor, call_args),
            StatType::Simple(incoming, StatCode::VoidCall(fname, call_args), call_successor) => {
                (incoming, None, call_successor, call_args)
            }
            _ => panic!("Not a call node"),
        };
        let Function {
            args: function_args,
            code: function_code,
            read_ref: _,
        } = &functions[fname];
        let new_call_node = function_code
            .as_ref()
            .map(|function_code| {
                let mut variable_mappings =
                    zip(call_args, function_args.iter().cloned()).collect::<HashMap<_, _>>();
                let mut new_variable = variable_mappings.keys().cloned().max().unwrap_or_default();
                inline_code_dfs(
                    &mut call_nodes,
                    function_code,
                    &mut HashMap::new(),
                    new_graph,
                    call_successor,
                    assigned_var,
                    &mut new_variable,
                    &mut variable_mappings,
                )
            })
            .unwrap_or(call_successor);
        call_node.set(new_call_node.set(StatType::deleted()));
        for call_successor in call_node.get().successors() {
            let successor_incoming = call_successor.get().incoming();
            call_successor.get_mut().set_incoming(
                successor_incoming
                    .into_iter()
                    .map(|successor_incoming_node| {
                        if successor_incoming_node == new_call_node {
                            call_node.clone()
                        } else {
                            successor_incoming_node
                        }
                    })
                    .collect(),
            );
        }
        new_graph.remove_node(new_call_node);
    }
    (new_code, args)
}

fn inline(
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
        int_handler,
    }: ThreeCode,
) -> ThreeCode {
    let code = if let Some(code) = code {
        code
    } else {
        return ThreeCode {
            functions: HashMap::new(),
            data_refs: HashMap::new(),
            graph: Graph::new(),
            read_ref: false,
            code: None,
            int_handler: None,
        };
    };
    let function_statistics = functions
        .iter()
        .map(|(fname, function)| (fname.as_ref(), get_function_statistics(function)))
        .collect::<HashMap<&str, _>>();

    let mut function_call_counts: HashMap<_, usize> = HashMap::new();
    for (_, (_, calls)) in &function_statistics {
        for call_fname in calls {
            *function_call_counts.entry(call_fname.as_str()).or_default() += 1;
        }
    }

    let call_graph = function_statistics
        .iter()
        .map(|(&fname, (_, calls))| (fname, calls.iter().map(String::as_str).collect()))
        .collect::<HashMap<&str, Vec<&str>>>();
    let mut transpose_call_graph: HashMap<_, Vec<_>> = HashMap::new();
    for (&fname, calls) in &call_graph {
        for &call in calls {
            transpose_call_graph.entry(call).or_default().push(fname);
        }
    }
    let sccs = get_function_sccs(&call_graph, &transpose_call_graph);

    ThreeCode {
        functions: todo!(),
        data_refs,
        graph: todo!(),
        read_ref,
        code: todo!(),
        int_handler,
    }
}
