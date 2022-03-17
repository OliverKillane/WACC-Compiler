//! Constant propagation on the [threecode](ThreeCode) control flow graph.
//!
//! Constants can include integers and data references (with offsets), and can
//! be propagated through any control flow structure (graph).

use super::data_flow::dataflow_analysis;
use super::three_code::*;
use crate::graph::{Deleted, Graph};
use crate::intermediate::VarRepr;
use rayon::prelude::*;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet, LinkedList};
use std::rc::Rc;

/// Returns the variable defined in this node, if one exists.
fn get_defined_var(node: &StatNode) -> Option<VarRepr> {
    match &*node.get() {
        StatType::Simple(
            _,
            StatCode::Assign(var, _)
            | StatCode::AssignOp(var, _, _, _, _)
            | StatCode::Load(var, _, _)
            | StatCode::Call(var, _, _),
            _,
        ) => Some(*var),
        _ => None,
    }
}

/// Retreives the variable used in the [op source](OpSrc), if one exists.
fn get_use_op_src(op_src: &OpSrc) -> Option<VarRepr> {
    if let &OpSrc::Var(var) = op_src {
        Some(var)
    } else {
        None
    }
}

/// Liveness analysis set type.
type LiveAnalysis = Rc<HashSet<VarRepr>>;

/// Retreives the used variables that can be substituted for a constant [op source](OpSrc).
fn get_const_prop_uses(node: &StatNode) -> HashSet<VarRepr> {
    match &*node.get() {
        StatType::Simple(
            _,
            StatCode::Assign(_, op_src)
            | StatCode::Load(_, op_src, _)
            | StatCode::Store(op_src, _, _),
            _,
        )
        | StatType::Branch(_, op_src, _, _) => get_use_op_src(op_src)
            .map(|var| HashSet::from([var]))
            .unwrap_or_default(),
        StatType::Simple(_, StatCode::AssignOp(_, op_src1, _, op_src2, _), _) => {
            vec![get_use_op_src(op_src1), get_use_op_src(op_src2)]
                .into_iter()
                .flatten()
                .collect()
        }
        StatType::Return(_, op_src) => op_src
            .and_then(|op_src| get_use_op_src(&op_src).map(|var| HashSet::from([var])))
            .unwrap_or_default(),
        _ => HashSet::new(),
    }
}

/// Constructs the LiveIn set from the LiveOut set for the dataflow analysis.
fn construct_live_in(node: &StatNode, live_out: LiveAnalysis) -> LiveAnalysis {
    let mut cow_live_out = Cow::Borrowed(&*live_out);
    if let Some(kill) = get_defined_var(node) && cow_live_out.contains(&kill) {
        cow_live_out.to_mut().remove(&kill);
    }
    for var in get_const_prop_uses(node) {
        if !cow_live_out.contains(&var) {
            cow_live_out.to_mut().insert(var);
        }
    }
    if cow_live_out.is_borrowed() {
        live_out
    } else {
        Rc::new(cow_live_out.into_owned())
    }
}

/// Constructs the initial values for LiveIn and LiveOut for the node.
fn live_init(node: &StatNode) -> (LiveAnalysis, LiveAnalysis) {
    let live_out = Rc::new(HashSet::new());
    (construct_live_in(node, live_out.clone()), live_out)
}

/// Constructs the new values for LiveIn and LiveOut, after an iteration of
/// the dataflow analysis.
fn live_update(
    _: Vec<&LiveAnalysis>,
    live_in: &LiveAnalysis,
    node: &StatNode,
    live_out: &LiveAnalysis,
    succ_live_in: Vec<&LiveAnalysis>,
) -> (LiveAnalysis, LiveAnalysis, bool) {
    let (new_live_in, new_live_out) = if succ_live_in.len() == 1 {
        if succ_live_in[0] == live_out {
            return (live_in.clone(), live_out.clone(), false);
        } else {
            let new_live_out = succ_live_in[0].clone();
            (construct_live_in(node, new_live_out.clone()), new_live_out)
        }
    } else {
        let new_live_out = Rc::new(succ_live_in.into_iter().fold(
            HashSet::new(),
            |mut live_out, live_in| {
                for live_in_var in live_in.iter() {
                    live_out.insert(*live_in_var);
                }
                live_out
            },
        ));
        (construct_live_in(node, new_live_out.clone()), new_live_out)
    };
    let updated = live_in != &new_live_in || live_out != &new_live_out;
    (new_live_in, new_live_out, updated)
}

/// Definitions analysis set type.
type DefsAnalysis = HashMap<VarRepr, Rc<HashSet<StatNode>>>;

/// Constructs the DefOut map from the DefIn map, provided a node is actually
/// needed further in the graph (based on the LiveIn set).
fn construct_defs_out(
    node: &StatNode,
    defs_in: &DefsAnalysis,
    live_out: &LiveAnalysis,
) -> DefsAnalysis {
    let mut defs_in = defs_in
        .iter()
        .filter_map(|(var, defs)| {
            if live_out.contains(var) {
                Some((*var, defs.clone()))
            } else {
                None
            }
        })
        .collect();
    let def_var = if let Some(def_var) = get_defined_var(node) && live_out.contains(&def_var) {
        def_var
    } else {
        return defs_in;
    };
    let def_in = defs_in
        .get(&def_var)
        .cloned()
        .unwrap_or_else(|| Rc::new(HashSet::new()));
    defs_in.insert(
        def_var,
        if !def_in.contains(node) {
            let mut def_in = (&*def_in).clone();
            def_in.insert(node.clone());
            Rc::new(def_in)
        } else {
            def_in
        },
    );
    defs_in
}

/// Constructs the initial DefIn and DefOut sets for the node based on the node itself,
/// its LiveOut set and some additional definitions that we might want to add on
/// top of those defined by the node (for example arguments to a function if the
/// node is the root of the code).
fn defs_init(
    node: &StatNode,
    live_out: &LiveAnalysis,
    additional_defs: &[(VarRepr, StatNode)],
) -> (DefsAnalysis, DefsAnalysis) {
    let defs_in: DefsAnalysis = additional_defs
        .iter()
        .map(|(var, def)| (*var, Rc::new(HashSet::from([def.clone()]))))
        .collect();
    let defs_out = construct_defs_out(node, &defs_in, live_out);
    (defs_in, defs_out)
}

/// Constructs the new values for DefIn and DefOut, after an iteration of the
/// dataflow analysis.
fn defs_update(
    pred_defs_out: Vec<&DefsAnalysis>,
    defs_in: &DefsAnalysis,
    node: &StatNode,
    defs_out: &DefsAnalysis,
    live_out: &LiveAnalysis,
    additional_defs: &[(VarRepr, StatNode)],
) -> (DefsAnalysis, DefsAnalysis, bool) {
    let (new_defs_in, new_defs_out) = if pred_defs_out.len() == 1 {
        if pred_defs_out[0] == defs_in {
            return (defs_in.clone(), defs_out.clone(), false);
        } else {
            let new_defs_in = pred_defs_out[0].clone();
            let new_defs_out = construct_defs_out(node, &new_defs_in, live_out);
            (new_defs_in, new_defs_out)
        }
    } else {
        let new_defs_in = pred_defs_out.into_iter().fold(
            additional_defs
                .iter()
                .map(|(var, def)| (*var, Rc::new(HashSet::from([def.clone()]))))
                .collect(),
            |mut defs_in: DefsAnalysis, defs_out| {
                for (&var, defs) in defs_out {
                    let def_in = defs_in.entry(var).or_default();
                    let new_def_in = def_in.union(&**defs).cloned().collect();
                    *def_in = Rc::new(new_def_in);
                }
                defs_in
            },
        );
        let new_defs_out = construct_defs_out(node, &new_defs_in, live_out);
        (new_defs_in, new_defs_out)
    };
    let updated = defs_in != &new_defs_in || defs_out != &new_defs_out;
    (new_defs_in, new_defs_out, updated)
}

/// Substitutes all uses of a variable in a node with a constant [op source](OpSrc).
/// The op source must not be a variable.
fn substitute_const_use(node: &StatNode, var: VarRepr, const_op: OpSrc) {
    if let OpSrc::Var(_) = const_op {
        panic!("Op source should be a constant");
    }
    match &mut*node.get_mut() {
        StatType::Simple(_, StatCode::Assign(_, op_src) | StatCode::Load(_, op_src, _) | StatCode::Store(op_src, _, _), _)
        | StatType::Branch(_, op_src, _, _) => {
            if let OpSrc::Var(op_src_var) = *op_src && op_src_var == var {
                *op_src = const_op;
            }
        }
        StatType::Simple(_, StatCode::AssignOp(_, op_src1, _, op_src2, _), _) => {
            if let OpSrc::Var(op_src_var) = *op_src1 && op_src_var == var {
                *op_src1 = const_op;
            }
            if let OpSrc::Var(op_src_var) = *op_src2 && op_src_var == var {
                *op_src2 = const_op;
            }
        }
        StatType::Return(_, op_src) => {
            if let Some(OpSrc::Var(op_src_var)) = *op_src && op_src_var == var {
                *op_src = Some(const_op);
            }
        }
        _ => {}
    }
}

/// The result of a constant evaluation attempt.
enum PropagateOpResult {
    /// Constant evaluation failed
    Failed,
    /// Result overflows
    Oob,
    /// Evaluated to a constant [op source](OpSrc)
    Const(OpSrc),
}

/// Attempts a constant evaluation.
fn propagate_op(op_src1: OpSrc, bin_op: BinOp, op_src2: OpSrc, checked: bool) -> PropagateOpResult {
    use PropagateOpResult::*;
    match (op_src1, bin_op, op_src2) {
        (OpSrc::Const(c1), BinOp::Add | BinOp::Sub | BinOp::Mul, OpSrc::Const(c2)) => {
            if checked {
                match bin_op {
                    BinOp::Add => c1.checked_add(c2),
                    BinOp::Sub => c1.checked_sub(c2),
                    BinOp::Mul => c1.checked_mul(c2),
                    _ => panic!("Unexpected binary operation"),
                }
                .map(|val| Const(OpSrc::Const(val)))
                .unwrap_or(Oob)
            } else {
                Const(OpSrc::Const(match bin_op {
                    BinOp::Add => c1.wrapping_add(c2),
                    BinOp::Sub => c1.wrapping_sub(c2),
                    BinOp::Mul => c1.wrapping_mul(c2),
                    _ => panic!("Unexpected binary operation"),
                }))
            }
        }
        (OpSrc::Const(_), BinOp::Div | BinOp::Mod, OpSrc::Const(0)) => Failed,
        (OpSrc::Const(c1), BinOp::Div | BinOp::Mod, OpSrc::Const(c2)) => {
            Const(OpSrc::Const(match bin_op {
                BinOp::Div => c1 / c2,
                BinOp::Mod => c1 % c2,
                _ => panic!("Unexpected binary operation"),
            }))
        }
        (OpSrc::DataRef(data_ref, o), BinOp::Add | BinOp::Sub, OpSrc::Const(c))
        | (OpSrc::Const(c), BinOp::Add | BinOp::Sub, OpSrc::DataRef(data_ref, o)) => {
            Const(OpSrc::DataRef(
                data_ref,
                match bin_op {
                    BinOp::Add => o.wrapping_add(c),
                    BinOp::Sub => o.wrapping_sub(c),
                    _ => panic!("Unexpected binary operation"),
                },
            ))
        }
        _ => Failed,
    }
}

/// Attempts a constant evaluation on a node.
fn prop_const_node(node: &StatNode, int_handler: &Option<String>) -> Option<VarRepr> {
    let (assigned_var, op_src1, bin_op, op_src2, checked) = if let StatType::Simple(
        _,
        StatCode::AssignOp(assigned_var, op_src1, bin_op, op_src2, checked),
        _,
    ) = *node.get()
    {
        (assigned_var, op_src1, bin_op, op_src2, checked)
    } else {
        return None;
    };
    let propagate_op_result =
        propagate_op(op_src1, bin_op, op_src2, checked || int_handler.is_some());
    let new_stat_code = match propagate_op_result {
        PropagateOpResult::Const(op_src) => StatCode::Assign(assigned_var, op_src),
        PropagateOpResult::Oob => StatCode::VoidCall(
            int_handler.as_ref().expect("No int handler").clone(),
            vec![],
        ),
        _ => return None,
    };
    if let StatType::Simple(_, stat_code, _) = &mut *node.get_mut() {
        *stat_code = new_stat_code;
    } else {
        panic!("Expected a simple node")
    };
    if let StatType::Simple(_, StatCode::Assign(assigned_var, _), _) = &*node.get() {
        Some(*assigned_var)
    } else {
        None
    }
}

/// Performs a constant evaluation on a code starting in the given node, with
/// given argument variables and a given integer overflow handler.
fn prop_const_graph(code: &StatNode, args: &[VarRepr], int_handler: &Option<String>) {
    let mut fake_defs_graph = Graph::new();
    let args_nodes = args
        .iter()
        .map(|var| (*var, fake_defs_graph.new_node(StatType::deleted())))
        .collect::<Vec<_>>();
    let live_vars = dataflow_analysis(code, live_init, live_update, false);
    let live_defs = dataflow_analysis(
        code,
        |node| {
            defs_init(
                node,
                &live_vars[node].1,
                if code == node { &args_nodes } else { &[] },
            )
        },
        |pred_live_out, def_in, node, def_out, _| {
            defs_update(
                pred_live_out,
                def_in,
                node,
                def_out,
                &live_vars[node].1,
                if code == node { &args_nodes } else { &[] },
            )
        },
        true,
    );
    let mut defs_uses: HashMap<_, HashSet<_>> = HashMap::new();
    for (node, (def_map, _)) in &live_defs {
        let uses = get_const_prop_uses(node)
            .into_iter()
            .collect::<HashSet<_>>();
        for (var, defs) in def_map {
            if uses.contains(var) {
                for def in &**defs {
                    defs_uses.entry(def).or_default().insert(node.clone());
                }
            }
        }
    }
    let mut non_const_defs: HashMap<_, _> = live_defs
        .iter()
        .map(|(node, (def_map, _))| {
            let uses = get_const_prop_uses(node)
                .into_iter()
                .collect::<HashSet<_>>();
            (
                node.clone(),
                def_map
                    .iter()
                    .filter_map(|(var, defs)| {
                        if uses.contains(var) {
                            Some((
                                *var,
                                defs.iter()
                                    .filter(|def| {
                                        if let StatType::Simple(_, StatCode::Assign(_, op_src), _) =
                                            &*def.get()
                                        {
                                            matches!(op_src, OpSrc::Var(_))
                                        } else {
                                            true
                                        }
                                    })
                                    .count(),
                            ))
                        } else {
                            None
                        }
                    })
                    .collect::<HashMap<_, _>>(),
            )
        })
        .collect::<HashMap<_, _>>();
    for node in live_defs.keys() {
        let assigned_var = if let Some(assigned_var) = prop_const_node(node, int_handler) {
            assigned_var
        } else {
            continue;
        };
        for use_node in defs_uses.get(&node).unwrap_or(&HashSet::new()) {
            *non_const_defs
                .get_mut(use_node)
                .unwrap()
                .get_mut(&assigned_var)
                .unwrap() -= 1;
        }
    }
    let mut const_prop = LinkedList::new();
    for (node, non_const_uses_counts) in &non_const_defs {
        for (&used_var, &non_const_uses_count) in non_const_uses_counts {
            if non_const_uses_count == 0 {
                const_prop.push_back((node.clone(), used_var));
            }
        }
    }
    while let Some((node, var)) = const_prop.pop_front() {
        let mut substitute_op_src = None;
        for def in &*(&live_defs[&node].0)[&var] {
            if let StatType::Simple(_, StatCode::Assign(_, op_src), _) = &*def.get() {
                if substitute_op_src == None || substitute_op_src == Some(*op_src) {
                    substitute_op_src = Some(*op_src)
                } else {
                    substitute_op_src = None;
                    break;
                }
            } else {
                panic!("Not a constant definition")
            }
        }
        let substitute_op_src = if let Some(substitute_op_src) = substitute_op_src {
            substitute_op_src
        } else {
            continue;
        };
        substitute_const_use(&node, var, substitute_op_src);
        non_const_defs.get_mut(&node).unwrap().remove(&var);
        if !non_const_defs[&node].is_empty() {
            continue;
        }
        let assigned_var = if let Some(assigned_var) = prop_const_node(&node, int_handler) {
            assigned_var
        } else {
            continue;
        };
        for use_node in defs_uses.get(&node).unwrap_or(&HashSet::new()) {
            *non_const_defs
                .get_mut(use_node)
                .unwrap()
                .get_mut(&assigned_var)
                .unwrap() -= 1;
            if non_const_defs[use_node][&assigned_var] == 0 {
                const_prop.push_back((use_node.clone(), assigned_var));
            }
        }
    }
}

/// Performs constant propagation on the [three code](ThreeCode).
pub(super) fn prop_consts(mut threecode: ThreeCode) -> ThreeCode {
    prop_const_graph(&threecode.code, &[], &threecode.int_handler);

    threecode.functions = threecode
        .functions
        .into_par_iter()
        .map(|(name, fun)| {
            prop_const_graph(&fun.code, &fun.args, &threecode.int_handler);
            (name, fun)
        })
        .collect();

    threecode
}
