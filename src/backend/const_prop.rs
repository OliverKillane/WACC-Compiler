use super::data_flow::dataflow_analysis;
use super::three_code::*;
use crate::graph::Graph;
use crate::intermediate::VarRepr;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type LiveAnalysis = Rc<HashSet<VarRepr>>;

fn get_defined_var(node: &StatNode) -> Option<VarRepr> {
    match &*node.get() {
        StatType::Simple(
            _,
            StatCode::Assign(var, _) | StatCode::AssignOp(var, _, _, _) | StatCode::Load(var, _, _),
            _,
        ) => Some(*var),
        _ => None,
    }
}

fn get_use_op_src(op_src: &OpSrc) -> Option<VarRepr> {
    if let &OpSrc::Var(var) = op_src {
        Some(var)
    } else {
        None
    }
}

fn get_uses(node: &StatNode) -> Vec<VarRepr> {
    match &*node.get() {
        StatType::Simple(_, StatCode::Assign(_, op_src), _) => get_use_op_src(op_src)
            .map(|var| vec![var])
            .unwrap_or_default(),
        StatType::Simple(_, StatCode::AssignOp(_, op_src1, _, op_src2), _) => {
            vec![get_use_op_src(op_src1), get_use_op_src(op_src2)]
                .into_iter()
                .filter_map(|var| var)
                .collect()
        }
        StatType::Simple(_, StatCode::Load(_, var, _), _) => vec![*var],
        StatType::Simple(_, StatCode::Store(var1, var2, _), _) => vec![*var1, *var2],
        StatType::Simple(_, StatCode::Call(_, _, args) | StatCode::VoidCall(_, args), _) => {
            args.clone()
        }
        StatType::Branch(_, var, _, _) => vec![*var],
        StatType::Return(_, var) => var.map(|var| vec![var]).unwrap_or_default(),
        _ => vec![],
    }
}

fn construct_live_in(node: &StatNode, live_out: LiveAnalysis) -> LiveAnalysis {
    let mut cow_live_out = Cow::Borrowed(&*live_out);
    if let Some(kill) = get_defined_var(node) && !cow_live_out.contains(&kill) {
        cow_live_out.to_mut().insert(kill);
    }
    for var in get_uses(node) {
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

fn live_init(node: &StatNode) -> (LiveAnalysis, LiveAnalysis) {
    let live_out = Rc::new(HashSet::new());
    (live_out.clone(), construct_live_in(node, live_out))
}

fn live_update(
    _: Vec<&LiveAnalysis>,
    live_in: LiveAnalysis,
    node: &StatNode,
    live_out: LiveAnalysis,
    succ_live_in: Vec<&LiveAnalysis>,
) -> (LiveAnalysis, LiveAnalysis, bool) {
    let new_live_out = if succ_live_in.len() == 1 {
        succ_live_in[0].clone()
    } else {
        Rc::new(
            succ_live_in
                .into_iter()
                .fold(HashSet::new(), |mut live_out, live_in| {
                    for live_in_var in live_in.iter() {
                        live_out.insert(*live_in_var);
                    }
                    live_out
                }),
        )
    };
    let new_live_in = construct_live_in(node, new_live_out.clone());
    let updated = live_in != new_live_in || live_out != new_live_out;
    (new_live_in, new_live_out, updated)
}

fn prop_const_graph(code: &StatNode, _: &[VarRepr], _: &mut Graph<StatType>) -> StatNode {
    println!(
        "{:?}",
        dataflow_analysis(code, live_init, live_update, false)
    );
    todo!()
}

fn prop_const(
    ThreeCode {
        mut functions,
        data_refs,
        mut graph,
        read_ref,
        code,
        int_handler,
    }: ThreeCode,
) -> ThreeCode {
    let code = prop_const_graph(&code, &vec![], &mut graph);
    for Function {
        args,
        code,
        read_ref: _,
    } in functions.values_mut()
    {
        let new_code = prop_const_graph(code, args, &mut graph);
        *code = new_code;
    }
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
        int_handler,
    }
}
