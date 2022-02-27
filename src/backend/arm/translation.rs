//! Translates the three code representation of a graph into an arm
//! representation, with temporaries instead of registers.

use super::{
    super::{
        super::graph::Graph,
        super::intermediate::DataRef,
        three_code::{Function, StatNode, ThreeCode},
    },
    arm_repr::{ArmNode, ControlFlow, Data, DataKind, Program, Subroutine, Temporary},
};
use std::collections::HashMap;

pub(super) fn translate_threecode(
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
    }: ThreeCode,
) -> Program<Temporary> {
    todo!()
}

fn translate_data(data_refs: HashMap<DataRef, Vec<u8>>) -> Vec<Data> {
    data_refs
        .into_iter()
        .map(|(data_ref, content)| {
            Data(
                data_ref,
                DataKind::Ascii(
                    String::from_utf8(content).expect("String must be composed of utf8"),
                ),
            )
        })
        .collect::<Vec<_>>()
}

fn translate_from_node(
    node: StatNode,
    graph: &mut Graph<ControlFlow<Temporary>>,
) -> ArmNode<Temporary> {
    todo!()
}

fn translate_function(
    Function {
        args,
        code,
        read_ref,
    }: Function,
    graph: &mut Graph<ControlFlow<Temporary>>,
) -> Subroutine<Temporary> {
    todo!()
}
