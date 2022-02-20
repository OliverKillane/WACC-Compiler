mod bool;
mod num;
mod ptr;

use super::{super::Options, BinOp, OpSrc, PtrSrc, Size, StatCode};
use crate::{
    backend::PropagationOpt,
    intermediate::{self as ir, VarRepr},
};
use std::{
    collections::HashMap,
    iter::{successors, zip},
};

impl From<i32> for OpSrc {
    fn from(num: i32) -> Self {
        OpSrc::Const(num)
    }
}

impl From<PtrSrc> for OpSrc {
    fn from(ptr_const: PtrSrc) -> Self {
        match ptr_const {
            PtrSrc::DataRef(data_ref, offset) => OpSrc::DataRef(data_ref, offset),
            PtrSrc::Null => OpSrc::Const(0),
        }
    }
}

impl From<ir::NumSize> for Size {
    fn from(size: ir::NumSize) -> Self {
        match size {
            ir::NumSize::DWord => Size::DWord,
            ir::NumSize::Word => Size::Word,
            ir::NumSize::Byte => Size::Byte,
        }
    }
}

impl From<ir::ArithOp> for BinOp {
    fn from(arith_op: ir::ArithOp) -> Self {
        match arith_op {
            ir::ArithOp::Add => BinOp::Add,
            ir::ArithOp::Sub => BinOp::Sub,
            ir::ArithOp::Mul => BinOp::Mul,
            ir::ArithOp::Div => BinOp::Div,
            ir::ArithOp::Mod => BinOp::Mod,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) struct ExprTranslationData<'l> {
    vars: &'l HashMap<VarRepr, ir::Type>,
    functions: &'l HashMap<String, ir::Function>,
    options: &'l Options,
}

impl<'l> ExprTranslationData<'l> {
    fn new(
        vars: &'l HashMap<VarRepr, ir::Type>,
        functions: &'l HashMap<String, ir::Function>,
        options: &'l Options,
    ) -> Self {
        ExprTranslationData {
            vars,
            functions,
            options,
        }
    }

    fn should_propagate(&self) -> bool {
        self.options.propagation != PropagationOpt::None
    }
}

fn translate_function_call(
    name: String,
    args: Vec<ir::Expr>,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) {
    let stat_code = StatCode::Call(
        result,
        name,
        zip(
            args,
            successors(Some(result), |arg_result| Some(*arg_result + 1)),
        )
        .map(|(expr, arg_result)| {
            translate_expr(expr, arg_result, stats, translation_data);
            arg_result
        })
        .collect(),
    );
    stats.push(stat_code);
}

pub(super) fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) {
    match expr {
        _ => todo!(),
    }
}
