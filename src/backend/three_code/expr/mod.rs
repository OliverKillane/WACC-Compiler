pub(super) mod bool;
pub(super) mod num;
pub(super) mod ptr;

use self::{bool::translate_bool_expr, num::translate_num_expr, ptr::translate_ptr_expr};
use super::{super::Options, OpSrc, StatCode};
use crate::intermediate::{self as ir, VarRepr};
use std::{
    collections::HashMap,
    iter::{successors, zip},
};

impl From<i32> for OpSrc {
    fn from(num: i32) -> Self {
        OpSrc::Const(num)
    }
}

/// Static data from the function/program the expression is being translated in.
#[derive(Clone, Copy)]
pub(super) struct ExprTranslationData<'l> {
    /// All the local variables and their types.
    vars: &'l HashMap<VarRepr, ir::Type>,
    /// All the functions and their definitions
    functions: &'l HashMap<String, ir::Function>,
}

impl<'l> ExprTranslationData<'l> {
    /// Creates a expression translation data struct.
    pub(super) fn new(
        vars: &'l HashMap<VarRepr, ir::Type>,
        functions: &'l HashMap<String, ir::Function>,
    ) -> Self {
        ExprTranslationData { vars, functions }
    }
}

/// Helper function for translating function call expressions.
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

/// Translates a single general expression. The result of the expression is placed
/// in the result variable. Returns the type of the expression. It is assumed that
/// no variables after the result variable are used.
pub(super) fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> ir::Type {
    match expr {
        ir::Expr::Num(num_expr) => {
            let size = translate_num_expr(num_expr, result, stats, translation_data);
            ir::Type::Num(size)
        }
        ir::Expr::Bool(bool_expr) => {
            translate_bool_expr(bool_expr, result, stats, translation_data);
            ir::Type::Bool
        }
        ir::Expr::Ptr(ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stats, translation_data);
            ir::Type::Ptr
        }
    }
}
