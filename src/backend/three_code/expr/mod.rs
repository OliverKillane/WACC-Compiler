mod bool;
mod num;
mod ptr;

use self::{
    bool::{propagate_bool_const, translate_bool_expr},
    num::{propagate_num_const, translate_num_expr},
    ptr::{propagate_ptr_const, translate_ptr_expr},
};
use super::{super::Options, OpSrc, StatCode};
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

/// Static data from the function/program the expression is being translated in.
#[derive(Clone, Copy)]
pub(super) struct ExprTranslationData<'l> {
    /// All the local variables and their types.
    vars: &'l HashMap<VarRepr, ir::Type>,
    /// All the functions and their definitions
    functions: &'l HashMap<String, ir::Function>,
    /// Compilation options
    options: &'l Options,
}

impl<'l> ExprTranslationData<'l> {
    /// Creates a expression translation data struct.
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

    /// Returns whether the compilation options state that constant propagation
    /// should be performed.
    fn should_propagate(&self) -> bool {
        self.options.propagation != PropagationOpt::None
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
/// in the result variable. Returns the type of the expression.
pub(super) fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> ir::Type {
    match expr {
        ir::Expr::Num(num_expr) => {
            let (num_const, size) = translate_num_expr(num_expr, result, stats, translation_data);
            propagate_num_const(result, stats, num_const);
            ir::Type::Num(size)
        }
        ir::Expr::Bool(bool_expr) => {
            let bool_const = translate_bool_expr(bool_expr, result, stats, translation_data);
            propagate_bool_const(result, stats, bool_const);
            ir::Type::Bool
        }
        ir::Expr::Ptr(ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, translation_data);
            propagate_ptr_const(result, stats, ptr_const);
            ir::Type::Bool
        }
    }
}
