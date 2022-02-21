use super::num::translate_num_expr;
use super::ptr::translate_ptr_expr;
use super::{
    super::{BinOp, OpSrc, Size, StatCode},
    translate_function_call, ExprTranslationData,
};
use crate::intermediate::{self as ir, VarRepr};

impl From<ir::BoolOp> for BinOp {
    fn from(bool_op: ir::BoolOp) -> Self {
        match bool_op {
            ir::BoolOp::And => BinOp::And,
            ir::BoolOp::Or => BinOp::Or,
            ir::BoolOp::Xor => BinOp::Xor,
        }
    }
}

/// Translates a boolean expression into a series of statements. The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used.
pub(in super::super) fn translate_bool_expr(
    bool_expr: ir::BoolExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) {
    match bool_expr {
        ir::BoolExpr::Const(bool_const) => {
            stats.push(StatCode::Assign(result, OpSrc::from(bool_const as i32)));
        }
        ir::BoolExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
        }
        ir::BoolExpr::Deref(ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stats, translation_data);
            stats.push(StatCode::LoadVar(result, result, Size::Byte));
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::And,
                OpSrc::from(0x01),
            ));
        }
        ir::BoolExpr::TestZero(num_expr) => {
            translate_num_expr(num_expr, result, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Eq,
                OpSrc::from(0x00),
            ));
        }
        ir::BoolExpr::TestPositive(num_expr) => {
            translate_num_expr(num_expr, result, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Gt,
                OpSrc::from(0x00),
            ));
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            translate_ptr_expr(ptr_expr1, result, stats, translation_data);
            translate_ptr_expr(ptr_expr2, result + 1, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Eq,
                OpSrc::Var(result + 1),
            ));
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            translate_bool_expr(bool_expr1, result, stats, translation_data);
            translate_bool_expr(bool_expr2, result + 1, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                bool_op.into(),
                OpSrc::Var(result + 1),
            ));
        }
        ir::BoolExpr::Not(box bool_expr) => {
            let bool_const = translate_bool_expr(bool_expr, result, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Xor,
                OpSrc::from(0x01),
            ));
        }
        ir::BoolExpr::Call(name, args) => {
            translate_function_call(name, args, result, stats, translation_data);
        }
    }
}
