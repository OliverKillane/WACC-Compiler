use super::num::{propagate_num_const, translate_num_expr};
use super::ptr::{propagate_ptr_const, translate_ptr_expr};
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

pub(super) fn propagate_bool_const(
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    bool_const: Option<bool>,
) {
    if let Some(bool_const) = bool_const {
        stats.push(StatCode::Assign(result, (bool_const as i32).into()));
    }
}

pub(super) fn translate_bool_expr(
    bool_expr: ir::BoolExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> Option<bool> {
    match bool_expr {
        ir::BoolExpr::Const(val) => Some(val),
        ir::BoolExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            None
        }
        ir::BoolExpr::Deref(ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, translation_data);
            if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                stats.push(StatCode::LoadImm(result, ptr_const, Size::Byte));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, Size::Byte));
            }
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::And,
                0x01.into(),
            ));
            None
        }
        ir::BoolExpr::TestZero(num_expr) => {
            let (num_const, _) = translate_num_expr(num_expr, result, stats, translation_data);
            if let Some(num_const) = num_const && translation_data.should_propagate() {
                Some(num_const == 0)
            } else {
                propagate_num_const(result, stats, num_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Eq, 0x00.into()));
                None
            }
        }
        ir::BoolExpr::TestPositive(num_expr) => {
            let (num_const, _) = translate_num_expr(num_expr, result, stats, translation_data);
            if let Some(num_const) = num_const && translation_data.should_propagate() {
                Some(num_const == 0)
            } else {
                propagate_num_const(result, stats, num_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Gt, 0x00.into()));
                None
            }
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            let mut sub_result = result;
            let ptr_const = translate_ptr_expr(ptr_expr1, sub_result, stats, translation_data);
            let op_src1 = if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let ptr_const = translate_ptr_expr(ptr_expr2, sub_result, stats, translation_data);
            let op_src2 = if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                OpSrc::Var(sub_result)
            };
            if let (
                OpSrc::DataRef(_, _) | OpSrc::Const(_),
                OpSrc::DataRef(_, _) | OpSrc::Const(_),
            ) = (op_src1, op_src2)
            {
                Some(op_src1 == op_src2)
            } else {
                stats.push(StatCode::AssignOp(result, op_src1, BinOp::Eq, op_src2));
                None
            }
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            let mut sub_result = result;
            let bool_const = translate_bool_expr(bool_expr1, sub_result, stats, translation_data);
            let op_src1 = if let Some(bool_const) = bool_const && translation_data.should_propagate() {
                (bool_const as i32).into()
            } else {
                propagate_bool_const(sub_result, stats, bool_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let bool_const = translate_bool_expr(bool_expr2, sub_result, stats, translation_data);
            let op_src2 = if let Some(bool_const) = bool_const && translation_data.should_propagate() {
                (bool_const as i32).into()
            } else {
                propagate_bool_const(sub_result, stats, bool_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::Const(bool_const1), OpSrc::Const(bool_const2)) = (op_src1, op_src2) {
                let bool_const1 = bool_const1 != 0;
                let bool_const2 = bool_const2 != 0;
                Some(match bool_op {
                    ir::BoolOp::And => bool_const1 && bool_const2,
                    ir::BoolOp::Or => bool_const1 || bool_const2,
                    ir::BoolOp::Xor => bool_const1 ^ bool_const2,
                })
            } else {
                stats.push(StatCode::AssignOp(result, op_src1, bool_op.into(), op_src2));
                None
            }
        }
        ir::BoolExpr::Not(box bool_expr) => {
            let bool_const = translate_bool_expr(bool_expr, result, stats, translation_data);
            if let Some(bool_const) = bool_const && translation_data.should_propagate() {
                Some(!bool_const)
            } else {
                propagate_bool_const(result, stats, bool_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Xor, 0x01.into()));
                None
            }
        }
        ir::BoolExpr::Call(name, args) => {
            translate_function_call(name, args, result, stats, translation_data);
            None
        }
    }
}
