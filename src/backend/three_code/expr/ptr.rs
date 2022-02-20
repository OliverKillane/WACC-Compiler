use super::num::{propagate_num_const, translate_num_expr};
use super::{
    super::{BinOp, OpSrc, PtrSrc, Size, StatCode},
    translate_function_call, ExprTranslationData,
};
use crate::intermediate::{self as ir, VarRepr};

pub(super) fn propagate_ptr_const(
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    ptr_const: Option<PtrSrc>,
) {
    if let Some(ptr_const) = ptr_const {
        stats.push(StatCode::Assign(result, ptr_const.into()));
    }
}

pub(super) fn translate_ptr_expr(
    ptr_expr: ir::PtrExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> Option<PtrSrc> {
    match ptr_expr {
        ir::PtrExpr::Null => Some(PtrSrc::Null),
        ir::PtrExpr::DataRef(data_ref) => Some(PtrSrc::DataRef(data_ref, 0)),
        ir::PtrExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            None
        }
        ir::PtrExpr::Deref(box ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, translation_data);
            if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                stats.push(StatCode::LoadImm(result, ptr_const, Size::DWord));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, Size::DWord));
            }
            None
        }
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            let mut sub_result = result;
            let ptr_const = translate_ptr_expr(ptr_expr, sub_result, stats, translation_data);
            let ptr_op_src = if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let (num_const, _) = translate_num_expr(num_expr, sub_result, stats, translation_data);
            let num_op_src = if let Some(num_const) = num_const && translation_data.should_propagate() {
                num_const.into()
            } else {
                propagate_num_const(sub_result, stats, num_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::DataRef(data_ref, offset1), OpSrc::Const(offset2)) =
                (ptr_op_src, num_op_src)
            {
                Some(PtrSrc::DataRef(data_ref, offset1 + offset2))
            } else {
                stats.push(StatCode::AssignOp(
                    result,
                    ptr_op_src,
                    BinOp::Add,
                    num_op_src,
                ));
                None
            }
        }

        _ => todo!(),
    }
}
