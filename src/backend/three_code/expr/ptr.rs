use super::num::{propagate_num_const, translate_num_expr};
use super::{
    super::{BinOp, OpSrc, PtrSrc, Size, StatCode},
    translate_expr, translate_function_call, ExprTranslationData,
};
use crate::intermediate::{self as ir, VarRepr};
use std::iter::{successors, zip};

impl From<PtrSrc> for OpSrc {
    fn from(ptr_const: PtrSrc) -> Self {
        match ptr_const {
            PtrSrc::DataRef(data_ref, offset) => OpSrc::DataRef(data_ref, offset),
            PtrSrc::Null => OpSrc::Const(0),
        }
    }
}

/// If a constant was returned during expression translation instead of having been
/// pushed as a statement, it will be flushed as a statement forcefully here.
pub(in super::super) fn propagate_ptr_const(
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    ptr_const: Option<PtrSrc>,
) {
    if let Some(ptr_const) = ptr_const {
        stats.push(StatCode::Assign(result, OpSrc::from(ptr_const)));
    }
}

/// Translates a pointer expression into a series of statements. The result of the
/// expression tree is placed in the result field. If the expression was expressible
/// as a constant pointer, the pointer is returned instead and no statements are added
/// to the stats vector.
pub(in super::super) fn translate_ptr_expr(
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
                OpSrc::from(ptr_const)
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let (num_const, _) = translate_num_expr(num_expr, sub_result, stats, translation_data);
            let num_op_src = if let Some(num_const) = num_const && translation_data.should_propagate() {
                OpSrc::from(num_const)
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
        malloc @ ir::PtrExpr::Malloc(_) | malloc @ ir::PtrExpr::WideMalloc(_) => {
            let is_wide = if let ir::PtrExpr::WideMalloc(_) = malloc {
                true
            } else {
                false
            };
            let exprs = if let ir::PtrExpr::Malloc(exprs) = malloc {
                exprs
            } else if let ir::PtrExpr::WideMalloc(exprs) = malloc {
                exprs
            } else {
                unreachable!();
            };

            let mut setting_stats = Vec::new();
            let width: i32 = zip(
                exprs,
                successors(Some(result + 1), |sub_result| Some(sub_result + 1)),
            )
            .into_iter()
            .map(|(expr, sub_result)| {
                if is_wide {
                    return 4;
                }
                match translate_expr(expr, sub_result, &mut setting_stats, translation_data) {
                    ir::Type::Num(ir::NumSize::DWord) => 4,
                    ir::Type::Num(ir::NumSize::Word) => 2,
                    ir::Type::Num(ir::NumSize::Byte) => 1,
                    ir::Type::Bool => 1,
                    ir::Type::Ptr => 4,
                }
            })
            .sum();

            stats.push(StatCode::Assign(result, OpSrc::from(width)));
            stats.push(StatCode::Call(result, "malloc".to_string(), vec![result]));
            stats.append(&mut setting_stats);
            None
        }
        ir::PtrExpr::Call(name, args) => {
            translate_function_call(name, args, result, stats, translation_data);
            None
        }
    }
}
