use super::num::translate_num_expr;
use super::{
    super::{get_type_width, BinOp, OpSrc, PtrSrc, Size, StatCode},
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

/// Translates a pointer expression into a series of statements. The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used.
pub(in super::super) fn translate_ptr_expr(
    ptr_expr: ir::PtrExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) {
    match ptr_expr {
        ir::PtrExpr::Null => {
            stats.push(StatCode::Assign(result, OpSrc::from(PtrSrc::Null)));
        }
        ir::PtrExpr::DataRef(data_ref) => {
            stats.push(StatCode::Assign(
                result,
                OpSrc::from(PtrSrc::DataRef(data_ref, 0)),
            ));
        }
        ir::PtrExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
        }
        ir::PtrExpr::Deref(box ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stats, translation_data);
            stats.push(StatCode::LoadVar(result, result, Size::DWord));
        }
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            translate_ptr_expr(ptr_expr, result, stats, translation_data);
            translate_num_expr(num_expr, result + 1, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Add,
                OpSrc::Var(result + 1),
            ));
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
                    4
                } else {
                    get_type_width(translate_expr(
                        expr,
                        sub_result,
                        &mut setting_stats,
                        translation_data,
                    ))
                    .into()
                }
            })
            .sum();

            stats.push(StatCode::Assign(result, OpSrc::from(width)));
            stats.push(StatCode::Call(result, "malloc".to_string(), vec![result]));
            stats.append(&mut setting_stats);
        }
        ir::PtrExpr::Call(name, args) => {
            translate_function_call(name, args, result, stats, translation_data);
        }
    }
}
