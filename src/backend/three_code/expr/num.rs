use super::ptr::{propagate_ptr_const, translate_ptr_expr};
use super::{
    super::{BinOp, OpSrc, Size, StatCode},
    translate_function_call, ExprTranslationData,
};
use crate::intermediate::{self as ir, VarRepr};

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

/// If a constant was returned during expression translation instead of having been
/// pushed as a statement, it will be flushed as a statement forcefully here.
pub(super) fn propagate_num_const(
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    num_const: Option<i32>,
) {
    if let Some(num_const) = num_const {
        stats.push(StatCode::Assign(result, num_const.into()));
    }
}

/// If a constant is bigger than the variable size, then trims it to fit in that size.
pub(super) fn clip_num_const(num_const: i32, size: &ir::NumSize) -> i32 {
    match size {
        ir::NumSize::DWord => num_const,
        ir::NumSize::Word => num_const as u16 as i32,
        ir::NumSize::Byte => num_const as u8 as i32,
    }
}

/// Translates a numerical expression into a series of statements. The result of the
/// expression tree is placed in the result field. If the expression was expressible
/// as a constant value, the constand value is returned instead as the first item in
/// the returned tuple and no statements are added to the stats vector. The second
/// item in the tuple represents the size of the resulting numerical expression, i.e.
/// the size of the variable placed in result.
pub(super) fn translate_num_expr(
    num_expr: ir::NumExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> (Option<i32>, ir::NumSize) {
    match num_expr {
        ir::NumExpr::SizeOf(size) => (
            Some(match size {
                ir::Type::Num(ir::NumSize::DWord) => 4,
                ir::Type::Num(ir::NumSize::Word) => 2,
                ir::Type::Num(ir::NumSize::Byte) => 1,
                ir::Type::Ptr => 4,
                ir::Type::Bool => 1,
            }),
            ir::NumSize::DWord,
        ),
        ir::NumExpr::SizeOfWideAlloc => (Some(4), ir::NumSize::DWord),
        ir::NumExpr::Const(size, val) => (Some(val), size),
        ir::NumExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            let size = match translation_data.vars.get(&var).expect("Variable not found") {
                ir::Type::Num(size) => *size,
                _ => panic!("Variable of a wrong type"),
            };
            (None, size.into())
        }
        ir::NumExpr::Deref(size, ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, translation_data);
            if let Some(ptr_const) = ptr_const && translation_data.should_propagate() {
                stats.push(StatCode::LoadImm(result, ptr_const, size.into()));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, size.into()));
            }
            (None, size.into())
        }
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let mut sub_result = result;
            let (num_const, size1) =
                translate_num_expr(num_expr1, sub_result, stats, translation_data);
            let op_src1 = if let Some(num_const) = num_const && translation_data.should_propagate() {
                OpSrc::from(num_const)
            } else {
                propagate_num_const(sub_result, stats, num_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let (num_const, size2) =
                translate_num_expr(num_expr2, sub_result, stats, translation_data);
            let op_src2 = if let Some(num_const) = num_const && translation_data.should_propagate() {
                OpSrc::from(num_const)
            } else {
                propagate_num_const(sub_result, stats, num_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::Const(num_const1), OpSrc::Const(num_const2)) = (op_src1, op_src2) {
                (
                    Some(clip_num_const(
                        match arith_op {
                            ir::ArithOp::Add => num_const1 + num_const2,
                            ir::ArithOp::Sub => num_const1 - num_const2,
                            ir::ArithOp::Mul => num_const1 * num_const2,
                            ir::ArithOp::Div => num_const1 / num_const2,
                            ir::ArithOp::Mod => num_const1 % num_const2,
                        },
                        &size1,
                    )),
                    size1,
                )
            } else {
                stats.push(StatCode::AssignOp(
                    result,
                    op_src1,
                    arith_op.into(),
                    op_src2,
                ));
                (None, size1)
            }
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let (num_const, old_size) =
                translate_num_expr(num_expr, result, stats, translation_data);
            if let Some(num_const) = num_const && translation_data.should_propagate() {
                (Some(clip_num_const(num_const, &size)), size.into())
            } else {
                propagate_num_const(result, stats, num_const);
                match (size, old_size) {
                    (ir::NumSize::Byte, ir::NumSize::DWord | ir::NumSize::Word) => {
                        stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::And, OpSrc::from(0xFF)));
                    }
                    (ir::NumSize::Word, ir::NumSize::DWord) => {
                        stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::And, OpSrc::from(0xFFFF)));
                    }
                    _ => {}
                }
                (None, size.into())
            }
        }
        ir::NumExpr::Call(name, args) => {
            let size = if let ir::Function(ir::Type::Num(size), _, _, _) = translation_data
                .functions
                .get(&name)
                .expect("Function not found")
            {
                *size
            } else {
                panic!("Function has a wrong return type")
            };
            translate_function_call(name, args, result, stats, translation_data);
            (None, size.into())
        }
    }
}
