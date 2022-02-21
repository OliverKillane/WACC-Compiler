use super::ptr::translate_ptr_expr;
use super::{
    super::{get_type_width, BinOp, OpSrc, Size, StatCode},
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

/// Translates a numerical expression into a series of statements. The result of the
/// expression tree is placed in the result field. Returns the size of the resulting
/// numerical expression, i.e. the size of the variable placed in result.
/// It is assumed that no variables after the result variable are used.
pub(in super::super) fn translate_num_expr(
    num_expr: ir::NumExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    translation_data: ExprTranslationData,
) -> ir::NumSize {
    match num_expr {
        ir::NumExpr::SizeOf(expr_type) => {
            let type_width: i32 = get_type_width(expr_type).into();
            stats.push(StatCode::Assign(result, OpSrc::from(type_width)));
            ir::NumSize::DWord
        }
        ir::NumExpr::SizeOfWideAlloc => {
            stats.push(StatCode::Assign(result, OpSrc::from(4)));
            ir::NumSize::DWord
        }
        ir::NumExpr::Const(size, val) => {
            stats.push(StatCode::Assign(result, OpSrc::from(val)));
            size
        }
        ir::NumExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            let size = match translation_data.vars.get(&var).expect("Variable not found") {
                ir::Type::Num(size) => *size,
                _ => panic!("Variable of a wrong type"),
            };
            size.into()
        }
        ir::NumExpr::Deref(size, ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stats, translation_data);
            stats.push(StatCode::LoadVar(result, result, size.into()));
            size.into()
        }
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let size1 = translate_num_expr(num_expr1, result, stats, translation_data);
            let size2 = translate_num_expr(num_expr2, result + 1, stats, translation_data);
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                arith_op.into(),
                OpSrc::Var(result + 1),
            ));
            size1
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let old_size = translate_num_expr(num_expr, result, stats, translation_data);
            match (size, old_size) {
                (ir::NumSize::Byte, ir::NumSize::DWord | ir::NumSize::Word) => {
                    stats.push(StatCode::AssignOp(
                        result,
                        OpSrc::Var(result),
                        BinOp::And,
                        OpSrc::from(0xFF),
                    ));
                }
                (ir::NumSize::Word, ir::NumSize::DWord) => {
                    stats.push(StatCode::AssignOp(
                        result,
                        OpSrc::Var(result),
                        BinOp::And,
                        OpSrc::from(0xFFFF),
                    ));
                }
                _ => {}
            }
            size.into()
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
            size.into()
        }
    }
}
