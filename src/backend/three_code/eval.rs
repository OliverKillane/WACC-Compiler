use super::stat::get_type_width;
use crate::intermediate as ir;

fn eval_bool_expr(bool_expr: ir::BoolExpr) -> bool {
    match bool_expr {
        ir::BoolExpr::Const(bool_const) => bool_const,
        ir::BoolExpr::TestZero(num_expr) => {
            let (_, num_const) = eval_num_expr(num_expr);
            num_const == 0
        }
        ir::BoolExpr::TestPositive(num_expr) => {
            let (_, num_const) = eval_num_expr(num_expr);
            num_const > 0
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            eval_ptr_expr(ptr_expr1) == eval_ptr_expr(ptr_expr2)
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            let bool_const1 = eval_bool_expr(bool_expr1);
            let bool_const2 = eval_bool_expr(bool_expr2);
            match bool_op {
                ir::BoolOp::And => bool_const1 && bool_const2,
                ir::BoolOp::Or => bool_const1 || bool_const2,
                ir::BoolOp::Xor => bool_const1 ^ bool_const2,
            }
        }
        ir::BoolExpr::Not(box bool_expr) => !eval_bool_expr(bool_expr),
        _ => todo!("Not a constant eval expression"),
    }
}

fn trim_num_const(size: ir::NumSize, num_const: i32) -> i32 {
    match size {
        ir::NumSize::DWord => num_const,
        ir::NumSize::Word => num_const as u16 as i32,
        ir::NumSize::Byte => num_const as u8 as i32,
    }
}

fn eval_num_expr(num_expr: ir::NumExpr) -> (ir::NumSize, i32) {
    match num_expr {
        ir::NumExpr::SizeOf(expr_type) => (ir::NumSize::DWord, get_type_width(expr_type).into()),
        ir::NumExpr::SizeOfWideAlloc => (ir::NumSize::DWord, 4),
        ir::NumExpr::Const(size, num_const) => (size, num_const),
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let (size1, num_const1) = eval_num_expr(num_expr1);
            let (size2, num_const2) = eval_num_expr(num_expr2);
            (
                size1,
                trim_num_const(
                    size1,
                    match arith_op {
                        ir::ArithOp::Add => num_const1 + num_const2,
                        ir::ArithOp::Sub => num_const1 - num_const2,
                        ir::ArithOp::Mul => num_const1 * num_const2,
                        ir::ArithOp::Div => num_const1 / num_const2,
                        ir::ArithOp::Mod => num_const1 % num_const2,
                    },
                ),
            )
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let (_, num_const) = eval_num_expr(num_expr);
            (size, trim_num_const(size, num_const))
        }
        _ => todo!("Not a constant eval expression"),
    }
}

fn eval_ptr_expr(ptr_expr: ir::PtrExpr) -> i32 {
    match ptr_expr {
        ir::PtrExpr::Null => 0,
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            let ptr_const = eval_ptr_expr(ptr_expr);
            let (_, num_const) = eval_num_expr(num_expr);
            ptr_const + num_const
        }
        _ => todo!("Not a constant eval expression"),
    }
}

pub(super) fn eval_expr(expr: ir::Expr) -> Vec<u8> {
    match expr {
        ir::Expr::Num(num_expr) => {
            let (size, num_const) = eval_num_expr(num_expr);
            match size {
                ir::NumSize::DWord => Vec::from(num_const.to_ne_bytes()),
                ir::NumSize::Word => Vec::from((num_const as u16).to_ne_bytes()),
                ir::NumSize::Byte => vec![num_const as u8],
            }
        }
        ir::Expr::Bool(bool_expr) => {
            let bool_const = eval_bool_expr(bool_expr);
            vec![bool_const as u8]
        }
        ir::Expr::Ptr(ptr_expr) => {
            let ptr_const = eval_ptr_expr(ptr_expr);
            Vec::from(ptr_const.to_ne_bytes())
        }
    }
}
