//! Expression translator for AST to IR

use core::num;

use crate::frontend::ast;
use crate::intermediate::{ArithOp, BoolExpr, BoolOp, Expr::*, NumExpr, NumSize::*, PtrExpr, Type};

use super::super::super::intermediate::Expr as IRExpr;
use super::super::ast::{Expr, ExprSpan, UnOp, WrapSpan};

/// usize -> &'a str
pub fn translate_expr<'a>(WrapSpan(_, ast_expr): ExprSpan<'a, usize>) -> IRExpr {
    match ast_expr {
        // pair literal 'null' - maybe define null as PtrExpr in IR?
        Expr::Null => Ptr(PtrExpr::Null),

        Expr::Int(i) => Num(NumExpr::Const(DWord, i)),

        Expr::Bool(b) => Bool(BoolExpr::Const(b)),

        Expr::Char(c) => Num(NumExpr::Const(Byte, c as i32)),

        // Use ptr to the vec that represents the String?
        Expr::String(s) => {
            todo!()
        }

        Expr::Var(v) => {
            // Variable Symbol Table
            todo!()
        }

        Expr::ArrayElem(_, _) => {
            todo!()
        }

        Expr::UnOp(o, box e) => match (o, translate_expr(e)) {
            (UnOp::Neg, Bool(b)) => Bool(BoolExpr::Not(box b)),
            (UnOp::Minus, Num(n)) => Num(NumExpr::ArithOp(
                box NumExpr::Const(DWord, -1),
                ArithOp::Mul,
                box n,
            )),
            (UnOp::Chr, Num(n)) => Num(NumExpr::Cast(Byte, box n)),
            (UnOp::Ord, Num(n)) => Num(NumExpr::Cast(DWord, box n)),
            _ => panic!("Shouldn't reach this!"),
        },
        Expr::UnOp(o, v) => match o {
            UnOp::Len => todo!(),
            UnOp::Fst => todo!(),
            UnOp::Snd => todo!(),
            _ => panic!("Shouldn't reach this!"),
        },

        Expr::BinOp(box e1, o, box e2) => {
            match (translate_expr(e1), o, translate_expr(e2)) {
                (Num(n1), ast::BinOp::Add, Num(n2)) => {
                    Num(NumExpr::ArithOp(box n1, ArithOp::Add, box n2))
                }
                (Num(n1), ast::BinOp::Sub, Num(n2)) => {
                    Num(NumExpr::ArithOp(box n1, ArithOp::Sub, box n2))
                }
                (Num(n1), ast::BinOp::Mul, Num(n2)) => {
                    Num(NumExpr::ArithOp(box n1, ArithOp::Mul, box n2))
                }
                (Num(n1), ast::BinOp::Div, Num(n2)) => {
                    Num(NumExpr::ArithOp(box n1, ArithOp::Div, box n2))
                }
                (Num(n1), ast::BinOp::Mod, Num(n2)) => {
                    Num(NumExpr::ArithOp(box n1, ArithOp::Mod, box n2))
                }
                (Num(n1), ast::BinOp::Gt, Num(n2)) => Bool(BoolExpr::TestPositive(
                    NumExpr::ArithOp(box n1, ArithOp::Sub, box n2),
                )),
                (Num(n1), ast::BinOp::Gte, Num(n2)) => Bool(BoolExpr::Not(
                    box BoolExpr::TestPositive(NumExpr::ArithOp(box n2, ArithOp::Sub, box n1)),
                )),
                (Num(n1), ast::BinOp::Lt, Num(n2)) => Bool(BoolExpr::TestPositive(
                    NumExpr::ArithOp(box n2, ArithOp::Sub, box n1),
                )),
                (Num(n1), ast::BinOp::Lte, Num(n2)) => Bool(BoolExpr::Not(
                    box BoolExpr::TestPositive(NumExpr::ArithOp(box n1, ArithOp::Sub, box n2)),
                )),
                (Num(n1), ast::BinOp::Eq, Num(n2)) => Bool(BoolExpr::TestZero(NumExpr::ArithOp(
                    box n1,
                    ArithOp::Sub,
                    box n2,
                ))),
                (Num(n1), ast::BinOp::Ne, Num(n2)) => Bool(BoolExpr::Not(box BoolExpr::TestZero(
                    NumExpr::ArithOp(box n1, ArithOp::Sub, box n2),
                ))),
                (Bool(b1), ast::BinOp::And, Bool(b2)) => {
                    Bool(BoolExpr::BoolOp(box b1, BoolOp::And, box b2))
                },
                (Bool(b1), ast::BinOp::Or, Bool(b2)) => {
                    Bool(BoolExpr::BoolOp(box b1, BoolOp::Or, box b2))
                },
                (Bool(b1), ast::BinOp::Eq, Bool(b2)) => Bool(BoolExpr::Not(box BoolExpr::BoolOp(
                    box b1,
                    BoolOp::Xor,
                    box b2,
                ))),
                (Bool(b1), ast::BinOp::Ne, Bool(b2)) => {
                    Bool(BoolExpr::BoolOp(box b1, BoolOp::Xor, box b2))
                }
                (e1, ast::BinOp::Newpair, e2) => {
                    todo!()
                }
                _ => panic!("Shouldn't reach this!"),
            }
        },
        _ => panic!("Shouldn't reach this!"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
