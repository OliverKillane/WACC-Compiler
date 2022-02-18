//! Expression translator for AST to IR

use crate::frontend::ast;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;
use crate::intermediate::{ArithOp, BoolExpr, BoolOp, Expr::*, NumExpr, NumSize::*, PtrExpr, Type};

use super::super::super::intermediate::Expr as IRExpr;
use super::super::ast::{Expr, ExprSpan, UnOp, WrapSpan};

/// usize -> &'a str
pub fn translate_expr<'a>(
    WrapSpan(_, ast_expr): ExprSpan<'a, usize>,
    var_symb: &VariableSymbolTable,
) -> IRExpr {
    match ast_expr {
        // pair literal 'null' - maybe define null as PtrExpr in IR?
        Expr::Null => Ptr(PtrExpr::Null),

        Expr::Int(i) => Num(NumExpr::Const(DWord, i)),

        Expr::Bool(b) => Bool(BoolExpr::Const(b)),

        Expr::Char(c) => Num(NumExpr::Const(Byte, c as i32)),

        // Use ptr to the vec that represents the String?
        // character array
        // vec([size]), [items]..[items])
        Expr::String(s) => {
            todo!()
        }

        Expr::Var(v) => match var_symb.get_type_from_id(v) {
            Some(t) => match t {
                // ast::Type::Int => Num(NumExpr::Var(v)),
                ast::Type::Bool => todo!(),
                ast::Type::Char => todo!(),
                ast::Type::String => todo!(),
                ast::Type::Any => todo!(),
                ast::Type::Generic(_) => todo!(),
                ast::Type::Pair(_, _) => todo!(),
                ast::Type::Array(_, _) => todo!(),
                _ => todo!()
            },
            None => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
        }

        Expr::ArrayElem(_, _) => {
            todo!()
        }

        Expr::UnOp(o, box e) => match (o, translate_expr(e, var_symb)) {
            (UnOp::Neg, Bool(b)) => Bool(BoolExpr::Not(box b)),
            (UnOp::Minus, Num(n)) => Num(NumExpr::ArithOp(
                box NumExpr::Const(DWord, -1),
                ArithOp::Mul,
                box n,
            )),
            (UnOp::Chr, Num(n)) => Num(NumExpr::Cast(Byte, box n)),
            (UnOp::Ord, Num(n)) => Num(NumExpr::Cast(DWord, box n)),
            _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
        },
        // NumExpr::Deref(NumSize::DWord, PtrExpr::Var(var))
        // someexpr::Deref(PtrExpr::Offset(PtrExpr::Var(pair), NumExpr::SizeOf(first_field_type)))
        Expr::UnOp(o, box v) => match o {
            // deref ptr 
            UnOp::Len => todo!(),
            UnOp::Fst => todo!(),
            UnOp::Snd => todo!(),
            _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
        },

        // Should change this so that boolean operations are short-circuited
        Expr::BinOp(box e1, o, box e2) => {
            match (
                translate_expr(e1, var_symb),
                o,
                translate_expr(e2, var_symb),
            ) {
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
                }
                (Bool(b1), ast::BinOp::Or, Bool(b2)) => {
                    Bool(BoolExpr::BoolOp(box b1, BoolOp::Or, box b2))
                }
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
                _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
            }
        }
        _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
    }
}

#[cfg(test)]
mod tests {
    use crate::{frontend::ast::BinOp, intermediate};

    use super::*;

    #[test]
    fn check_boolean_expression() {
        let var_symb = VariableSymbolTable::new();

        let expr: WrapSpan<Expr<usize>> = WrapSpan(
            "!true",
            Expr::UnOp(UnOp::Neg, box WrapSpan("true", Expr::Bool(true))),
        );

        let translated: intermediate::Expr = Bool(BoolExpr::Not(box BoolExpr::Const(true)));

        assert_eq!(translate_expr(expr, &var_symb), translated);

        let expr2: WrapSpan<Expr<usize>> = WrapSpan(
            "(ord 'a' == 65) || (true && !false)",
            Expr::BinOp(
                box WrapSpan(
                    "ord 'a' == 65",
                    Expr::BinOp(
                        box WrapSpan(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box WrapSpan("'a'", Expr::Char('a'))),
                        ),
                        BinOp::Eq,
                        box WrapSpan("65", Expr::Int(65)),
                    ),
                ),
                BinOp::Or,
                box WrapSpan(
                    "true && !false",
                    Expr::BinOp(
                        box WrapSpan("true", Expr::Bool(true)),
                        BinOp::And,
                        box WrapSpan(
                            "!false",
                            Expr::UnOp(UnOp::Neg, box WrapSpan("false", Expr::Bool(false))),
                        ),
                    ),
                ),
            ),
        );

        let translated2: intermediate::Expr = Bool(BoolExpr::BoolOp(
            box BoolExpr::TestZero(NumExpr::ArithOp(
                box NumExpr::Cast(DWord, box NumExpr::Const(Byte, 'a' as i32)),
                ArithOp::Sub,
                box NumExpr::Const(DWord, 65),
            )),
            BoolOp::Or,
            box BoolExpr::BoolOp(
                box BoolExpr::Const(true),
                BoolOp::And,
                box BoolExpr::Not(box BoolExpr::Const(false)),
            ),
        ));

        assert_eq!(translate_expr(expr2, &var_symb), translated2);
    }
}
