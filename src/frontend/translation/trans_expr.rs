//! Expression translator for AST to IR

use std::collections::HashMap;

use super::super::ast::{Expr, ExprSpan, UnOp, WrapSpan};
use crate::frontend::ast;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;

use super::super::super::intermediate::Expr as IRExpr;
use crate::intermediate::{
    ArithOp, BoolExpr, BoolOp, DataRef,
    Expr::*,
    NumExpr,
    NumSize::{self, *},
    PtrExpr, Type,
};

/// usize -> &'a str
pub fn translate_expr<'a>(
    WrapSpan(_, ast_expr): ExprSpan<'a, usize>,
    var_symb: &VariableSymbolTable,
    dataref_map: &mut HashMap<DataRef, Vec<IRExpr>>,
) -> IRExpr {
    match ast_expr {
        // pair literal 'null' - maybe define null as PtrExpr in IR?
        Expr::Null => Ptr(PtrExpr::Null),

        Expr::Int(int) => Num(NumExpr::Const(DWord, int)),

        Expr::Bool(bln) => Bool(BoolExpr::Const(bln)),

        Expr::Char(chr) => Num(NumExpr::Const(Byte, chr as i32)),

        Expr::String(str) => {
            let dataref: u64 = dataref_map.len() as u64;
            let mut char_vec: Vec<IRExpr> = Vec::new();

            for chr in str.chars() {
                char_vec.push(Num(NumExpr::Const(Byte, chr as i32)));
            }

            dataref_map.insert(dataref, char_vec);
            Ptr(PtrExpr::DataRef(dataref))
        }

        Expr::Var(var) => match var_symb.get_type_from_id(var) {
            Some(typ) => match typ {
                ast::Type::Int => Num(NumExpr::Var(var)),
                ast::Type::Bool => Bool(BoolExpr::Var(var)),
                ast::Type::Char => Num(NumExpr::Var(var)),
                ast::Type::String => Ptr(PtrExpr::Var(var)),
                ast::Type::Any => Ptr(PtrExpr::Var(var)),
                ast::Type::Pair(_, _) => Ptr(PtrExpr::Var(var)),
                ast::Type::Array(_, _) => Ptr(PtrExpr::Var(var)),
                _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
            },
            None => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
        }

        // Function call
        // create fun deref_array
        // i'm gonna be happy and everyone's gonna be super cool
        Expr::ArrayElem(_, _) => {
            todo!()
        }

        Expr::UnOp(un_op, box expr_span) => match (un_op, translate_expr(expr_span, var_symb, dataref_map)) {
            (UnOp::Neg, Bool(bln)) => Bool(BoolExpr::Not(box bln)),
            (UnOp::Minus, Num(num)) => Num(NumExpr::ArithOp(
                box NumExpr::Const(DWord, -1),
                ArithOp::Mul,
                box num,
            )),
            (UnOp::Chr, Num(num)) => Num(NumExpr::Cast(Byte, box num)),
            (UnOp::Ord, Num(num)) => Num(NumExpr::Cast(DWord, box num)),
            (UnOp::Len, Ptr(ptr)) => Num(NumExpr::Deref(NumSize::DWord, ptr)),
            (_, Ptr(ptr)) => todo!(),
            _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
        },

        // Should change this so that boolean operations are short-circuited
        Expr::BinOp(box expr_span1, bin_op, box expr_span2) => {
            match (
                translate_expr(expr_span1, var_symb, dataref_map),
                bin_op,
                translate_expr(expr_span2, var_symb, dataref_map),
            ) {
                (Num(num_expr1), ast::BinOp::Add, Num(num_expr2)) => {
                    Num(NumExpr::ArithOp(box num_expr1, ArithOp::Add, box num_expr2))
                }
                (Num(num_expr1), ast::BinOp::Sub, Num(num_expr2)) => {
                    Num(NumExpr::ArithOp(box num_expr1, ArithOp::Sub, box num_expr2))
                }
                (Num(num_expr1), ast::BinOp::Mul, Num(num_expr2)) => {
                    Num(NumExpr::ArithOp(box num_expr1, ArithOp::Mul, box num_expr2))
                }
                (Num(num_expr1), ast::BinOp::Div, Num(num_expr2)) => {
                    Num(NumExpr::ArithOp(box num_expr1, ArithOp::Div, box num_expr2))
                }
                (Num(num_expr1), ast::BinOp::Mod, Num(num_expr2)) => {
                    Num(NumExpr::ArithOp(box num_expr1, ArithOp::Mod, box num_expr2))
                }
                (Num(num_expr1), ast::BinOp::Gt, Num(num_expr2)) => Bool(BoolExpr::TestPositive(
                    NumExpr::ArithOp(box num_expr1, ArithOp::Sub, box num_expr2),
                )),
                (Num(num_expr1), ast::BinOp::Gte, Num(num_expr2)) => Bool(BoolExpr::Not(
                    box BoolExpr::TestPositive(NumExpr::ArithOp(box num_expr2, ArithOp::Sub, box num_expr1)),
                )),
                (Num(num_expr1), ast::BinOp::Lt, Num(num_expr2)) => Bool(BoolExpr::TestPositive(
                    NumExpr::ArithOp(box num_expr2, ArithOp::Sub, box num_expr1),
                )),
                (Num(num_expr1), ast::BinOp::Lte, Num(num_expr2)) => Bool(BoolExpr::Not(
                    box BoolExpr::TestPositive(NumExpr::ArithOp(box num_expr1, ArithOp::Sub, box num_expr2)),
                )),
                (Num(num_expr1), ast::BinOp::Eq, Num(num_expr2)) => Bool(BoolExpr::TestZero(NumExpr::ArithOp(
                    box num_expr1,
                    ArithOp::Sub,
                    box num_expr2,
                ))),
                (Num(num_expr1), ast::BinOp::Ne, Num(num_expr2)) => Bool(BoolExpr::Not(box BoolExpr::TestZero(
                    NumExpr::ArithOp(box num_expr1, ArithOp::Sub, box num_expr2),
                ))),
                (Bool(bln_expr1), ast::BinOp::And, Bool(bln_expr2)) => {
                    Bool(BoolExpr::BoolOp(box bln_expr1, BoolOp::And, box bln_expr2))
                }
                (Bool(bln_expr1), ast::BinOp::Or, Bool(bln_expr2)) => {
                    Bool(BoolExpr::BoolOp(box bln_expr1, BoolOp::Or, box bln_expr2))
                }
                (Bool(bln_expr1), ast::BinOp::Eq, Bool(bln_expr2)) => Bool(BoolExpr::Not(box BoolExpr::BoolOp(
                    box bln_expr1,
                    BoolOp::Xor,
                    box bln_expr2,
                ))),
                (Bool(bln_expr1), ast::BinOp::Ne, Bool(bln_expr2)) => {
                    Bool(BoolExpr::BoolOp(box bln_expr1, BoolOp::Xor, box bln_expr2))
                }
                (expr1, ast::BinOp::Newpair, expr2) => {
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
        let mut dataref_map: HashMap<DataRef, Vec<IRExpr>> = HashMap::new();

        let expr: WrapSpan<Expr<usize>> = WrapSpan(
            "!true",
            Expr::UnOp(UnOp::Neg, box WrapSpan("true", Expr::Bool(true))),
        );

        let translated: intermediate::Expr = Bool(BoolExpr::Not(box BoolExpr::Const(true)));

        assert_eq!(
            translate_expr(expr, &var_symb, &mut dataref_map),
            translated
        );

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

        assert_eq!(
            translate_expr(expr2, &var_symb, &mut dataref_map),
            translated2
        );
    }
}
