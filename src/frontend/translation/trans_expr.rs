//! Expression translator for AST to IR

use std::collections::HashMap;

use super::super::ast::{ASTWrapper, Expr, ExprWrap, UnOp};
use crate::frontend::ast;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;

use super::super::super::intermediate::Expr as IRExpr;
use crate::intermediate::{
    ArithOp, BoolExpr, BoolOp, DataRef,
    Expr::*,
    NumExpr,
    NumSize::{self, *},
    PtrExpr,
};

/// usize -> &'a str
pub fn translate_expr<'a>(
    ASTWrapper(_, ast_expr): ExprWrap<Option<ast::Type>, usize>,
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
                ast::Type::Int | ast::Type::Char => Num(NumExpr::Var(var)),
                ast::Type::Bool => Bool(BoolExpr::Var(var)),
                ast::Type::String | ast::Type::Any | ast::Type::Pair(_, _) | ast::Type::Array(_, _) => Ptr(PtrExpr::Var(var)),
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

        Expr::UnOp(un_op, box expr_span) => match (un_op, translate_expr(expr_span.clone(), var_symb, dataref_map)) {
            (UnOp::Neg, Bool(bln)) => Bool(BoolExpr::Not(box bln)),
            (UnOp::Minus, Num(num)) => Num(NumExpr::ArithOp(
                box NumExpr::Const(DWord, -1),
                ArithOp::Mul,
                box num,
            )),
            (UnOp::Chr, Num(num)) => Num(NumExpr::Cast(Byte, box num)),
            (UnOp::Ord, Num(num)) => Num(NumExpr::Cast(DWord, box num)),
            (UnOp::Len, Ptr(ptr)) => Num(NumExpr::Deref(NumSize::DWord, ptr)),
            (UnOp::Fst, Ptr(ptr)) => match expr_span.0 {
                Some(typ) => match typ {
                    ast::Type::Int => Num(NumExpr::Deref(NumSize::DWord, ptr)),
                    ast::Type::Char | ast::Type::String => Num(NumExpr::Deref(NumSize::Byte, ptr)),
                    ast::Type::Bool => Bool(BoolExpr::Deref(ptr)),
                    ast::Type::Array(_, _) | ast::Type::Pair(_, _) | ast::Type::Any => Ptr(PtrExpr::Deref(box ptr)),
                    ast::Type::Generic(_) => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
                },
                None => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
            },
            (UnOp::Snd, Ptr(ptr)) => match expr_span.0 {
                Some(typ) => {
                    let offset_ptr = PtrExpr::Offset(box ptr, box NumExpr::SizeOfWideAlloc);
                    match typ {
                    ast::Type::Int => Num(NumExpr::Deref(NumSize::DWord, offset_ptr)),
                    ast::Type::Char | ast::Type::String => Num(NumExpr::Deref(NumSize::Byte, offset_ptr)),
                    ast::Type::Bool => Bool(BoolExpr::Deref(offset_ptr)),
                    ast::Type::Array(_, _) | ast::Type::Pair(_, _) | ast::Type::Any => Ptr(PtrExpr::Deref(box offset_ptr)),
                    ast::Type::Generic(_) => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
                }},
                None => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
            },
            _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!")
        },

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
                    Ptr(PtrExpr::WideMalloc(vec![expr1, expr2]))
                }
                _ => panic!("What are you even doing with your life by this point Oli? Semantic analyzer broken!"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{frontend::ast::BinOp, intermediate};

    use super::*;

    #[test]
    fn check_boolean_expression() {
        let expr: ExprWrap<Option<ast::Type>, usize> = ASTWrapper(
            Some(ast::Type::Bool),
            Expr::UnOp(
                UnOp::Neg,
                box ASTWrapper(Some(ast::Type::Bool), Expr::Bool(true)),
            ),
        );

        let translated: intermediate::Expr = Bool(BoolExpr::Not(box BoolExpr::Const(true)));

        assert_eq!(
            translate_expr(expr, &VariableSymbolTable::new(), &mut HashMap::new()),
            translated
        );

        let expr2: ExprWrap<Option<ast::Type>, usize> = ASTWrapper(
            Some(ast::Type::Bool),
            Expr::BinOp(
                box ASTWrapper(
                    Some(ast::Type::Bool),
                    Expr::BinOp(
                        box ASTWrapper(
                            Some(ast::Type::Int),
                            Expr::UnOp(
                                UnOp::Ord,
                                box ASTWrapper(Some(ast::Type::Char), Expr::Char('a')),
                            ),
                        ),
                        BinOp::Eq,
                        box ASTWrapper(Some(ast::Type::Int), Expr::Int(65)),
                    ),
                ),
                BinOp::Or,
                box ASTWrapper(
                    Some(ast::Type::Bool),
                    Expr::BinOp(
                        box ASTWrapper(Some(ast::Type::Bool), Expr::Bool(true)),
                        BinOp::And,
                        box ASTWrapper(
                            Some(ast::Type::Bool),
                            Expr::UnOp(
                                UnOp::Neg,
                                box ASTWrapper(Some(ast::Type::Bool), Expr::Bool(false)),
                            ),
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
            translate_expr(expr2, &VariableSymbolTable::new(), &mut HashMap::new()),
            translated2
        );
    }

    #[test]
    fn check_integer_expression() {
        let expr: ExprWrap<Option<ast::Type>, usize> = ASTWrapper(
            Some(ast::Type::Int),
            Expr::BinOp(
                box ASTWrapper(Some(ast::Type::Int), Expr::Int(3)),
                BinOp::Add,
                box ASTWrapper(Some(ast::Type::Int), Expr::Int(7)),
            ),
        );

        let translated: intermediate::Expr = intermediate::Expr::Num(NumExpr::ArithOp(
            box NumExpr::Const(NumSize::DWord, 3),
            ArithOp::Add,
            box NumExpr::Const(NumSize::DWord, 7),
        ));

        assert_eq!(
            translate_expr(expr, &VariableSymbolTable::new(), &mut HashMap::new()),
            translated
        );

        let expr: ExprWrap<Option<ast::Type>, usize> = ASTWrapper(
            Some(ast::Type::Int),
            Expr::BinOp(
                box ASTWrapper(
                    Some(ast::Type::Int),
                    Expr::BinOp(
                        box ASTWrapper(Some(ast::Type::Int), Expr::Var(0)),
                        BinOp::Mod,
                        box ASTWrapper(Some(ast::Type::Int), Expr::Int(7)),
                    ),
                ),
                BinOp::Mul,
                box ASTWrapper(
                    Some(ast::Type::Int),
                    Expr::UnOp(
                        UnOp::Ord,
                        box ASTWrapper(Some(ast::Type::Char), Expr::Char('a')),
                    ),
                ),
            ),
        );

        let mut var_table = VariableSymbolTable::new();
        var_table.0.insert(0, ast::Type::Int);

        let translated: intermediate::Expr = intermediate::Expr::Num(NumExpr::ArithOp(
            box NumExpr::ArithOp(
                box NumExpr::Var(0),
                ArithOp::Mod,
                box NumExpr::Const(NumSize::DWord, 7),
            ),
            ArithOp::Mul,
            box NumExpr::Cast(NumSize::DWord, box NumExpr::Const(Byte, 'a' as i32)),
        ));

        assert_eq!(
            translate_expr(expr, &var_table, &mut HashMap::new()),
            translated
        );
    }
}
