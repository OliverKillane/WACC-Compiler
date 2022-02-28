use std::collections::HashMap;

use super::super::ast::{ASTWrapper, Expr, Type, UnOp};
use crate::frontend::ast;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;
use crate::intermediate::{self as ir, DataRef};

pub fn translate_expr(
    ast_expr: Expr<Option<Type>, usize>,
    ast_expr_type: &Type,
    var_symb: &VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> ir::Expr {
    match ast_expr {
        Expr::Null => ir::Expr::Ptr(ir::PtrExpr::Null),
        Expr::Int(int_const) => ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, int_const)),
        Expr::Bool(bool_const) => ir::Expr::Bool(ir::BoolExpr::Const(bool_const)),
        Expr::Char(char_const) => {
            ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, char_const as i32))
        }
        Expr::String(str_const) => {
            let data_ref: u64 = data_ref_map.len() as u64;
            data_ref_map.insert(
                data_ref,
                str_const
                    .into_bytes()
                    .into_iter()
                    .map(|c| ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, c as i32)))
                    .collect(),
            );
            ir::Expr::Ptr(ir::PtrExpr::DataRef(data_ref))
        }
        Expr::Var(var) => match var_symb
            .get_type_from_id(var)
            .expect("Expected a type for a variable")
        {
            ast::Type::Int | ast::Type::Char => ir::Expr::Num(ir::NumExpr::Var(var)),
            ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Var(var)),
            ast::Type::String | ast::Type::Pair(_, _) | ast::Type::Array(_, _) => {
                ir::Expr::Ptr(ir::PtrExpr::Var(var))
            }
            ast::Type::Generic(_) | ast::Type::Any => panic!("Expected a concrete type"),
        },

        Expr::ArrayElem(var, indices) => {
            let (fields_type, &num_indices) =
                if let ast::Type::Array(box fields_type, num_indices) = ast_expr_type {
                    (fields_type, num_indices)
                } else {
                    panic!("Expected an array expression");
                };
            assert_eq!(num_indices, indices.len());
            let ASTWrapper(last_index_type, last_index) = indices.remove(num_indices - 1);
            let last_index = translate_expr(
                last_index,
                &last_index_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            );

            let single_dim_ptr = indices
                .into_iter()
                .map(|ASTWrapper(sub_expr_type, sub_expr)| {
                    translate_expr(
                        sub_expr,
                        &sub_expr_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                    )
                })
                .fold(
                    ir::Expr::Ptr(ir::PtrExpr::Var(var)),
                    |data_ptr, sub_index| {
                        ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                            "a_index".to_string(),
                            vec![
                                data_ptr,
                                sub_index,
                                ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Ptr)),
                            ],
                        )))
                    },
                );
            match fields_type {
                ast::Type::Int => ir::Expr::Num(ir::NumExpr::Deref(
                    ir::NumSize::DWord,
                    ir::PtrExpr::Call(
                        "a_index".to_string(),
                        vec![
                            single_dim_ptr,
                            last_index,
                            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord))),
                        ],
                    ),
                )),
                ast::Type::Char => ir::Expr::Num(ir::NumExpr::Deref(
                    ir::NumSize::Byte,
                    ir::PtrExpr::Call(
                        "a_index".to_string(),
                        vec![
                            single_dim_ptr,
                            last_index,
                            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                        ],
                    ),
                )),
                ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Deref(ir::PtrExpr::Call(
                    "a_index".to_string(),
                    vec![
                        single_dim_ptr,
                        last_index,
                        ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Bool)),
                    ],
                ))),
                ast::Type::String | ast::Type::Pair(_, _) | ast::Type::Array(_, _) => {
                    ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                        "a_index".to_string(),
                        vec![
                            single_dim_ptr,
                            last_index,
                            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Ptr)),
                        ],
                    )))
                }
                ast::Type::Generic(_) | ast::Type::Any => panic!("Expected a concrete type"),
            }
        }

        Expr::UnOp(un_op, box ASTWrapper(sub_expr_type, sub_expr)) => {
            let sub_expr_type = sub_expr_type.expect("Expected a type for an expression");
            match (
                un_op,
                translate_expr(sub_expr, &sub_expr_type, var_symb, data_ref_map),
            ) {
                (UnOp::Neg, ir::Expr::Bool(bool_expr)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box bool_expr))
                }
                (UnOp::Minus, ir::Expr::Num(num)) => ir::Expr::Num(ir::NumExpr::ArithOp(
                    box num,
                    ir::ArithOp::Mul,
                    box ir::NumExpr::Const(ir::NumSize::DWord, -1),
                )),
                (UnOp::Chr, ir::Expr::Num(num)) => {
                    ir::Expr::Num(ir::NumExpr::Cast(ir::NumSize::Byte, box num))
                }
                (UnOp::Ord, ir::Expr::Num(num)) => {
                    ir::Expr::Num(ir::NumExpr::Cast(ir::NumSize::DWord, box num))
                }
                (UnOp::Len, ir::Expr::Ptr(ptr)) => {
                    ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::DWord, ptr))
                }
                (UnOp::Fst, ir::Expr::Ptr(ptr)) => match sub_expr_type {
                    ast::Type::Int => ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::DWord, ptr)),
                    ast::Type::Char => ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::Byte, ptr)),
                    ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Deref(ptr)),
                    ast::Type::Array(_, _) | ast::Type::Pair(_, _) | ast::Type::String => {
                        ir::Expr::Ptr(ir::PtrExpr::Deref(box ptr))
                    }
                    ast::Type::Generic(_) | ast::Type::Any => {
                        panic!("Expected a concrete type")
                    }
                },
                (UnOp::Snd, ir::Expr::Ptr(ptr)) => {
                    let offset_ptr = ir::PtrExpr::Offset(box ptr, box ir::NumExpr::SizeOfWideAlloc);
                    match sub_expr_type {
                        ast::Type::Int => {
                            ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::DWord, offset_ptr))
                        }
                        ast::Type::Char => {
                            ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::Byte, offset_ptr))
                        }
                        ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Deref(offset_ptr)),
                        ast::Type::Array(_, _) | ast::Type::Pair(_, _) | ast::Type::String => {
                            ir::Expr::Ptr(ir::PtrExpr::Deref(box offset_ptr))
                        }
                        ast::Type::Generic(_) | ast::Type::Any => {
                            panic!("Expected a concrete type")
                        }
                    }
                }
                _ => panic!("Incompatible operator application"),
            }
        }

        Expr::BinOp(
            box ASTWrapper(sub_expr_type1, sub_expr1),
            bin_op,
            box ASTWrapper(sub_expr_type2, sub_expr2),
        ) => {
            let sub_expr_type1 = sub_expr_type1.expect("Expected a type for an expression");
            let sub_expr_type2 = sub_expr_type2.expect("Expected a type for an expression");
            match (
                translate_expr(sub_expr1, &sub_expr_type1, var_symb, data_ref_map),
                bin_op,
                translate_expr(sub_expr2, &sub_expr_type2, var_symb, data_ref_map),
            ) {
                (ir::Expr::Num(num_expr1), ast::BinOp::Add, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Add,
                        box num_expr2,
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Sub, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Sub,
                        box num_expr2,
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Mul, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Mul,
                        box num_expr2,
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Div, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Div,
                        box num_expr2,
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Mod, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Mod,
                        box num_expr2,
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Gt, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::TestPositive(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Sub,
                        box num_expr2,
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Gte, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::TestPositive(
                        ir::NumExpr::ArithOp(box num_expr2, ir::ArithOp::Sub, box num_expr1),
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Lt, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::TestPositive(ir::NumExpr::ArithOp(
                        box num_expr2,
                        ir::ArithOp::Sub,
                        box num_expr1,
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Lte, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::TestPositive(
                        ir::NumExpr::ArithOp(box num_expr1, ir::ArithOp::Sub, box num_expr2),
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Eq, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::TestZero(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Sub,
                        box num_expr2,
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Ne, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::TestZero(
                        ir::NumExpr::ArithOp(box num_expr1, ir::ArithOp::Sub, box num_expr2),
                    )))
                }
                (ir::Expr::Bool(bool_expr1), ast::BinOp::And, ir::Expr::Bool(bool_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::BoolOp(
                        box bool_expr1,
                        ir::BoolOp::And,
                        box bool_expr2,
                    ))
                }
                (ir::Expr::Bool(bool_expr1), ast::BinOp::Or, ir::Expr::Bool(bool_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::BoolOp(
                        box bool_expr1,
                        ir::BoolOp::Or,
                        box bool_expr2,
                    ))
                }
                (ir::Expr::Bool(bool_expr1), ast::BinOp::Eq, ir::Expr::Bool(bool_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::BoolOp(
                        box bool_expr1,
                        ir::BoolOp::Xor,
                        box bool_expr2,
                    )))
                }
                (ir::Expr::Bool(bool_expr1), ast::BinOp::Ne, ir::Expr::Bool(bool_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::BoolOp(
                        box bool_expr1,
                        ir::BoolOp::Xor,
                        box bool_expr2,
                    ))
                }
                (expr1, ast::BinOp::Newpair, expr2) => {
                    ir::Expr::Ptr(ir::PtrExpr::WideMalloc(vec![expr1, expr2]))
                }
                _ => panic!("Incompatible operator application"),
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
        let translated: intermediate::Expr =
            ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::Const(true)));

        assert_eq!(
            translate_expr(
                Expr::UnOp(
                    UnOp::Neg,
                    box ASTWrapper(Some(ast::Type::Bool), Expr::Bool(true)),
                ),
                &Type::Bool,
                &VariableSymbolTable::new(),
                &mut HashMap::new()
            ),
            translated
        );

        let translated2: intermediate::Expr = ir::Expr::Bool(ir::BoolExpr::BoolOp(
            box ir::BoolExpr::TestZero(ir::NumExpr::ArithOp(
                box ir::NumExpr::Cast(
                    ir::NumSize::DWord,
                    box ir::NumExpr::Const(ir::NumSize::Byte, 'a' as i32),
                ),
                ir::ArithOp::Sub,
                box ir::NumExpr::Const(ir::NumSize::DWord, 65),
            )),
            ir::BoolOp::Or,
            box ir::BoolExpr::BoolOp(
                box ir::BoolExpr::Const(true),
                ir::BoolOp::And,
                box ir::BoolExpr::Not(box ir::BoolExpr::Const(false)),
            ),
        ));

        assert_eq!(
            translate_expr(
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
                &Type::Bool,
                &VariableSymbolTable::new(),
                &mut HashMap::new()
            ),
            translated2
        );
    }

    #[test]
    fn check_integer_expression() {
        let translated: intermediate::Expr = intermediate::Expr::Num(ir::NumExpr::ArithOp(
            box ir::NumExpr::Const(ir::NumSize::DWord, 3),
            ir::ArithOp::Add,
            box ir::NumExpr::Const(ir::NumSize::DWord, 7),
        ));

        assert_eq!(
            translate_expr(
                Expr::BinOp(
                    box ASTWrapper(Some(ast::Type::Int), Expr::Int(3)),
                    BinOp::Add,
                    box ASTWrapper(Some(ast::Type::Int), Expr::Int(7)),
                ),
                &Type::Int,
                &VariableSymbolTable::new(),
                &mut HashMap::new()
            ),
            translated
        );

        let mut var_table = VariableSymbolTable::new();
        var_table.0.insert(0, ast::Type::Int);

        let translated: intermediate::Expr = intermediate::Expr::Num(ir::NumExpr::ArithOp(
            box ir::NumExpr::ArithOp(
                box ir::NumExpr::Var(0),
                ir::ArithOp::Mod,
                box ir::NumExpr::Const(ir::NumSize::DWord, 7),
            ),
            ir::ArithOp::Mul,
            box ir::NumExpr::Cast(
                ir::NumSize::DWord,
                box ir::NumExpr::Const(ir::NumSize::Byte, 'a' as i32),
            ),
        ));

        assert_eq!(
            translate_expr(
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
                &Type::Int,
                &var_table,
                &mut HashMap::new()
            ),
            translated
        );
    }
}
