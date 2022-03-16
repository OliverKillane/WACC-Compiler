use std::collections::HashMap;
use std::sync::RwLock;

use super::super::ast::{self, ASTWrapper, Expr, Type, UnOp};
use super::helper_funcs::*;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;
use crate::intermediate::{self as ir, DataRef};

/// Adds a string [data reference](DataRef) to the data reference map.
/// Returns the data reference to the newly added string.
pub(super) fn add_string(
    data_ref_map: &RwLock<HashMap<DataRef, Vec<ir::Expr>>>,
    string: String,
) -> DataRef {
    let mut data_ref_map = data_ref_map.write().expect("Cannot obtain lock");
    let data_ref: u64 = data_ref_map.len() as u64;
    data_ref_map.insert(
        data_ref,
        vec![ir::Expr::Num(ir::NumExpr::Const(
            ir::NumSize::DWord,
            string.len() as i32,
        ))]
        .into_iter()
        .chain(
            string
                .into_bytes()
                .into_iter()
                .map(|c| ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, c as i32))),
        )
        .collect(),
    );
    data_ref
}

/// Translates a single expression. The arguments are as follows:
///  - The expression to be translated.
///  - The cached type of the expression to be translated.
///  - Symbol table for this particular expression.
///  - The return types of all the functions.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
pub(super) fn translate_expr(
    ast_expr: Expr<Option<Type>, usize>,
    ast_expr_type: &Type,
    var_symb: &VariableSymbolTable,
    data_ref_map: &RwLock<HashMap<DataRef, Vec<ir::Expr>>>,
    helper_function_flags: &RwLock<HelperFunctionFlags>,
) -> ir::Expr {
    match ast_expr {
        Expr::Null => ir::Expr::Ptr(ir::PtrExpr::Null),
        Expr::Int(int_const) => ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, int_const)),
        Expr::Bool(bool_const) => ir::Expr::Bool(ir::BoolExpr::Const(bool_const)),
        Expr::Char(char_const) => {
            ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, char_const as i32))
        }
        Expr::String(str_const) => {
            ir::Expr::Ptr(ir::PtrExpr::DataRef(add_string(data_ref_map, str_const)))
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

        Expr::ArrayElem(var, mut indices) => {
            helper_function_flags
                .write()
                .expect("Cannot obtain lock")
                .array_indexing = true;
            let ASTWrapper(last_index_type, last_index) = indices.remove(indices.len() - 1);
            let last_index = translate_expr(
                last_index,
                &last_index_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
                helper_function_flags,
            );

            let single_dim_ptr = indices
                .into_iter()
                .map(|ASTWrapper(sub_expr_type, sub_expr)| {
                    translate_expr(
                        sub_expr,
                        &sub_expr_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                        helper_function_flags,
                    )
                })
                .fold(
                    ir::Expr::Ptr(ir::PtrExpr::Var(var)),
                    |data_ptr, sub_index| {
                        ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                            ARRAY_INDEX_FNAME.to_string(),
                            vec![
                                data_ptr,
                                sub_index,
                                ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Ptr)),
                            ],
                        )))
                    },
                );
            match ast_expr_type {
                ast::Type::Int => ir::Expr::Num(ir::NumExpr::Deref(
                    ir::NumSize::DWord,
                    ir::PtrExpr::Call(
                        ARRAY_INDEX_FNAME.to_string(),
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
                        ARRAY_INDEX_FNAME.to_string(),
                        vec![
                            single_dim_ptr,
                            last_index,
                            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                        ],
                    ),
                )),
                ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Deref(ir::PtrExpr::Call(
                    ARRAY_INDEX_FNAME.to_string(),
                    vec![
                        single_dim_ptr,
                        last_index,
                        ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Bool)),
                    ],
                ))),
                ast::Type::String | ast::Type::Pair(_, _) | ast::Type::Array(_, _) => {
                    ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                        ARRAY_INDEX_FNAME.to_string(),
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
            match (
                un_op,
                translate_expr(
                    sub_expr,
                    &sub_expr_type.expect("Expected a type for an expression"),
                    var_symb,
                    data_ref_map,
                    helper_function_flags,
                ),
            ) {
                (UnOp::Neg, ir::Expr::Bool(bool_expr)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box bool_expr))
                }
                (UnOp::Minus, ir::Expr::Num(num)) => ir::Expr::Num(ir::NumExpr::ArithOp(
                    box ir::NumExpr::Const(ir::NumSize::DWord, 0),
                    ir::ArithOp::Sub,
                    box num,
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
                (UnOp::Fst, expr @ ir::Expr::Ptr(_)) => {
                    helper_function_flags
                        .write()
                        .expect("Cannot obtain lock")
                        .check_null = true;
                    let ptr = ir::PtrExpr::Call(CHECK_NULL_FNAME.to_string(), vec![expr]);
                    match ast_expr_type {
                        ast::Type::Int => {
                            ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::DWord, ptr))
                        }
                        ast::Type::Char => {
                            ir::Expr::Num(ir::NumExpr::Deref(ir::NumSize::Byte, ptr))
                        }
                        ast::Type::Bool => ir::Expr::Bool(ir::BoolExpr::Deref(ptr)),
                        ast::Type::Array(_, _) | ast::Type::Pair(_, _) | ast::Type::String => {
                            ir::Expr::Ptr(ir::PtrExpr::Deref(box ptr))
                        }
                        ast::Type::Generic(_) | ast::Type::Any => {
                            panic!("Expected a concrete type")
                        }
                    }
                }
                (UnOp::Snd, expr @ ir::Expr::Ptr(_)) => {
                    helper_function_flags
                        .write()
                        .expect("Cannot obtain lock")
                        .check_null = true;
                    let offset_ptr = ir::PtrExpr::Offset(
                        box ir::PtrExpr::Call(CHECK_NULL_FNAME.to_string(), vec![expr]),
                        box ir::NumExpr::SizeOfWideAlloc,
                    );
                    match ast_expr_type {
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
                translate_expr(
                    sub_expr1,
                    &sub_expr_type1,
                    var_symb,
                    data_ref_map,
                    helper_function_flags,
                ),
                bin_op,
                translate_expr(
                    sub_expr2,
                    &sub_expr_type2,
                    var_symb,
                    data_ref_map,
                    helper_function_flags,
                ),
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
                    helper_function_flags
                        .write()
                        .expect("Cannot obtain lock")
                        .divide_modulo_check = true;
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Div,
                        box ir::NumExpr::Call(
                            ZERO_CHECK_FNAME.to_string(),
                            vec![ir::Expr::Num(num_expr2)],
                        ),
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Mod, ir::Expr::Num(num_expr2)) => {
                    helper_function_flags
                        .write()
                        .expect("Cannot obtain lock")
                        .divide_modulo_check = true;
                    ir::Expr::Num(ir::NumExpr::ArithOp(
                        box num_expr1,
                        ir::ArithOp::Mod,
                        box ir::NumExpr::Call(
                            ZERO_CHECK_FNAME.to_string(),
                            vec![ir::Expr::Num(num_expr2)],
                        ),
                    ))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Gt, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::NumLt(num_expr2, num_expr1))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Gte, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::NumLt(
                        num_expr1, num_expr2,
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Lt, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::NumLt(num_expr1, num_expr2))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Lte, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::NumLt(
                        num_expr2, num_expr1,
                    )))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Eq, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::NumEq(num_expr1, num_expr2))
                }
                (ir::Expr::Num(num_expr1), ast::BinOp::Ne, ir::Expr::Num(num_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::NumEq(
                        num_expr1, num_expr2,
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
                (ir::Expr::Ptr(ptr_expr1), ast::BinOp::Eq, ir::Expr::Ptr(ptr_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2))
                }
                (ir::Expr::Ptr(ptr_expr1), ast::BinOp::Ne, ir::Expr::Ptr(ptr_expr2)) => {
                    ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::PtrEq(
                        ptr_expr1, ptr_expr2,
                    )))
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
    fn check_simple_boolean_unop_expression() {
        let helper_function_flags = RwLock::new(HelperFunctionFlags::default());
        let data_ref_map: RwLock<HashMap<DataRef, Vec<ir::Expr>>> = RwLock::new(HashMap::new());
        let var_symb = VariableSymbolTable::new();

        let translated: intermediate::Expr =
            ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::Const(true)));

        assert_eq!(
            translated,
            translate_expr(
                Expr::UnOp(
                    UnOp::Neg,
                    box ASTWrapper(Some(ast::Type::Bool), Expr::Bool(true)),
                ),
                &Type::Bool,
                &var_symb,
                &data_ref_map,
                &helper_function_flags
            )
        );
    }

    #[test]
    fn check_complex_boolean_binop_expression() {
        let helper_function_flags: RwLock<HelperFunctionFlags> =
            RwLock::new(HelperFunctionFlags::default());
        let data_ref_map: RwLock<HashMap<DataRef, Vec<ir::Expr>>> = RwLock::new(HashMap::new());
        let var_symb = VariableSymbolTable::new();

        let translated: intermediate::Expr = ir::Expr::Bool(ir::BoolExpr::BoolOp(
            box ir::BoolExpr::NumEq(
                ir::NumExpr::Cast(
                    ir::NumSize::DWord,
                    box ir::NumExpr::Const(ir::NumSize::Byte, 'a' as i32),
                ),
                ir::NumExpr::Const(ir::NumSize::DWord, 65),
            ),
            ir::BoolOp::Or,
            box ir::BoolExpr::BoolOp(
                box ir::BoolExpr::Const(true),
                ir::BoolOp::And,
                box ir::BoolExpr::Not(box ir::BoolExpr::Const(false)),
            ),
        ));

        assert_eq!(
            translated,
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
                &var_symb,
                &data_ref_map,
                &helper_function_flags
            )
        );
    }

    #[test]
    fn check_simple_integer_binop_expression() {
        let helper_function_flags: RwLock<HelperFunctionFlags> =
            RwLock::new(HelperFunctionFlags::default());
        let data_ref_map: RwLock<HashMap<DataRef, Vec<ir::Expr>>> = RwLock::new(HashMap::new());
        let var_symb = VariableSymbolTable::new();

        let translated: intermediate::Expr = intermediate::Expr::Num(ir::NumExpr::ArithOp(
            box ir::NumExpr::Const(ir::NumSize::DWord, 3),
            ir::ArithOp::Add,
            box ir::NumExpr::Const(ir::NumSize::DWord, 7),
        ));

        assert_eq!(
            translated,
            translate_expr(
                Expr::BinOp(
                    box ASTWrapper(Some(ast::Type::Int), Expr::Int(3)),
                    BinOp::Add,
                    box ASTWrapper(Some(ast::Type::Int), Expr::Int(7)),
                ),
                &Type::Int,
                &var_symb,
                &data_ref_map,
                &helper_function_flags
            )
        );
    }

    #[test]
    fn check_complex_integer_binop_expression() {
        let helper_function_flags: RwLock<HelperFunctionFlags> =
            RwLock::new(HelperFunctionFlags::default());
        let data_ref_map: RwLock<HashMap<DataRef, Vec<ir::Expr>>> = RwLock::new(HashMap::new());
        let mut var_symb = VariableSymbolTable::new();

        var_symb.0.insert(0, ast::Type::Int);

        let translated: intermediate::Expr = intermediate::Expr::Num(ir::NumExpr::ArithOp(
            box ir::NumExpr::ArithOp(
                box ir::NumExpr::Var(0),
                ir::ArithOp::Mod,
                box ir::NumExpr::Call(
                    ZERO_CHECK_FNAME.to_string(),
                    vec![ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 7))],
                ),
            ),
            ir::ArithOp::Mul,
            box ir::NumExpr::Cast(
                ir::NumSize::DWord,
                box ir::NumExpr::Const(ir::NumSize::Byte, 'a' as i32),
            ),
        ));

        assert_eq!(
            translated,
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
                &var_symb,
                &data_ref_map,
                &helper_function_flags
            )
        );
    }
}
