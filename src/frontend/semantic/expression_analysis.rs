//! Expression analysis 
//! 
//! Analyses WACC expressions
//! 
//! Analyse expressions expressions, performing variable reference renaming and 
//! determining the type of the expression for use in further analysis 
//! (e.g statements). 
//! 
//! Extensively pushes forwards with finding errors, potentially from every 
//! leaf node of an expression tree.
use super::{
    super::ast::{Expr, Type, WrapSpan},
    semantic_errors::SemanticError,
    symbol_table::{LocalSymbolTable, VariableSymbolTable},
    type_constraints::{binop_match, de_index, unop_match},
};


/// Recursively analyse a given expression:
/// - If the expression is valid, return the renamed ast and the type of the 
///   expression.
/// - If the expression is invalid, a collection of semantic errors are returned
///   (these may be from inner expressions)
pub fn analyse_expression<'a, 'b>(
    expr: Expr<'a, &'a str>,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
) -> Result<(Type, Expr<'a, usize>), Vec<SemanticError<'a>>> {
    match expr {

        // Primitive expression checking (will always succeed)
        Expr::Null => Ok((Type::Pair(box Type::Any, box Type::Any), Expr::Null)),
        Expr::Int(n) => Ok((Type::Int, Expr::Int(n))),
        Expr::Bool(b) => Ok((Type::Bool, Expr::Bool(b))),
        Expr::Char(c) => Ok((Type::Char, Expr::Char(c))),
        Expr::String(s) => Ok((Type::String, Expr::String(s))),

        // If variable defined, rename and return type, otherwise return undefined 
        // error
        Expr::Var(name) => match var_symb.get_type(name, local_symb) {
            Some((rename, t)) => Ok((t, Expr::Var(rename))),
            None => Err(vec![SemanticError::UndefinedVariableUse(name)]),
        },

        // An array indexing (e.g a[0][1]), checks all indexing expressions are 
        // integers, reporting an errors in those expressions, and check both 
        // that the variable is defined, and is an array deep enough to be 
        // indexed.
        Expr::ArrayElem(name, indexes) => {
            let mut correct_indexes = Vec::new();
            let mut errors = Vec::new();
            let index_dim = indexes.len();

            for WrapSpan(span, index_expr) in indexes.into_iter() {
                match analyse_expression(index_expr, local_symb, var_symb) {
                    Ok((Type::Int, new_index_expr)) => {
                        correct_indexes.push(WrapSpan(span, new_index_expr))
                    }
                    Ok((_, _)) => errors.push(SemanticError::InvalidIndex(span)),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }

            let symb = match var_symb.get_type(name, local_symb) {
                Some((rename, t)) => match de_index(&t, index_dim) {
                    Some(de_ind_t) => Some((rename, de_ind_t)),
                    None => {
                        errors.push(SemanticError::InvalidVariableType(
                            name,
                            Type::Array(box Type::Any, index_dim),
                            t,
                        ));
                        None
                    }
                },
                None => {
                    errors.push(SemanticError::UndefinedVariableUse(name));
                    None
                }
            };

            if let (0, Some((rename, t))) = (errors.len(), symb) {
                Ok((t, Expr::ArrayElem(rename, correct_indexes)))
            } else {
                Err(errors)
            }
        }

        // Unary operator checking, using the unop_matching functionality from 
        // type_constraint.rs. If the internal contains errors, these are passed 
        // upwards.
        Expr::UnOp(WrapSpan(op_span, op), box WrapSpan(inner_span, inner_expr)) => {
            match analyse_expression(inner_expr, local_symb, var_symb) {
                Ok((inner_t, ast)) => match unop_match(&op, &inner_t) {
                    Ok(unop_t) => Ok((
                        unop_t,
                        Expr::UnOp(WrapSpan(op_span, op), box WrapSpan(inner_span, ast)),
                    )),
                    Err((pot_types, pot_ops)) => Err(vec![SemanticError::InvalidUnOp(
                        op_span, inner_span, pot_types, pot_ops, inner_t, op,
                    )]),
                },
                Err(errs) => Err(errs),
            }
        }

        // Binary operator checking, using the binop_matching functionality from 
        // type_constraint.rs. If either side contains errors these are 
        // passed forwards. Otherwise type checking occurs.
        Expr::BinOp(
            box WrapSpan(left_span, left_expr),
            WrapSpan(op_span, op),
            box WrapSpan(right_span, right_expr),
        ) => {
            match (
                analyse_expression(left_expr, local_symb, var_symb),
                analyse_expression(right_expr, local_symb, var_symb),
            ) {
                (Ok((left_type, left_ast)), Ok((right_type, right_ast))) => {
                    match binop_match(&op, &left_type, &right_type) {
                        Ok(t) => Ok((
                            t,
                            Expr::BinOp(
                                box WrapSpan(left_span, left_ast),
                                WrapSpan(op_span, op),
                                box WrapSpan(right_span, right_ast),
                            ),
                        )),
                        Err((pot_types, pot_ops)) => Err(vec![SemanticError::InvalidBinOp(
                            left_span,
                            op_span,
                            right_span,
                            pot_types,
                            pot_ops,
                            (left_type, right_type),
                            op,
                        )]),
                    }
                }
                (Ok(_), Err(errs)) => Err(errs),
                (Err(errs), Ok(_)) => Err(errs),
                (Err(mut errs1), Err(mut errs2)) => {
                    errs1.append(&mut errs2);
                    Err(errs1)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::ast::{BinOp, UnOp};
    use super::*;

    #[test]
    fn analyse_expression_matches_primitive_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: Expr<&str> = Expr::Int(9);

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: Expr<&str> = Expr::Char('a');

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: Expr<&str> = Expr::Bool(true);

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: Expr<&str> = Expr::String(String::from("\"hello world\""));

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::String, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4, &local_symb, &var_symb) {
            Ok((Type::String, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_detects_errors_in_primitive_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: Expr<&str> = Expr::Int(9);

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: Expr<&str> = Expr::Char('a');

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: Expr<&str> = Expr::Bool(true);

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: Expr<&str> = Expr::String(String::from("\"hello world\""));

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(expr4, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: Expr<&str> =
            Expr::UnOp(WrapSpan("-", UnOp::Minus), box WrapSpan("3", Expr::Int(3)));

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: Expr<&str> = Expr::UnOp(
            WrapSpan("!", UnOp::Neg),
            box WrapSpan("true", Expr::Bool(true)),
        );

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: Expr<&str> = Expr::UnOp(
            WrapSpan("ord", UnOp::Ord),
            box WrapSpan("'a'", Expr::Char('a')),
        );

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: Expr<&str> = Expr::UnOp(
            WrapSpan("chr", UnOp::Chr),
            box WrapSpan("97", Expr::Int(97)),
        );

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_chains_of_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: Expr<&str> = Expr::UnOp(
            WrapSpan("-", UnOp::Minus),
            box WrapSpan(
                "ord chr 'a'",
                Expr::UnOp(
                    WrapSpan("ord", UnOp::Ord),
                    box WrapSpan(
                        "chr 'a'",
                        Expr::UnOp(WrapSpan("chr", UnOp::Chr), box WrapSpan("9", Expr::Int(9))),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: Expr<&str> = Expr::UnOp(
            WrapSpan("chr", UnOp::Chr),
            box WrapSpan(
                "ord chr 'a'",
                Expr::UnOp(
                    WrapSpan("ord", UnOp::Ord),
                    box WrapSpan(
                        "chr 'a'",
                        Expr::UnOp(WrapSpan("chr", UnOp::Chr), box WrapSpan("9", Expr::Int(9))),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_constant_only_binary_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "1 + 1",
                Expr::BinOp(
                    box WrapSpan("1", Expr::Int(1)),
                    WrapSpan("+", BinOp::Add),
                    box WrapSpan("1", Expr::Int(1)),
                ),
            ),
            WrapSpan(">=", BinOp::Gte),
            box WrapSpan(
                "3 * -2",
                Expr::BinOp(
                    box WrapSpan("3", Expr::Int(3)),
                    WrapSpan("*", BinOp::Mul),
                    box WrapSpan(
                        "-2",
                        Expr::UnOp(WrapSpan("-", UnOp::Minus), box WrapSpan("2", Expr::Int(2))),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "1 + 1",
                Expr::BinOp(
                    box WrapSpan("1", Expr::Int(1)),
                    WrapSpan("*", BinOp::Mul),
                    box WrapSpan("1", Expr::Int(1)),
                ),
            ),
            WrapSpan(">=", BinOp::Add),
            box WrapSpan(
                "3 * -2",
                Expr::BinOp(
                    box WrapSpan("3", Expr::Int(3)),
                    WrapSpan("*", BinOp::Mul),
                    box WrapSpan(
                        "-2",
                        Expr::UnOp(WrapSpan("-", UnOp::Minus), box WrapSpan("2", Expr::Int(2))),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "1 + 1",
                Expr::BinOp(
                    box WrapSpan("1", Expr::Int(1)),
                    WrapSpan("+", BinOp::Add),
                    box WrapSpan("1", Expr::Int(1)),
                ),
            ),
            WrapSpan("+", BinOp::Add),
            box WrapSpan(
                "3 * -2",
                Expr::BinOp(
                    box WrapSpan("3", Expr::Int(3)),
                    WrapSpan("*", BinOp::Mul),
                    box WrapSpan(
                        "-2",
                        Expr::UnOp(WrapSpan("-", UnOp::Minus), box WrapSpan("2", Expr::Int(2))),
                    ),
                ),
            ),
        );

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "ord 'a' == 65",
                Expr::BinOp(
                    box WrapSpan(
                        "ord 'a'",
                        Expr::UnOp(
                            WrapSpan("ord", UnOp::Ord),
                            box WrapSpan("'a'", Expr::Char('a')),
                        ),
                    ),
                    WrapSpan("==", BinOp::Eq),
                    box WrapSpan("65", Expr::Int(65)),
                ),
            ),
            WrapSpan("||", BinOp::Or),
            box WrapSpan(
                "true && !false",
                Expr::BinOp(
                    box WrapSpan("true", Expr::Bool(true)),
                    WrapSpan("&&", BinOp::And),
                    box WrapSpan(
                        "!false",
                        Expr::UnOp(
                            WrapSpan("!", UnOp::Neg),
                            box WrapSpan("false", Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr5: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "97 >= ord 'a'",
                Expr::BinOp(
                    box WrapSpan("97", Expr::Int(97)),
                    WrapSpan("==", BinOp::Eq),
                    box WrapSpan(
                        "ord 'a'",
                        Expr::UnOp(
                            WrapSpan("ord", UnOp::Ord),
                            box WrapSpan("'a''", Expr::Char('a')),
                        ),
                    ),
                ),
            ),
            WrapSpan("||", BinOp::Or),
            box WrapSpan(
                "true && !false",
                Expr::BinOp(
                    box WrapSpan("true", Expr::Bool(true)),
                    WrapSpan("&&", BinOp::And),
                    box WrapSpan(
                        "!false",
                        Expr::UnOp(
                            WrapSpan("!", UnOp::Neg),
                            box WrapSpan("false", Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr5.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr5.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr5, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_variables() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb
            .def_var("var1", &Type::Int, "int var1 = 9", &mut local_symb)
            .expect("variable not yet defined");

        let expr1: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "chr var1 >= ord 'a'",
                Expr::BinOp(
                    box WrapSpan("var1", Expr::Var("var1")),
                    WrapSpan("==", BinOp::Eq),
                    box WrapSpan(
                        "ord 'a'",
                        Expr::UnOp(
                            WrapSpan("ord", UnOp::Ord),
                            box WrapSpan("'a''", Expr::Char('a')),
                        ),
                    ),
                ),
            ),
            WrapSpan("||", BinOp::Or),
            box WrapSpan(
                "true && !false",
                Expr::BinOp(
                    box WrapSpan("true", Expr::Bool(true)),
                    WrapSpan("&&", BinOp::And),
                    box WrapSpan(
                        "!false",
                        Expr::UnOp(
                            WrapSpan("!", UnOp::Neg),
                            box WrapSpan("false", Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        println!(
            "{:?}",
            analyse_expression(expr1.clone(), &local_symb, &var_symb)
        );

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb
            .def_var("var2", &Type::Bool, "bool var2 = true", &mut local_symb)
            .expect("variable not yet defined");

        let expr4: Expr<&str> = Expr::BinOp(
            box WrapSpan(
                "ord 'a' == 65",
                Expr::BinOp(
                    box WrapSpan(
                        "ord 'a'",
                        Expr::UnOp(
                            WrapSpan("ord", UnOp::Ord),
                            box WrapSpan("'a'", Expr::Char('a')),
                        ),
                    ),
                    WrapSpan("==", BinOp::Eq),
                    box WrapSpan("65", Expr::Int(65)),
                ),
            ),
            WrapSpan("||", BinOp::Or),
            box WrapSpan(
                "true && !false",
                Expr::BinOp(
                    box WrapSpan("true", Expr::Bool(true)),
                    WrapSpan("&&", BinOp::And),
                    box WrapSpan(
                        "!false",
                        Expr::UnOp(
                            WrapSpan("!", UnOp::Neg),
                            box WrapSpan("false", Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr4, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_arrays() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb
            .def_var(
                "array1",
                &Type::Array(box Type::Int, 1),
                "int[] array1 = [1,2,3,4]",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr1: Expr<&str> = Expr::ArrayElem("array1", vec![WrapSpan("5", Expr::Int(5))]);

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb
            .def_var(
                "array2",
                &Type::Array(box Type::Int, 2),
                "int[][] array1 = [a,b,c]",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr2: Expr<&str> = Expr::ArrayElem(
            "array2",
            vec![WrapSpan(
                "5 * 5",
                Expr::BinOp(
                    box WrapSpan("5", Expr::Int(5)),
                    WrapSpan("*", BinOp::Mul),
                    box WrapSpan("5", Expr::Int(5)),
                ),
            )],
        );

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Array(box Type::Int, 1), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Array(box Type::Int, 1), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb
            .def_var("num1", &Type::Int, "int num1 = 9", &mut local_symb)
            .expect("variable not yet defined");

        let expr3: Expr<&str> = Expr::ArrayElem(
            "array2",
            vec![
                WrapSpan(
                    "5 * 5",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("5", Expr::Int(5)),
                    ),
                ),
                WrapSpan("num1", Expr::Var("num1")),
            ],
        );

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: Expr<&str> = Expr::ArrayElem(
            "array2",
            vec![
                WrapSpan(
                    "5 * 5",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("5", Expr::Int(5)),
                    ),
                ),
                WrapSpan(
                    "array1[num1]",
                    Expr::ArrayElem("array1", vec![WrapSpan("num1", Expr::Var("num1"))]),
                ),
            ],
        );

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3.clone(), &local_symb, &var_symb) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr3, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_pairs() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb
            .def_var(
                "pair1",
                &Type::Pair(box Type::Int, box Type::Int),
                "pair(int, int) pair1 = newpair(9,9)",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr1: Expr<&str> = Expr::BinOp(
            box WrapSpan("pair1", Expr::Var("pair1")),
            WrapSpan("==", BinOp::Eq),
            box WrapSpan("pair1", Expr::Var("pair1")),
        );

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1.clone(), &local_symb, &var_symb) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr1, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb
            .def_var(
                "pair_array",
                &Type::Array(box Type::Pair(box Type::Int, box Type::Int), 1),
                "pair(int, int)[] pair_array = [pair1]",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr2: Expr<&str> = Expr::ArrayElem(
            "pair_array",
            vec![WrapSpan(
                "len pair_array - 1",
                Expr::BinOp(
                    box WrapSpan(
                        "len pair_array",
                        Expr::UnOp(
                            WrapSpan("len", UnOp::Len),
                            box WrapSpan("pair_array", Expr::Var("pair_array")),
                        ),
                    ),
                    WrapSpan("-", BinOp::Sub),
                    box WrapSpan("1", Expr::Int(1)),
                ),
            )],
        );

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Pair(box Type::Int, box Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2.clone(), &local_symb, &var_symb) {
            Ok((Type::Pair(box Type::Int, box Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(expr2, &local_symb, &var_symb) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }
}
