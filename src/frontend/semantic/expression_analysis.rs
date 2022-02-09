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
    WrapSpan(span, expr): WrapSpan<'a, Expr<'a, &'a str>>,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
    errors: &mut Vec<SemanticError<'a>>,
) -> Option<(Type, WrapSpan<'a, Expr<'a, usize>>)> {
    match expr {
        // Primitive expression checking (will always succeed)
        Expr::Null => Some((
            Type::Pair(box Type::Any, box Type::Any),
            WrapSpan(span, Expr::Null),
        )),
        Expr::Int(n) => Some((Type::Int, WrapSpan(span, Expr::Int(n)))),
        Expr::Bool(b) => Some((Type::Bool, WrapSpan(span, Expr::Bool(b)))),
        Expr::Char(c) => Some((Type::Char, WrapSpan(span, Expr::Char(c)))),
        Expr::String(s) => Some((Type::String, WrapSpan(span, Expr::String(s)))),

        // If variable defined, rename and return type, otherwise return undefined
        // error
        Expr::Var(name) => match var_symb.get_type(name, local_symb) {
            Some((rename, t)) => Some((t, WrapSpan(span, Expr::Var(rename)))),
            None => {
                errors.push(SemanticError::UndefinedVariableUse(name));
                None
            }
        },

        // An array indexing (e.g a[0][1]), checks all indexing expressions are
        // integers, reporting an errors in those expressions, and check both
        // that the variable is defined, and is an array deep enough to be
        // indexed.
        Expr::ArrayElem(name, indexes) => {
            let mut correct_indexes = Vec::new();
            let mut any_errors = false;
            let index_dim = indexes.len();

            for index_expr in indexes.into_iter() {
                match analyse_expression(index_expr, local_symb, var_symb, errors) {
                    Some((Type::Int, new_index_expr)) => correct_indexes.push(new_index_expr),
                    Some((_, WrapSpan(index_span, _))) => {
                        errors.push(SemanticError::InvalidIndex(index_span))
                    }
                    None => any_errors = true,
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
                        any_errors = true;
                        None
                    }
                },
                None => {
                    errors.push(SemanticError::UndefinedVariableUse(name));
                    None
                }
            };

            if let (false, Some((rename, t))) = (any_errors, symb) {
                Some((t, WrapSpan(span, Expr::ArrayElem(rename, correct_indexes))))
            } else {
                None
            }
        }

        // Unary operator checking, using the unop_matching functionality from
        // type_constraint.rs. If the internal contains errors, these are passed
        // upwards.
        Expr::UnOp(op, box inner_expr) => {
            match analyse_expression(inner_expr, local_symb, var_symb, errors) {
                Some((inner_t, WrapSpan(inner_span, ast))) => match unop_match(&op, &inner_t) {
                    Ok(unop_t) => Some((
                        unop_t,
                        WrapSpan(span, Expr::UnOp(op, box WrapSpan(inner_span, ast))),
                    )),
                    Err((pot_types, pot_ops)) => {
                        errors.push(SemanticError::InvalidUnOp(
                            span, pot_types, pot_ops, inner_t, op,
                        ));
                        None
                    }
                },
                None => None,
            }
        }

        // Binary operator checking, using the binop_matching functionality from
        // type_constraint.rs. If either side contains errors these are
        // passed forwards. Otherwise type checking occurs.
        Expr::BinOp(box left_expr, op, box right_expr) => {
            match (
                analyse_expression(left_expr, local_symb, var_symb, errors),
                analyse_expression(right_expr, local_symb, var_symb, errors),
            ) {
                (
                    Some((left_type, WrapSpan(left_span, left_ast))),
                    Some((right_type, WrapSpan(right_span, right_ast))),
                ) => match binop_match(&op, &left_type, &right_type) {
                    Ok(t) => Some((
                        t,
                        WrapSpan(
                            span,
                            Expr::BinOp(
                                box WrapSpan(left_span, left_ast),
                                op,
                                box WrapSpan(right_span, right_ast),
                            ),
                        ),
                    )),
                    Err((pot_types, pot_ops)) => {
                        errors.push(SemanticError::InvalidBinOp(
                            span,
                            pot_types,
                            pot_ops,
                            (left_type, right_type),
                            op,
                        ));
                        None
                    }
                },
                _ => None,
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

        let expr1: WrapSpan<Expr<&str>> = WrapSpan("9", Expr::Int(9));

        match analyse_expression(expr1, &local_symb, &var_symb, &mut vec![]) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan("'a'", Expr::Char('a'));

        match analyse_expression(expr2, &local_symb, &var_symb, &mut vec![]) {
            Some((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan("true", Expr::Bool(true));

        match analyse_expression(expr3, &local_symb, &var_symb, &mut vec![]) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "\"hello world\"",
            Expr::String(String::from("\"hello world\"")),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut vec![]) {
            Some((Type::String, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_detects_errors_in_basic_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan("x", Expr::Var("x"));
        let mut expr1_errors = Vec::new();

        match analyse_expression(expr1, &local_symb, &var_symb, &mut expr1_errors) {
            None => {
                assert!(expr1_errors.contains(&SemanticError::UndefinedVariableUse("x")));
            }
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "'a'",
            Expr::ArrayElem("x", vec![WrapSpan("'c'", Expr::Char('c'))]),
        );
        let mut expr2_errors = Vec::new();
        match analyse_expression(expr2, &local_symb, &var_symb, &mut expr2_errors) {
            None => {
                assert!(expr2_errors.contains(&SemanticError::UndefinedVariableUse("x")));
                assert!(expr2_errors.contains(&SemanticError::InvalidIndex("'c'")));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "-3",
            Expr::UnOp(UnOp::Minus, box WrapSpan("3", Expr::Int(3))),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "!true",
            Expr::UnOp(UnOp::Neg, box WrapSpan("true", Expr::Bool(true))),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "ord 'a'",
            Expr::UnOp(UnOp::Ord, box WrapSpan("'a'", Expr::Char('a'))),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "chr 97",
            Expr::UnOp(UnOp::Chr, box WrapSpan("97", Expr::Int(97))),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_chains_of_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "- ord chr 9",
            Expr::UnOp(
                UnOp::Minus,
                box WrapSpan(
                    "ord chr 9",
                    Expr::UnOp(
                        UnOp::Ord,
                        box WrapSpan(
                            "chr 9",
                            Expr::UnOp(UnOp::Chr, box WrapSpan("9", Expr::Int(9))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "chr ord chr 9",
            Expr::UnOp(
                UnOp::Chr,
                box WrapSpan(
                    "ord chr 9",
                    Expr::UnOp(
                        UnOp::Ord,
                        box WrapSpan(
                            "chr 9",
                            Expr::UnOp(UnOp::Chr, box WrapSpan("9", Expr::Int(9))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_constant_only_binary_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) >= (3 * -2)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        BinOp::Add,
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                ),
                BinOp::Gte,
                box WrapSpan(
                    "3 * -2",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        BinOp::Mul,
                        box WrapSpan(
                            "-2",
                            Expr::UnOp(UnOp::Minus, box WrapSpan("2", Expr::Int(2))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) + (3 * -2)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        BinOp::Mul,
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                ),
                BinOp::Add,
                box WrapSpan(
                    "3 * -2",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        BinOp::Mul,
                        box WrapSpan(
                            "-2",
                            Expr::UnOp(UnOp::Minus, box WrapSpan("2", Expr::Int(2))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "chr ((1 + 1) + (3 * -2))",
            Expr::UnOp(
                UnOp::Chr,
                box WrapSpan(
                    "(1 + 1) + (3 * -2)",
                    Expr::BinOp(
                        box WrapSpan(
                            "1 + 1",
                            Expr::BinOp(
                                box WrapSpan("1", Expr::Int(1)),
                                BinOp::Mul,
                                box WrapSpan("1", Expr::Int(1)),
                            ),
                        ),
                        BinOp::Add,
                        box WrapSpan(
                            "3 * -2",
                            Expr::BinOp(
                                box WrapSpan("3", Expr::Int(3)),
                                BinOp::Mul,
                                box WrapSpan(
                                    "-2",
                                    Expr::UnOp(UnOp::Minus, box WrapSpan("2", Expr::Int(2))),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
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

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr5: WrapSpan<Expr<&str>> = WrapSpan(
            "(97 >= ord 'a') && (true && !false)",
            Expr::BinOp(
                box WrapSpan(
                    "97 >= ord 'a'",
                    Expr::BinOp(
                        box WrapSpan("97", Expr::Int(97)),
                        BinOp::Eq,
                        box WrapSpan(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box WrapSpan("'a''", Expr::Char('a'))),
                        ),
                    ),
                ),
                BinOp::And,
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

        match analyse_expression(expr5, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_variables() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb
            .def_var("var1", &Type::Int, "int var1 = 9", &mut local_symb)
            .expect("variable not yet defined");

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "(chr var1 >= ord 'a') || (true && !false)",
            Expr::BinOp(
                box WrapSpan(
                    "chr var1 >= ord 'a'",
                    Expr::BinOp(
                        box WrapSpan("var1", Expr::Var("var1")),
                        BinOp::Eq,
                        box WrapSpan(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box WrapSpan("'a''", Expr::Char('a'))),
                        ),
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

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var("var2", &Type::Bool, "bool var2 = true", &mut local_symb)
            .expect("variable not yet defined");

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "(ord 'a' == - ord chr 9) || (true && !false)",
            Expr::BinOp(
                box WrapSpan(
                    "ord 'a' == - ord chr 9",
                    Expr::BinOp(
                        box WrapSpan(
                            "- ord chr 9",
                            Expr::UnOp(
                                UnOp::Minus,
                                box WrapSpan(
                                    "ord chr 'a'",
                                    Expr::UnOp(
                                        UnOp::Ord,
                                        box WrapSpan(
                                            "chr 'a'",
                                            Expr::UnOp(UnOp::Chr, box WrapSpan("9", Expr::Int(9))),
                                        ),
                                    ),
                                ),
                            ),
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

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
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

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "array1[5]",
            Expr::ArrayElem("array1", vec![WrapSpan("5", Expr::Int(5))]),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var(
                "array2",
                &Type::Array(box Type::Int, 2),
                "int[][] array1 = [a,b,c]",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5 * 5]",
            Expr::ArrayElem(
                "array2",
                vec![WrapSpan(
                    "5 * 5",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        BinOp::Mul,
                        box WrapSpan("5", Expr::Int(5)),
                    ),
                )],
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Array(box Type::Int, 1), _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var("num1", &Type::Int, "int num1 = 9", &mut local_symb)
            .expect("variable not yet defined");

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5 * 5][num1]",
            Expr::ArrayElem(
                "array2",
                vec![
                    WrapSpan(
                        "5 * 5",
                        Expr::BinOp(
                            box WrapSpan("5", Expr::Int(5)),
                            BinOp::Mul,
                            box WrapSpan("5", Expr::Int(5)),
                        ),
                    ),
                    WrapSpan("num1", Expr::Var("num1")),
                ],
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5*5][array1[num1]]",
            Expr::ArrayElem(
                "array2",
                vec![
                    WrapSpan(
                        "5 * 5",
                        Expr::BinOp(
                            box WrapSpan("5", Expr::Int(5)),
                            BinOp::Mul,
                            box WrapSpan("5", Expr::Int(5)),
                        ),
                    ),
                    WrapSpan(
                        "array1[num1]",
                        Expr::ArrayElem("array1", vec![WrapSpan("num1", Expr::Var("num1"))]),
                    ),
                ],
            ),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Int, _)) => assert!(true),
            _ => assert!(false),
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

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "pair1 == pair1",
            Expr::BinOp(
                box WrapSpan("pair1", Expr::Var("pair1")),
                BinOp::Eq,
                box WrapSpan("pair1", Expr::Var("pair1")),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var(
                "pair_array",
                &Type::Array(box Type::Pair(box Type::Int, box Type::Int), 1),
                "pair(int, int)[] pair_array = [pair1]",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "pair_array[len pair_array - 1]",
            Expr::ArrayElem(
                "pair_array",
                vec![WrapSpan(
                    "len pair_array - 1",
                    Expr::BinOp(
                        box WrapSpan(
                            "len pair_array",
                            Expr::UnOp(
                                UnOp::Len,
                                box WrapSpan("pair_array", Expr::Var("pair_array")),
                            ),
                        ),
                        BinOp::Sub,
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                )],
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Pair(box Type::Int, box Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "newpair(5+5, newpair(5+5, 'a')))",
            Expr::BinOp(
                box WrapSpan(
                    "5 + 5",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        BinOp::Add,
                        box WrapSpan("5", Expr::Int(5)),
                    ),
                ),
                BinOp::Newpair,
                box WrapSpan(
                    "newpair(5+5, 'a')",
                    Expr::BinOp(
                        box WrapSpan(
                            "5 + 5",
                            Expr::BinOp(
                                box WrapSpan("5", Expr::Int(5)),
                                BinOp::Add,
                                box WrapSpan("5", Expr::Int(5)),
                            ),
                        ),
                        BinOp::Newpair,
                        box WrapSpan("'a'", Expr::Char('a')),
                    ),
                ),
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::Char)), _)) => {
                assert!(true)
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_correctly_matches_fst_and_snd() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb
            .def_var(
                "pair1",
                &Type::Pair(box Type::Int, box Type::Array(box Type::Bool, 2)),
                "pair(int, bool[][]) pair1 = newpair(9, array1)",
                &mut local_symb,
            )
            .expect("variable not yet defined");

        // example of newpair(snd pair1, fst pair1) <- swapping pair elements
        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "newpair(snd pair1, fst pair2)",
            Expr::BinOp(
                box WrapSpan(
                    "snd pair1",
                    Expr::UnOp(UnOp::Snd, box WrapSpan("pair1", Expr::Var("pair1"))),
                ),
                BinOp::Newpair,
                box WrapSpan(
                    "fst pair1",
                    Expr::UnOp(UnOp::Fst, box WrapSpan("pair1", Expr::Var("pair1"))),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some((Type::Pair(box Type::Array(box Type::Bool, 2), box Type::Int), _)) => {
                assert!(true)
            }
            _ => assert!(false),
        }
    }
}
