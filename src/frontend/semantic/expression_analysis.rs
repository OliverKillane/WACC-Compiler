//! Analyse WACC expressions, renaming variable uses.
//!
//! Analyse expressions, performing variable reference renaming and
//! determining the type of the expression for use in further analysis
//! (e.g statements).
//!
//! Extensively pushes forwards with finding errors, potentially from every
//! leaf node of an expression tree.
use super::{
    super::ast::{Expr, Type, UnOp, ASTWrapper, ExprSpan},
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
    ASTWrapper(span, expr): ExprSpan<&'a str, &'a str>,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
    errors: &mut Vec<SemanticError<'a>>,
) -> Option<ExprSpan<Option<Type>, usize>> {
    match expr {
        // Primitive expression checking (will always succeed)
        Expr::Null => Some(
            ASTWrapper(Some(Type::Pair(box Type::Any, box Type::Any)), Expr::Null),
        ),
        Expr::Int(n) => Some(ASTWrapper(Some(Type::Int), Expr::Int(n))),
        Expr::Bool(b) => Some(ASTWrapper(Some(Type::Bool), Expr::Bool(b))),
        Expr::Char(c) => Some(ASTWrapper(Some(Type::Char), Expr::Char(c))),
        Expr::String(s) => Some(ASTWrapper(Some(Type::String), Expr::String(s))),

        // If variable defined, rename and return type, otherwise return undefined
        // error
        Expr::Var(name) => match var_symb.get_type(name, local_symb) {
            Some((rename, t)) => Some(ASTWrapper(Some(t), Expr::Var(rename))),
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

            for index_expr @ ASTWrapper(index_span, _) in indexes.into_iter() {
                match analyse_expression(index_expr, local_symb, var_symb, errors) {
                    Some(inner @ ASTWrapper(Some(Type::Int), _)) => correct_indexes.push(inner),
                    Some(ASTWrapper(Some(index_type), _)) => {
                        errors.push(SemanticError::InvalidIndex(index_span, index_type))
                    }
                    Some(ASTWrapper(None, _)) => panic!("Correctly typed expressions must always have a type"),
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
                Some(ASTWrapper(Some(t), Expr::ArrayElem(rename, correct_indexes)))
            } else {
                None
            }
        }

        // Capturing the `snd null` or `fst null` semantic error (personally I
        // think this makes little sense, but alas the spec).
        Expr::UnOp(UnOp::Snd | UnOp::Fst, box ASTWrapper(null_span, Expr::Null)) => {
            errors.push(SemanticError::InvalidPairOp(null_span));
            None
        }

        // Unary operator checking, using the unop_matching functionality from
        // type_constraint.rs. If the internal contains errors, these are passed
        // upwards.
        Expr::UnOp(op, box inner_expr) => {
            match analyse_expression(inner_expr, local_symb, var_symb, errors) {
                Some(ASTWrapper(Some(inner_t), ast)) => match unop_match(&op, &inner_t) {
                    Ok(unop_t) => Some(
                        ASTWrapper(Some(unop_t), Expr::UnOp(op, box ASTWrapper(Some(inner_t), ast))),
                    ),
                    Err((pot_types, pot_ops)) => {
                        errors.push(SemanticError::InvalidUnOp(
                            span, pot_types, pot_ops, inner_t, op,
                        ));
                        None
                    }
                },
                Some(ASTWrapper(None, _)) => panic!("Correctly typed expressions must always have a type"),
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
                    Some(ASTWrapper(Some(left_type), left_ast)),
                    Some(ASTWrapper(Some(right_type), right_ast)),
                ) => match binop_match(&op, &left_type, &right_type) {
                    Ok(t) => Some(
                        ASTWrapper(
                            Some(t),
                            Expr::BinOp(
                                box ASTWrapper(Some(left_type), left_ast),
                                op,
                                box ASTWrapper(Some(right_type), right_ast),
                            ),
                        ),
                    ),
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
                (Some(ASTWrapper(None, _)), _) | (_, Some(ASTWrapper(None, _))) => panic!("Correctly typed expressions must always have a type"),
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

        let expr1: ExprSpan<&str, &str> = ASTWrapper("9", Expr::Int(9));

        match analyse_expression(expr1, &local_symb, &var_symb, &mut vec![]) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: ExprSpan<&str, &str> = ASTWrapper("'a'", Expr::Char('a'));

        match analyse_expression(expr2, &local_symb, &var_symb, &mut vec![]) {
            Some(ASTWrapper(Some(Type::Char), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: ExprSpan<&str, &str> = ASTWrapper("true", Expr::Bool(true));

        match analyse_expression(expr3, &local_symb, &var_symb, &mut vec![]) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: ExprSpan<&str, &str> = ASTWrapper(
            "\"hello world\"",
            Expr::String(String::from("\"hello world\"")),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut vec![]) {
            Some(ASTWrapper(Some(Type::String), _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_detects_errors_in_basic_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: ExprSpan<&str, &str> = ASTWrapper("x", Expr::Var("x"));
        let mut expr1_errors = Vec::new();

        match analyse_expression(expr1, &local_symb, &var_symb, &mut expr1_errors) {
            None => {
                assert!(expr1_errors.contains(&SemanticError::UndefinedVariableUse("x")));
            }
            _ => assert!(false),
        }

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "'a'",
            Expr::ArrayElem("x", vec![ASTWrapper("'c'", Expr::Char('c'))]),
        );
        let mut expr2_errors = Vec::new();
        match analyse_expression(expr2, &local_symb, &var_symb, &mut expr2_errors) {
            None => {
                assert!(expr2_errors.contains(&SemanticError::UndefinedVariableUse("x")));
                assert!(expr2_errors.contains(&SemanticError::InvalidIndex("'c'", Type::Char)));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "-3",
            Expr::UnOp(UnOp::Minus, box ASTWrapper("3", Expr::Int(3))),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "!true",
            Expr::UnOp(UnOp::Neg, box ASTWrapper("true", Expr::Bool(true))),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: ExprSpan<&str, &str> = ASTWrapper(
            "ord 'a'",
            Expr::UnOp(UnOp::Ord, box ASTWrapper("'a'", Expr::Char('a'))),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: ExprSpan<&str, &str> = ASTWrapper(
            "chr 97",
            Expr::UnOp(UnOp::Chr, box ASTWrapper("97", Expr::Int(97))),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Char), _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_chains_of_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "- ord chr 9",
            Expr::UnOp(
                UnOp::Minus,
                box ASTWrapper(
                    "ord chr 9",
                    Expr::UnOp(
                        UnOp::Ord,
                        box ASTWrapper(
                            "chr 9",
                            Expr::UnOp(UnOp::Chr, box ASTWrapper("9", Expr::Int(9))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "chr ord chr 9",
            Expr::UnOp(
                UnOp::Chr,
                box ASTWrapper(
                    "ord chr 9",
                    Expr::UnOp(
                        UnOp::Ord,
                        box ASTWrapper(
                            "chr 9",
                            Expr::UnOp(UnOp::Chr, box ASTWrapper("9", Expr::Int(9))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Char), _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_constant_only_binary_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "(1 + 1) >= (3 * -2)",
            Expr::BinOp(
                box ASTWrapper(
                    "1 + 1",
                    Expr::BinOp(
                        box ASTWrapper("1", Expr::Int(1)),
                        BinOp::Add,
                        box ASTWrapper("1", Expr::Int(1)),
                    ),
                ),
                BinOp::Gte,
                box ASTWrapper(
                    "3 * -2",
                    Expr::BinOp(
                        box ASTWrapper("3", Expr::Int(3)),
                        BinOp::Mul,
                        box ASTWrapper(
                            "-2",
                            Expr::UnOp(UnOp::Minus, box ASTWrapper("2", Expr::Int(2))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "(1 + 1) + (3 * -2)",
            Expr::BinOp(
                box ASTWrapper(
                    "1 + 1",
                    Expr::BinOp(
                        box ASTWrapper("1", Expr::Int(1)),
                        BinOp::Mul,
                        box ASTWrapper("1", Expr::Int(1)),
                    ),
                ),
                BinOp::Add,
                box ASTWrapper(
                    "3 * -2",
                    Expr::BinOp(
                        box ASTWrapper("3", Expr::Int(3)),
                        BinOp::Mul,
                        box ASTWrapper(
                            "-2",
                            Expr::UnOp(UnOp::Minus, box ASTWrapper("2", Expr::Int(2))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: ExprSpan<&str, &str> = ASTWrapper(
            "chr ((1 + 1) + (3 * -2))",
            Expr::UnOp(
                UnOp::Chr,
                box ASTWrapper(
                    "(1 + 1) + (3 * -2)",
                    Expr::BinOp(
                        box ASTWrapper(
                            "1 + 1",
                            Expr::BinOp(
                                box ASTWrapper("1", Expr::Int(1)),
                                BinOp::Mul,
                                box ASTWrapper("1", Expr::Int(1)),
                            ),
                        ),
                        BinOp::Add,
                        box ASTWrapper(
                            "3 * -2",
                            Expr::BinOp(
                                box ASTWrapper("3", Expr::Int(3)),
                                BinOp::Mul,
                                box ASTWrapper(
                                    "-2",
                                    Expr::UnOp(UnOp::Minus, box ASTWrapper("2", Expr::Int(2))),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Char), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: ExprSpan<&str, &str> = ASTWrapper(
            "(ord 'a' == 65) || (true && !false)",
            Expr::BinOp(
                box ASTWrapper(
                    "ord 'a' == 65",
                    Expr::BinOp(
                        box ASTWrapper(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box ASTWrapper("'a'", Expr::Char('a'))),
                        ),
                        BinOp::Eq,
                        box ASTWrapper("65", Expr::Int(65)),
                    ),
                ),
                BinOp::Or,
                box ASTWrapper(
                    "true && !false",
                    Expr::BinOp(
                        box ASTWrapper("true", Expr::Bool(true)),
                        BinOp::And,
                        box ASTWrapper(
                            "!false",
                            Expr::UnOp(UnOp::Neg, box ASTWrapper("false", Expr::Bool(false))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr5: ExprSpan<&str, &str> = ASTWrapper(
            "(97 >= ord 'a') && (true && !false)",
            Expr::BinOp(
                box ASTWrapper(
                    "97 >= ord 'a'",
                    Expr::BinOp(
                        box ASTWrapper("97", Expr::Int(97)),
                        BinOp::Eq,
                        box ASTWrapper(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box ASTWrapper("'a''", Expr::Char('a'))),
                        ),
                    ),
                ),
                BinOp::And,
                box ASTWrapper(
                    "true && !false",
                    Expr::BinOp(
                        box ASTWrapper("true", Expr::Bool(true)),
                        BinOp::And,
                        box ASTWrapper(
                            "!false",
                            Expr::UnOp(UnOp::Neg, box ASTWrapper("false", Expr::Bool(false))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr5, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
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

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "(chr var1 >= ord 'a') || (true && !false)",
            Expr::BinOp(
                box ASTWrapper(
                    "chr var1 >= ord 'a'",
                    Expr::BinOp(
                        box ASTWrapper("var1", Expr::Var("var1")),
                        BinOp::Eq,
                        box ASTWrapper(
                            "ord 'a'",
                            Expr::UnOp(UnOp::Ord, box ASTWrapper("'a''", Expr::Char('a'))),
                        ),
                    ),
                ),
                BinOp::Or,
                box ASTWrapper(
                    "true && !false",
                    Expr::BinOp(
                        box ASTWrapper("true", Expr::Bool(true)),
                        BinOp::And,
                        box ASTWrapper(
                            "!false",
                            Expr::UnOp(UnOp::Neg, box ASTWrapper("false", Expr::Bool(false))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var("var2", &Type::Bool, "bool var2 = true", &mut local_symb)
            .expect("variable not yet defined");

        let expr4: ExprSpan<&str, &str> = ASTWrapper(
            "(ord 'a' == - ord chr 9) || (true && !false)",
            Expr::BinOp(
                box ASTWrapper(
                    "ord 'a' == - ord chr 9",
                    Expr::BinOp(
                        box ASTWrapper(
                            "- ord chr 9",
                            Expr::UnOp(
                                UnOp::Minus,
                                box ASTWrapper(
                                    "ord chr 'a'",
                                    Expr::UnOp(
                                        UnOp::Ord,
                                        box ASTWrapper(
                                            "chr 'a'",
                                            Expr::UnOp(UnOp::Chr, box ASTWrapper("9", Expr::Int(9))),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                        BinOp::Eq,
                        box ASTWrapper("65", Expr::Int(65)),
                    ),
                ),
                BinOp::Or,
                box ASTWrapper(
                    "true && !false",
                    Expr::BinOp(
                        box ASTWrapper("true", Expr::Bool(true)),
                        BinOp::And,
                        box ASTWrapper(
                            "!false",
                            Expr::UnOp(UnOp::Neg, box ASTWrapper("false", Expr::Bool(false))),
                        ),
                    ),
                ),
            ),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
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

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "array1[5]",
            Expr::ArrayElem("array1", vec![ASTWrapper("5", Expr::Int(5))]),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
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

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "array2[5 * 5]",
            Expr::ArrayElem(
                "array2",
                vec![ASTWrapper(
                    "5 * 5",
                    Expr::BinOp(
                        box ASTWrapper("5", Expr::Int(5)),
                        BinOp::Mul,
                        box ASTWrapper("5", Expr::Int(5)),
                    ),
                )],
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Array(box Type::Int, 1)), _)) => assert!(true),
            _ => assert!(false),
        }

        var_symb
            .def_var("num1", &Type::Int, "int num1 = 9", &mut local_symb)
            .expect("variable not yet defined");

        let expr3: ExprSpan<&str, &str> = ASTWrapper(
            "array2[5 * 5][num1]",
            Expr::ArrayElem(
                "array2",
                vec![
                    ASTWrapper(
                        "5 * 5",
                        Expr::BinOp(
                            box ASTWrapper("5", Expr::Int(5)),
                            BinOp::Mul,
                            box ASTWrapper("5", Expr::Int(5)),
                        ),
                    ),
                    ASTWrapper("num1", Expr::Var("num1")),
                ],
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: ExprSpan<&str, &str> = ASTWrapper(
            "array2[5*5][array1[num1]]",
            Expr::ArrayElem(
                "array2",
                vec![
                    ASTWrapper(
                        "5 * 5",
                        Expr::BinOp(
                            box ASTWrapper("5", Expr::Int(5)),
                            BinOp::Mul,
                            box ASTWrapper("5", Expr::Int(5)),
                        ),
                    ),
                    ASTWrapper(
                        "array1[num1]",
                        Expr::ArrayElem("array1", vec![ASTWrapper("num1", Expr::Var("num1"))]),
                    ),
                ],
            ),
        );

        match analyse_expression(expr4, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Int), _)) => assert!(true),
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

        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "pair1 == pair1",
            Expr::BinOp(
                box ASTWrapper("pair1", Expr::Var("pair1")),
                BinOp::Eq,
                box ASTWrapper("pair1", Expr::Var("pair1")),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Bool), _)) => assert!(true),
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

        let expr2: ExprSpan<&str, &str> = ASTWrapper(
            "pair_array[len pair_array - 1]",
            Expr::ArrayElem(
                "pair_array",
                vec![ASTWrapper(
                    "len pair_array - 1",
                    Expr::BinOp(
                        box ASTWrapper(
                            "len pair_array",
                            Expr::UnOp(
                                UnOp::Len,
                                box ASTWrapper("pair_array", Expr::Var("pair_array")),
                            ),
                        ),
                        BinOp::Sub,
                        box ASTWrapper("1", Expr::Int(1)),
                    ),
                )],
            ),
        );

        match analyse_expression(expr2, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Pair(box Type::Int, box Type::Int)), _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: ExprSpan<&str, &str> = ASTWrapper(
            "newpair(5+5, newpair(5+5, 'a')))",
            Expr::BinOp(
                box ASTWrapper(
                    "5 + 5",
                    Expr::BinOp(
                        box ASTWrapper("5", Expr::Int(5)),
                        BinOp::Add,
                        box ASTWrapper("5", Expr::Int(5)),
                    ),
                ),
                BinOp::Newpair,
                box ASTWrapper(
                    "newpair(5+5, 'a')",
                    Expr::BinOp(
                        box ASTWrapper(
                            "5 + 5",
                            Expr::BinOp(
                                box ASTWrapper("5", Expr::Int(5)),
                                BinOp::Add,
                                box ASTWrapper("5", Expr::Int(5)),
                            ),
                        ),
                        BinOp::Newpair,
                        box ASTWrapper("'a'", Expr::Char('a')),
                    ),
                ),
            ),
        );

        match analyse_expression(expr3, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::Char))), _)) => {
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
        let expr1: ExprSpan<&str, &str> = ASTWrapper(
            "newpair(snd pair1, fst pair2)",
            Expr::BinOp(
                box ASTWrapper(
                    "snd pair1",
                    Expr::UnOp(UnOp::Snd, box ASTWrapper("pair1", Expr::Var("pair1"))),
                ),
                BinOp::Newpair,
                box ASTWrapper(
                    "fst pair1",
                    Expr::UnOp(UnOp::Fst, box ASTWrapper("pair1", Expr::Var("pair1"))),
                ),
            ),
        );

        match analyse_expression(expr1, &local_symb, &var_symb, &mut Vec::new()) {
            Some(ASTWrapper(Some(Type::Pair(box Type::Array(box Type::Bool, 2), box Type::Int)), _)) => {
                assert!(true)
            }
            _ => assert!(false),
        }
    }
}
