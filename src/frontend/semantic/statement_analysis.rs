//! Analyse a block of instructions to get all errors, return either a modified
//! AST or the errors produced.
use crate::frontend::ast::*;
use crate::frontend::semantic::expression_analysis::*;
use crate::frontend::semantic::semantic_errors::*;
use crate::frontend::semantic::symbol_table::*;
use crate::frontend::semantic::type_constraints::*;

/// Analyse a vector of statements
fn analyse_block<'a, 'b>(
    stats: Vec<StatSpan<'a, Stat<'a, &'a str>>>,
    fun_symb: &FunctionSymbolTable,
    local_symb: &mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
    ret_type: Option<Type>,
) -> Result<(Vec<StatSpan<'a, Stat<'a, usize>>>, bool), SemanticErrorSummary<'a>> {
    let correct: Vec<StatSpan<'a, Stat<'a, usize>>> = Vec::new();
    // let errors = Vec::new();

    for WrapSpan(span, stat) in stats {
        match stat {
            Stat::Skip => todo!(),
            Stat::Def(var_type, name, rhs) => todo!(),
            Stat::Assign(_, _) => todo!(),
            Stat::Read(_) => todo!(),
            Stat::Free(_) => todo!(),
            Stat::Return(_) => todo!(),
            Stat::Exit(_) => todo!(),
            Stat::Print(_) => todo!(),
            Stat::PrintLn(_) => todo!(),
            Stat::If(_, _, _) => todo!(),
            Stat::While(_, _) => todo!(),
            Stat::Block(_) => todo!(),
        }
    }

    todo!()
}

/// Given the type of the internal expressions to return, the assignment and
/// symbol tables, determines if errors are present.

fn analyse_rhs<'a, 'b>(
    expected: &Type,
    rhs: AssignRhs<'a, &'a str>,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
) -> Result<AssignRhs<'a, usize>, Vec<SemanticError<'a>>> {
    match rhs {
        AssignRhs::Expr(WrapSpan(expr_span, expr)) => {
            match analyse_expression(expr, local_symb, var_symb) {
                Ok((expr_t, ast)) => {
                    if can_coerce(expected, &expr_t) {
                        Ok(AssignRhs::Expr(WrapSpan(expr_span, ast)))
                    } else {
                        Err(vec![SemanticError::InvalidType(
                            expr_span,
                            expected.clone(),
                            expr_t,
                        )])
                    }
                }
                Err(errs) => Err(errs),
            }
        }
        AssignRhs::Array(vals) => {
            // check if assignment is possible
            match de_index(expected, 1) {
                Some(expr_expected) => {
                    let mut correct = Vec::new();
                    let mut errors = Vec::new();
                    vals.into_iter().for_each(
                        |WrapSpan(expr_span, expr)| match analyse_expression(
                            expr, local_symb, var_symb,
                        ) {
                            Ok((expr_type, ast)) => {
                                if can_coerce(&expr_expected, &expr_type) {
                                    correct.push(WrapSpan(expr_span, ast))
                                } else {
                                    errors.push(SemanticError::InvalidType(
                                        expr_span,
                                        expected.clone(),
                                        expr_type,
                                    ))
                                }
                            }
                            Err(mut errs) => errors.append(&mut errs),
                        },
                    );

                    if errors.len() == 0 {
                        Ok(AssignRhs::Array(correct))
                    } else {
                        Err(errors)
                    }
                }
                None => Err(vec![SemanticError::InvalidArrayLiteral(
                    vals.into_iter().map(|WrapSpan(span, _)| span).collect(),
                )]),
            }
        }
        AssignRhs::Call(fun_name, args) => {
            match fun_symb.get_fun(fun_name) {
                Some((ret_type, param_types)) => {
                    let mut errors = Vec::new();
                    let mut correct = Vec::new();
                    let args_len = args.len();
                    let mut check_args = true;

                    if !can_coerce(expected, &ret_type) {
                        errors.push(SemanticError::InvalidCallType(
                            fun_name,
                            expected.clone(),
                            ret_type,
                        ));
                        check_args = false;
                    }

                    if param_types.len() != args_len {
                        errors.push(SemanticError::FunctionParametersLengthMismatch(
                            fun_name,
                            param_types.len(),
                            args_len,
                        ));
                        check_args = false;
                    }

                    if check_args {
                        args.into_iter().zip(param_types.into_iter()).for_each(
                            |(WrapSpan(expr_span, expr), (param_name, param_type))| {
                                match analyse_expression(expr, local_symb, var_symb) {
                                    Ok((expr_type, ast)) => {
                                        if can_coerce(&param_type, &expr_type) {
                                            correct.push(WrapSpan(expr_span, ast))
                                        } else {
                                            errors.push(SemanticError::FunctionArgumentTypeInvalid(
                                                expr_span, param_name, param_type, expr_type,
                                            ))
                                        }
                                    }
                                    Err(mut errs) => errors.append(&mut errs),
                                }
                            },
                        );
                    }

                    if errors.len() == 0 {
                        Ok(AssignRhs::Call(fun_name, correct))
                    } else {
                        Err(errors)
                    }
                }
                None => {
                    // even if the function is undefined, we can find issues in its arguments
                    let mut errors = Vec::new();
                    args.into_iter().for_each(|WrapSpan(_, expr)| {
                        match analyse_expression(expr, local_symb, var_symb) {
                            Err(mut errs) => errors.append(&mut errs),
                            _ => (),
                        }
                    });

                    errors.push(SemanticError::UndefinedFunction(fun_name));
                    Err(errors)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analyse_rhs_can_check_valid_function_calls() {
        let mut var_symb = VariableSymbolTable::new();
        let mut fun_symb = FunctionSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        fun_symb.def_fun(
            "fun1",
            (Type::Int, vec![("a", Type::Int), ("b", Type::Int)]),
        );
        fun_symb.def_fun("fun2", (Type::Int, vec![]));
        fun_symb.def_fun(
            "fun3",
            (
                Type::Char,
                vec![("p1", Type::Pair(box Type::Int, box Type::Int))],
            ),
        );
        fun_symb.def_fun("fun4", (Type::Int, vec![("a", Type::Int)]));

        assert_eq!(
            analyse_rhs(
                &Type::Int,
                AssignRhs::Call(
                    "fun1",
                    vec![WrapSpan("1", Expr::Int(3)), WrapSpan("4", Expr::Int(4))]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Call(
                "fun1",
                vec![WrapSpan("1", Expr::Int(3)), WrapSpan("4", Expr::Int(4))]
            ))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Int,
                AssignRhs::Call("fun2", vec![]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Call("fun2", vec![]))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Char,
                AssignRhs::Call(
                    "fun3",
                    vec![WrapSpan(
                        "newpair(5,5)",
                        Expr::BinOp(
                            box WrapSpan("5", Expr::Int(5)),
                            WrapSpan("newpair", BinOp::Newpair),
                            box WrapSpan("5", Expr::Int(5))
                        )
                    )]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Call(
                "fun3",
                vec![WrapSpan(
                    "newpair(5,5)",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        WrapSpan("newpair", BinOp::Newpair),
                        box WrapSpan("5", Expr::Int(5))
                    )
                )]
            ))
        );

        var_symb.def_var(
            "a",
            Type::Pair(box Type::Int, box Type::Int),
            "pair(int,int) a = newpair(5,5)",
            &mut local_symb,
        );

        // with a variable
        assert_eq!(
            analyse_rhs(
                &Type::Char,
                AssignRhs::Call("fun3", vec![WrapSpan("a", Expr::Var("a"))]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Call("fun3", vec![WrapSpan("a", Expr::Var(0))]))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Int,
                AssignRhs::Call(
                    "fun4",
                    vec![WrapSpan(
                        "fst a",
                        Expr::UnOp(
                            WrapSpan("fst", UnOp::Fst),
                            box WrapSpan("a", Expr::Var("a"))
                        )
                    )]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Call(
                "fun4",
                vec![WrapSpan(
                    "fst a",
                    Expr::UnOp(WrapSpan("fst", UnOp::Fst), box WrapSpan("a", Expr::Var(0)))
                )]
            ))
        );
    }

    #[test]
    fn analyse_rhs_can_check_invalid_function_calls() {
        let mut var_symb = VariableSymbolTable::new();
        let mut fun_symb = FunctionSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        fun_symb.def_fun(
            "fun1",
            (Type::Int, vec![("a", Type::Int), ("b", Type::Int)]),
        );
        fun_symb.def_fun("fun2", (Type::Int, vec![]));
        fun_symb.def_fun(
            "fun3",
            (
                Type::Char,
                vec![("p1", Type::Pair(box Type::Int, box Type::Int))],
            ),
        );

        var_symb.def_var(
            "pair_a",
            Type::Pair(box Type::Int, box Type::Int),
            "pair(int,int) a = newpair(5,5)",
            &mut local_symb,
        );

        // Errors:
        // - The return type of fun1 is int (expects char)
        // - The number of parameters for fun1 is 2, there is only 1
        match analyse_rhs(
            &Type::Char,
            AssignRhs::Call("fun1", vec![WrapSpan("pair_a", Expr::Var("pair_a"))]),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
        ) {
            Err(errs) => {
                assert!(errs.contains(&SemanticError::InvalidCallType(
                    "fun1",
                    Type::Char,
                    Type::Int
                )));
                assert!(
                    errs.contains(&SemanticError::FunctionParametersLengthMismatch(
                        "fun1", 2, 1
                    ))
                );
            }
            _ => assert!(false),
        }

        // Errors:
        // - The first argument is a pair(int,int) should be an int
        // - The second argument is a bool, should be an int
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Call(
                "fun1",
                vec![
                    WrapSpan("pair_a", Expr::Var("pair_a")),
                    WrapSpan("true", Expr::Bool(true)),
                ],
            ),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
        ) {
            Err(errs) => {
                // FunctionArgumentTypeInvalid("a", "a", Int, Pair(Int, Int)), FunctionArgumentTypeInvalid("true", "b", Int, Bool)
                assert!(errs.contains(&SemanticError::FunctionArgumentTypeInvalid(
                    "pair_a",
                    "a",
                    Type::Int,
                    Type::Pair(box Type::Int, box Type::Int)
                )));
                assert!(errs.contains(&SemanticError::FunctionArgumentTypeInvalid(
                    "true",
                    "b",
                    Type::Int,
                    Type::Bool
                )));
            }
            _ => assert!(false),
        }

        // Errors:
        // - The first argument is a pair(int,int) should be an int
        // - The second argument is a bool, should be an int
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Call(
                "fun1",
                vec![
                    WrapSpan("pair_j", Expr::Var("pair_j")),
                    WrapSpan("true", Expr::Bool(true)),
                ],
            ),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
        ) {
            Err(errs) => {
                assert!(errs.contains(&SemanticError::UndefinedVariableUse("pair_j")));
                assert!(errs.contains(&SemanticError::FunctionArgumentTypeInvalid(
                    "true",
                    "b",
                    Type::Int,
                    Type::Bool
                )));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_rhs_can_check_valid_array_literals() {
        let mut var_symb = VariableSymbolTable::new();
        let fun_symb = FunctionSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        var_symb.def_var("a", Type::Int, "int a = 9", &mut local_symb);
        var_symb.def_var("b", Type::Char, "char b = 'a'", &mut local_symb);

        assert_eq!(
            analyse_rhs(
                &Type::Array(box Type::Int, 1),
                AssignRhs::Array(vec![
                    WrapSpan("3", Expr::Int(3)),
                    WrapSpan("3", Expr::Int(3)),
                    WrapSpan("3", Expr::Int(3)),
                    WrapSpan("3", Expr::Int(3))
                ]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Array(vec![
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3))
            ]))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Array(box Type::Int, 1),
                AssignRhs::Array(vec![
                    WrapSpan("a", Expr::Var("a")),
                    WrapSpan("3", Expr::Int(3)),
                    WrapSpan("3", Expr::Int(3)),
                    WrapSpan("3", Expr::Int(3))
                ]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Array(vec![
                WrapSpan("a", Expr::Var(0)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3))
            ]))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Array(box Type::Char, 1),
                AssignRhs::Array(vec![WrapSpan("b", Expr::Var("b"))]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb
            ),
            Ok(AssignRhs::Array(vec![WrapSpan("b", Expr::Var(1))]))
        );
    }

    fn analyse_rhs_can_check_invalid_array_literals() {
        let mut var_symb = VariableSymbolTable::new();
        let fun_symb = FunctionSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        var_symb.def_var("a", Type::Int, "int a = 9", &mut local_symb);
        var_symb.def_var("b", Type::Char, "char b = 'a'", &mut local_symb);

        // the array is the wrong type
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Array(vec![
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
            ]),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
        ) {
            Err(errs) => {
                assert!(errs.contains(&SemanticError::InvalidArrayLiteral(vec![
                    "3", "3", "3", "3"
                ])));
            }
            _ => assert!(false),
        }

        // variable b is a char not an int
        match analyse_rhs(
            &Type::Array(box Type::Int, 1),
            AssignRhs::Array(vec![
                WrapSpan("b", Expr::Var("b")),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
                WrapSpan("3", Expr::Int(3)),
            ]),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
        ) {
            Err(errs) => {
                assert!(errs.contains(&SemanticError::InvalidType("b", Type::Int, Type::Char)));
            }
            _ => assert!(false),
        }
    }
}
