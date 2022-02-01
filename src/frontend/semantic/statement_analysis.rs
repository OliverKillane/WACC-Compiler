//! Analyse a block of instructions to get all errors, return either a modified
//! AST or the errors produced.

use super::{
    super::ast::{AssignLhs, AssignRhs, Stat, StatSpan, Type, WrapSpan},
    expression_analysis::analyse_expression,
    semantic_errors::SemanticError,
    symbol_table::{FunctionSymbolTable, LocalSymbolTable, VariableSymbolTable},
    type_constraints::{can_coerce, de_index},
};

/// Analyse a vector of statements
pub fn analyse_block<'a, 'b>(
    stats: Vec<StatSpan<'a, &'a str>>,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &'b mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
    ret_type: &Option<Type>,
) -> Result<(Vec<WrapSpan<'a, Stat<'a, usize>>>, bool), Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>> {
    let mut correct: Vec<StatSpan<'a, usize>> = Vec::new();
    let mut errors: Vec<WrapSpan<'a, Vec<SemanticError<'a>>>> = Vec::new();
    let mut terminated = false;

    for WrapSpan(span, stat) in stats {
        match stat {
            Stat::Skip => todo!(),
            Stat::Def(var_type, name, rhs) => {
                match var_symb.def_var(name, &var_type, span, local_symb) {
                    Ok(rename) => {
                        match analyse_rhs(&var_type, rhs, fun_symb, local_symb, var_symb) {
                            Ok(renamed_rhs) => correct
                                .push(WrapSpan(span, Stat::Def(var_type, rename, renamed_rhs))),
                            Err(errs) => errors.push(WrapSpan(span, errs)),
                        }
                    }
                    Err(err) => errors.push(WrapSpan(span, vec![err])),
                }
            }
            Stat::Assign(lhs, rhs) => match analyse_lhs(lhs, var_symb, local_symb) {
                Ok((left_type, lhs_ast)) => {
                    match analyse_rhs(&left_type, rhs, fun_symb, local_symb, var_symb) {
                        Ok(rhs_ast) => correct.push(WrapSpan(span, Stat::Assign(lhs_ast, rhs_ast))),
                        Err(errs) => errors.push(WrapSpan(span, errs)),
                    }
                }
                Err(errs) => errors.push(WrapSpan(span, errs)),
            },
            Stat::Read(lhs) => match analyse_lhs(lhs, var_symb, local_symb) {
                Ok((lhs_type, lhs_ast)) => {
                    if can_coerce(&Type::Int, &lhs_type) || can_coerce(&Type::Int, &lhs_type) {
                        correct.push(WrapSpan(span, Stat::Read(lhs_ast)))
                    } else {
                        errors.push(WrapSpan(
                            span,
                            vec![SemanticError::ReadStatementMismatch(span, lhs_type)],
                        ))
                    }
                }
                Err(errs) => errors.push(WrapSpan(span, errs)),
            },
            Stat::Free(WrapSpan(expr_span, expr)) => {
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((expr_type, expr_ast)) => {
                        if can_coerce(&Type::Pair(box Type::Any, box Type::Any), &expr_type)
                            || can_coerce(&Type::Array(box Type::Any, 1), &expr_type)
                        {
                            correct.push(WrapSpan(span, Stat::Free(WrapSpan(expr_span, expr_ast))))
                        }
                    }
                    Err(errs) => errors.push(WrapSpan(span, errs)),
                }
            }
            Stat::Return(WrapSpan(expr_span, expr)) => {
                terminated = true;
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((expr_type, expr_ast)) => match &ret_type {
                        Some(return_type) => {
                            if can_coerce(return_type, &expr_type) {
                                correct.push(WrapSpan(
                                    span,
                                    Stat::Return(WrapSpan(expr_span, expr_ast)),
                                ))
                            } else {
                                errors.push(WrapSpan(
                                    span,
                                    vec![SemanticError::InvalidFunctionReturn(
                                        expr_span,
                                        return_type.clone(),
                                        expr_type,
                                    )],
                                ))
                            }
                        }
                        None => errors.push(WrapSpan(
                            span,
                            vec![SemanticError::ReturnStatementMisplaced],
                        )),
                    },
                    Err(errs) => errors.push(WrapSpan(span, errs)),
                }
            }
            Stat::Exit(WrapSpan(expr_span, expr)) => {
                terminated = true;
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((expr_type, expr_ast)) => {
                        if can_coerce(&Type::Int, &expr_type) {
                            correct.push(WrapSpan(span, Stat::Exit(WrapSpan(span, expr_ast))))
                        } else {
                            errors.push(WrapSpan(
                                span,
                                vec![SemanticError::ExitStatementMismatch(expr_span, expr_type)],
                            ))
                        }
                    }
                    Err(errs) => errors.push(WrapSpan(span, errs)),
                }
            }
            Stat::Print(WrapSpan(_, expr)) => {
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((_, expr_ast)) => {
                        correct.push(WrapSpan(span, Stat::Print(WrapSpan(span, expr_ast))))
                    }
                    Err(errs) => errors.push(WrapSpan(span, errs)),
                }
            }
            Stat::PrintLn(WrapSpan(_, expr)) => {
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((_, expr_ast)) => {
                        correct.push(WrapSpan(span, Stat::PrintLn(WrapSpan(span, expr_ast))))
                    }
                    Err(errs) => errors.push(WrapSpan(span, errs)),
                }
            }
            Stat::If(WrapSpan(cond_span, cond), if_block, else_block) => {
                let cond_valid = match analyse_expression(cond, local_symb, var_symb) {
                    Ok((cond_type, cond_ast)) => {
                        if can_coerce(&Type::Bool, &cond_type) {
                            Some(WrapSpan(cond_span, cond_ast))
                        } else {
                            errors.push(WrapSpan(
                                span,
                                vec![SemanticError::InvalidIfCondition(cond_span, cond_type)],
                            ));
                            None
                        }
                    }
                    Err(errs) => {
                        errors.push(WrapSpan(span, errs));
                        None
                    }
                };

                let if_block_valid = match analyse_block(
                    if_block,
                    fun_symb,
                    &mut LocalSymbolTable::new_child(local_symb),
                    var_symb,
                    ret_type,
                ) {
                    Ok((block_ast, block_terminated)) => {
                        terminated = terminated || block_terminated;
                        Some(block_ast)
                    }
                    Err(mut block_errs) => {
                        errors.append(&mut block_errs);
                        None
                    }
                };

                let else_block_valid = match analyse_block(
                    else_block,
                    fun_symb,
                    &mut LocalSymbolTable::new_child(local_symb),
                    var_symb,
                    ret_type,
                ) {
                    Ok((block_ast, block_terminated)) => {
                        terminated = terminated || block_terminated;
                        Some(block_ast)
                    }
                    Err(mut block_errs) => {
                        errors.append(&mut block_errs);
                        None
                    }
                };

                match (cond_valid, if_block_valid, else_block_valid) {
                    (Some(cond_ast), Some(if_block_ast), Some(else_block_ast)) => correct.push(
                        WrapSpan(span, Stat::If(cond_ast, if_block_ast, else_block_ast)),
                    ),
                    _ => (),
                }
            }
            Stat::While(WrapSpan(cond_span, cond), loop_block) => {
                let condition_valid = match analyse_expression(cond, local_symb, var_symb) {
                    Ok((cond_type, cond_ast)) => {
                        if can_coerce(&Type::Bool, &cond_type) {
                            Some(WrapSpan(cond_span, cond_ast))
                        } else {
                            errors.push(WrapSpan(
                                span,
                                vec![SemanticError::InvalidWhileCondition(cond_span, cond_type)],
                            ));
                            None
                        }
                    }
                    Err(errs) => {
                        errors.push(WrapSpan(span, errs));
                        None
                    }
                };

                match (
                    condition_valid,
                    analyse_block(
                        loop_block,
                        fun_symb,
                        &mut LocalSymbolTable::new_child(local_symb),
                        var_symb,
                        ret_type,
                    ),
                ) {
                    (Some(cond_wrap), Ok((block_ast, block_terminated))) => {
                        terminated = terminated || block_terminated;
                        correct.push(WrapSpan(span, Stat::While(cond_wrap, block_ast)));
                    }
                    (_, Err(mut block_errs)) => errors.append(&mut block_errs),
                    (_, _) => (),
                }
            }
            Stat::Block(block) => {
                match analyse_block(
                    block,
                    fun_symb,
                    &mut LocalSymbolTable::new_child(local_symb),
                    var_symb,
                    ret_type,
                ) {
                    Ok((block_ast, block_terminated)) => {
                        correct.push(WrapSpan(span, Stat::Block(block_ast)));
                        terminated = terminated || block_terminated;
                    }
                    Err(mut block_errs) => errors.append(&mut block_errs),
                }
            }
        }
    }

    if errors.len() == 0 {
        Ok((correct, terminated))
    } else {
        Err(errors)
    }
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

fn analyse_lhs<'a, 'b>(
    lhs: AssignLhs<'a, &'a str>,
    var_symb: &mut VariableSymbolTable,
    local_symb: &LocalSymbolTable<'a, 'b>,
) -> Result<(Type, AssignLhs<'a, usize>), Vec<SemanticError<'a>>> {
    match lhs {
        AssignLhs::Var(var_name) => match var_symb.get_type(var_name, local_symb) {
            Some((rename, var_type)) => Ok((var_type, AssignLhs::Var(rename))),
            None => Err(vec![SemanticError::UndefinedVariableAssignment(var_name)]),
        },
        AssignLhs::ArrayElem(var_name, index_exprs) => {
            let mut errors = Vec::new();
            let mut correct = Vec::new();
            let dim = index_exprs.len();

            for WrapSpan(expr_span, expr) in index_exprs.into_iter() {
                match analyse_expression(expr, local_symb, var_symb) {
                    Ok((expr_type, ast)) => {
                        if can_coerce(&Type::Int, &expr_type) {
                            correct.push(WrapSpan(expr_span, ast));
                        } else {
                            errors.push(SemanticError::InvalidIndex(expr_span))
                        }
                    }
                    Err(mut errs) => errors.append(&mut errs),
                }
            }

            match var_symb.get_type(var_name, local_symb) {
                Some((rename, var_type)) => match de_index(&var_type, dim) {
                    Some(expected_type) => {
                        if errors.len() == 0 {
                            return Ok((expected_type, AssignLhs::ArrayElem(rename, correct)));
                        }
                    }
                    None => errors.push(SemanticError::InvalidVariableType(
                        var_name,
                        Type::Array(box Type::Any, dim),
                        var_type,
                    )),
                },
                None => {
                    errors.push(SemanticError::UndefinedVariableAssignment(var_name));
                }
            }
            Err(errors)
        }
        AssignLhs::PairFst(WrapSpan(span, expr)) => {
            match analyse_expression(expr, local_symb, var_symb) {
                Ok((Type::Pair(box t, _), ast)) => Ok((t, AssignLhs::PairFst(WrapSpan(span, ast)))),
                Ok((t, _)) => Err(vec![SemanticError::InvalidType(
                    span,
                    Type::Pair(box Type::Any, box Type::Any),
                    t,
                )]),
                Err(errs) => Err(errs),
            }
        }
        AssignLhs::PairSnd(WrapSpan(span, expr)) => {
            match analyse_expression(expr, local_symb, var_symb) {
                Ok((Type::Pair(_, box t), ast)) => Ok((t, AssignLhs::PairFst(WrapSpan(span, ast)))),
                Ok((t, _)) => Err(vec![SemanticError::InvalidType(
                    span,
                    Type::Pair(box Type::Any, box Type::Any),
                    t,
                )]),
                Err(errs) => Err(errs),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::super::ast::{BinOp, Expr, UnOp},
        *,
    };

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
                            BinOp::Newpair,
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
                        BinOp::Newpair,
                        box WrapSpan("5", Expr::Int(5))
                    )
                )]
            ))
        );

        var_symb
            .def_var(
                "a",
                &Type::Pair(box Type::Int, box Type::Int),
                "pair(int,int) a = newpair(5,5)",
                &mut local_symb,
            )
            .expect("variable not yet defined");

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
                            UnOp::Fst,
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
                    Expr::UnOp(UnOp::Fst, box WrapSpan("a", Expr::Var(0)))
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

        var_symb
            .def_var(
                "pair_a",
                &Type::Pair(box Type::Int, box Type::Int),
                "pair(int,int) a = newpair(5,5)",
                &mut local_symb,
            )
            .expect("variable not yet defined");

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

        var_symb
            .def_var("a", &Type::Int, "int a = 9", &mut local_symb)
            .expect("variable not yet defined");
        var_symb
            .def_var("b", &Type::Char, "char b = 'a'", &mut local_symb)
            .expect("variable not yet defined");

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

        var_symb
            .def_var("a", &Type::Int, "int a = 9", &mut local_symb)
            .expect("variable not yet defined");
        var_symb
            .def_var("b", &Type::Char, "char b = 'a'", &mut local_symb)
            .expect("variable not yet defined");

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
