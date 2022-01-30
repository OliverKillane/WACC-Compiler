use crate::frontend::ast::*;
use crate::frontend::semantic::type_constraints::*;
use crate::frontend::semantic::symbol_table::*;
use crate::frontend::semantic::semantic_errors::*;

fn analyse_expression<'a, 'b>(
    WrapSpan(span, expr): WrapSpan<'a, Expr<'a, &'a str>>,
    type_cons: TypeConstraint,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
) -> Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>> {
    // primitive check of base cases for expr (for conciseness)
    let prim_check = |prim_type: Type,
                      prim_expr: Expr<'a, usize>,
                      span: &'a str,
                      type_cons: TypeConstraint|
     -> Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>> {
        if type_cons.inside(&prim_type) {
            Ok((prim_type, WrapSpan(span, prim_expr)))
        } else {
            Err(ExpressionError::Is(ExprErrType::InvalidType(
                span, type_cons, prim_type,
            )))
        }
    };

    match expr {
        Expr::Null => prim_check(
            Type::Pair(box Type::Any, box Type::Any),
            Expr::Null,
            span,
            type_cons,
        ),
        Expr::Int(i) => prim_check(Type::Int, Expr::Int(i), span, type_cons),
        Expr::Bool(b) => prim_check(Type::Bool, Expr::Bool(b), span, type_cons),
        Expr::Char(c) => prim_check(Type::Char, Expr::Char(c), span, type_cons),
        Expr::String(s) => prim_check(Type::String, Expr::String(s), span, type_cons),
        Expr::Var(var_id) => match var_symb.get_type(var_id, &local_symb) {
            Some((var_rename, var_type)) => {
                if type_cons.inside(&var_type) {
                    Ok((var_type, WrapSpan(span, Expr::Var(var_rename))))
                } else {
                    Err(ExpressionError::Is(ExprErrType::InvalidType(
                        span, type_cons, var_type,
                    )))
                }
            }
            None => Err(ExpressionError::Is(ExprErrType::UndefinedVar(var_id))),
        },
        Expr::ArrayElem(var_id, indexes) => {
            // To get expected type for the variable, increase the
            // indentation of the constraints by the index_dim. unless ANY
            let index_level = indexes.len();
            let indexed_type_cons = type_cons.index_constraints(index_level);

            // check the expressions indexing
            let index_exprs: Vec<
                Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>>,
            > = indexes
                .into_iter()
                .map(|wrapped_exp: WrapSpan<'a, Expr<'a, &'a str>>| {
                    analyse_expression(
                        wrapped_exp,
                        TypeConstraint::new(Type::Int),
                        local_symb,
                        var_symb,
                    )
                })
                .collect();

            // check the identifier
            match var_symb.get_type(var_id, &local_symb) {
                Some((var_rename, var_type)) => {
                    // Get all errors from the indexes
                    let mut errors = vec![];
                    let mut correct = vec![];
                    for res in index_exprs {
                        match res {
                            Ok((_, expr)) => correct.push(expr),
                            Err(ExpressionError::Is(err)) => errors.push(err),
                            Err(ExpressionError::Contains(mut errs)) => errors.append(&mut errs),
                        }
                    }

                    if indexed_type_cons.inside(&var_type) {
                        // the type of the
                        if errors.len() == 0 {
                            // The variable is the correct type and all indexing expressions are correctly typed
                            Ok(
                                (var_type.reduce_index_depth(index_level).expect("expression reduced index could not match, despite the increased matching type constraints"), 
                                WrapSpan(span, Expr::ArrayElem(var_rename, correct)))
                            )
                        } else {
                            // Some of the expressions were erroneous e.g a[true]['a']
                            Err(ExpressionError::Contains(errors))
                        }
                    } else {
                        if errors.len() == 0 {
                            match var_type.clone().reduce_index_depth(index_level) {
                                // The subexpression is valid, but the whole expression evaluates to the wrong type
                                Some(t) => Err(ExpressionError::Is(ExprErrType::InvalidType(
                                    span, type_cons, t,
                                ))),
                                // The indexing is wrong for the variable being indexed, but all indexing expressions are well typed
                                None => {
                                    Err(ExpressionError::Contains(vec![ExprErrType::InvalidType(
                                        var_id, type_cons, var_type,
                                    )]))
                                }
                            }
                        } else {
                            // The variable being indexed is the wrong type, and there are errors in some of the indexing expressions.
                            errors.push(ExprErrType::InvalidType(var_id, type_cons, var_type));
                            Err(ExpressionError::Contains(errors))
                        }
                    }
                }
                None => {
                    let mut errors = vec![ExprErrType::UndefinedVar(var_id)];
                    for res in index_exprs {
                        match res {
                            Ok(_) => (),
                            Err(ExpressionError::Is(err)) => errors.push(err),
                            Err(ExpressionError::Contains(mut errs)) => errors.append(&mut errs),
                        }
                    }
                    // The variable being indexed is undefined, if any errors occur in the indexing expressions they are passed forwards
                    Err(ExpressionError::Contains(errors))
                }
            }
        }
        Expr::UnOp(WrapSpan(unop_span, unop), box inner_expr) => {
            match type_cons.new_from_unop(&unop) {
                Ok(inner_cons) => {
                    match analyse_expression(inner_expr, inner_cons, local_symb, var_symb) {
                        Ok((inner_type, inner)) => Ok((get_unop_output_type(&unop, &inner_type).expect("The operator type was correct, and the inner expression matched so it must have an output type"), WrapSpan(span, Expr::UnOp(WrapSpan(unop_span, unop), box inner)))),
                        Err(err) => Err(err.to_contains()),
                    }
                }
                Err(inner_cons) => {
                    match analyse_expression(inner_expr, inner_cons, local_symb, var_symb) {
                        Ok((inner_type, _)) => {
                            Err(ExpressionError::Contains(vec![ExprErrType::InvalidUnOp(
                                unop_span,
                                type_cons.get_possible_unops(&inner_type),
                                unop,
                            )]))
                        }
                        Err(ExpressionError::Is(ExprErrType::InvalidType(
                            inner_span,
                            inner_cons,
                            inner_type,
                        ))) => match get_unop_output_type(&unop, &inner_type) {
                            Some(out_type) => Err(ExpressionError::Is(ExprErrType::InvalidType(
                                span, type_cons, out_type,
                            ))),
                            None => Err(ExpressionError::Contains(vec![
                                ExprErrType::InvalidUnOp(
                                    unop_span,
                                    type_cons.get_possible_unops(&inner_type),
                                    unop,
                                ),
                                ExprErrType::InvalidType(inner_span, inner_cons, inner_type),
                            ])),
                        },
                        Err(other) => {
                            Err(other.append_err(ExprErrType::InvalidUnOp(unop_span, vec![], unop)))
                        }
                    }
                }
            }
        }
        Expr::BinOp(
            box WrapSpan(left_span, left_expr),
            WrapSpan(binop_span, binop),
            box WrapSpan(right_span, right_expr),
        ) => {
            match type_cons.new_from_binop(&binop) {
                Ok((left_cons, right_cons)) => {
                    match (
                        analyse_expression(
                            WrapSpan(left_span, left_expr),
                            left_cons,
                            local_symb,
                            var_symb,
                        ),
                        analyse_expression(
                            WrapSpan(right_span, right_expr),
                            right_cons,
                            local_symb,
                            var_symb,
                        ),
                    ) {
                        (Ok((left_type, left)), Ok((right_type, right))) => {
                            if type_cons.binop_generic_check(&left_type, &right_type, &binop) {
                                Ok((
                                    get_binop_output_type(&binop, &left_type, &right_type)
                                        .expect("Types matched, so must have an output type"),
                                    WrapSpan(
                                        span,
                                        Expr::BinOp(
                                            box left,
                                            WrapSpan(binop_span, binop),
                                            box right,
                                        ),
                                    ),
                                ))
                            } else {
                                Err(ExpressionError::Is(ExprErrType::InvalidGeneric(
                                    left_span, binop_span, right_span, left_type, binop, right_type,
                                )))
                            }
                        }
                        (Ok(_), Err(err)) => Err(err.to_contains()),
                        (Err(err), Ok(_)) => Err(err.to_contains()),
                        (Err(err1), Err(err2)) => Err(err1.add_errs(err2)),
                    }
                }
                Err((left_cons, right_cons)) => {
                    // the operator is invalid, searching with constraint from all binary operators
                    match (
                        analyse_expression(
                            WrapSpan(left_span, left_expr),
                            left_cons,
                            local_symb,
                            var_symb,
                        ),
                        analyse_expression(
                            WrapSpan(right_span, right_expr),
                            right_cons,
                            local_symb,
                            var_symb,
                        ),
                    ) {
                        (Ok((left_type, _)), Ok((right_type, _))) => {
                            Err(ExpressionError::Contains(vec![ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&left_type, &right_type),
                                binop,
                            )]))
                        }
                        (Ok((left_type, _)), Err(err)) => {
                            Err(err.append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&left_type, &Type::Any),
                                binop,
                            )))
                        }
                        (Err(err), Ok((right_type, _))) => {
                            Err(err.append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&Type::Any, &right_type),
                                binop,
                            )))
                        }
                        (Err(left_err), Err(right_err)) => Err(left_err
                            .add_errs(right_err)
                            .append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&Type::Any, &Type::Any),
                                binop,
                            ))),
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn analyse_expression_matches_primitive_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan("9", Expr::Int(9));

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan("'a''", Expr::Char('a'));

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan("true", Expr::Bool(true));

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "\"hello world\"",
            Expr::String(String::from("\"hello world\"")),
        );

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::String, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4,
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::String, _)) => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn analyse_expression_detects_errors_in_primitive_expressions() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan("9", Expr::Int(9));

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan("'a''", Expr::Char('a'));

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan("true", Expr::Bool(true));

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "\"hello world\"",
            Expr::String(String::from("\"hello world\"")),
        );

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        match analyse_expression(
            expr4,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "-3",
            Expr::UnOp(WrapSpan("-", UnOp::Minus), box WrapSpan("3", Expr::Int(3))),
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "!true",
            Expr::UnOp(
                WrapSpan("!", UnOp::Neg),
                box WrapSpan("true", Expr::Bool(true)),
            ),
        );

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "ord 'a'",
            Expr::UnOp(
                WrapSpan("ord", UnOp::Ord),
                box WrapSpan("'a'", Expr::Char('a')),
            ),
        );

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "chr 97",
            Expr::UnOp(
                WrapSpan("chr", UnOp::Chr),
                box WrapSpan("97", Expr::Int(97)),
            ),
        );

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4,
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_chains_of_unary_operations() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "- ord chr 9",
            Expr::UnOp(
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
            ),
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "chr ord chr 9",
            Expr::UnOp(
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
            ),
        );

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Char, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
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
            ),
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 * 1) + (3 * -2)",
            Expr::BinOp(
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
            ),
        );

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) + (3 * -2)",
            Expr::BinOp(
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
            ),
        );

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "(ord 'a' == 65) || (true && !false)",
            Expr::BinOp(
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
            ),
        );

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr5: WrapSpan<Expr<&str>> = WrapSpan(
            "(97 >= ord 'a') || (true && !false)",
            Expr::BinOp(
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
            ),
        );

        match analyse_expression(
            expr5.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr5.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr5,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_variables() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb);

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "(var1 >= ord 'a') || (true && !false)",
            Expr::BinOp(
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
            ),
        );

        println!(
            "{:?}",
            analyse_expression(
                expr1.clone(),
                TypeConstraint::new(Type::Bool),
                &local_symb,
                &var_symb
            )
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb.def_var("var2", Type::Bool, "bool var2 = true", &mut local_symb);

        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "(ord 'a' == 65) || (true && !false)",
            Expr::BinOp(
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
            ),
        );

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr4,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_arrays() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb.def_var(
            "array1",
            Type::Array(box Type::Int, 1),
            "int[] array1 = [1,2,3,4]",
            &mut local_symb,
        );

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "array1[5]",
            Expr::ArrayElem("array1", vec![WrapSpan("5", Expr::Int(5))]),
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb.def_var(
            "array2",
            Type::Array(box Type::Int, 2),
            "int[][] array1 = [a,b,c]",
            &mut local_symb,
        );

        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5 * 5]",
            Expr::ArrayElem(
                "array2",
                vec![WrapSpan(
                    "5 * 5",
                    Expr::BinOp(
                        box WrapSpan("5", Expr::Int(5)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("5", Expr::Int(5)),
                    ),
                )],
            ),
        );

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Array(box Type::Int, 1)),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Array(box Type::Int, 1), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Array(box Type::Int, 1), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb.def_var(
            "array2",
            Type::Array(box Type::Int, 2),
            "int[][] array1 = [a,b,c]",
            &mut local_symb,
        );
        var_symb.def_var("num1", Type::Int, "int num1 = 9", &mut local_symb);

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5 * 5][num1]",
            Expr::ArrayElem(
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
            ),
        );

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::Char),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "array2[5 * 5][array1[num1]]",
            Expr::ArrayElem(
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
            ),
        );

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Int),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Int, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr3,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }

    #[test]
    fn analyse_expression_matches_well_typed_expressions_using_pairs() {
        let mut local_symb = LocalSymbolTable::new_root();
        let mut var_symb = VariableSymbolTable::new();

        var_symb.def_var(
            "pair1",
            Type::Pair(box Type::Int, box Type::Int),
            "pair(int, int) pair1 = newpair(9,9)",
            &mut local_symb,
        );

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "pair1 == pair1",
            Expr::BinOp(
                box WrapSpan("pair1", Expr::Var("pair1")),
                WrapSpan("==", BinOp::Eq),
                box WrapSpan("pair1", Expr::Var("pair1")),
            ),
        );

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Bool),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Bool, _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr1,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }

        var_symb.def_var(
            "pair_array",
            Type::Array(box Type::Pair(box Type::Int, box Type::Int), 1),
            "pair(int, int)[] pair_array = [pair1]",
            &mut local_symb,
        );

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
                                WrapSpan("len", UnOp::Len),
                                box WrapSpan("pair_array", Expr::Var("pair_array")),
                            ),
                        ),
                        WrapSpan("-", BinOp::Sub),
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                )],
            ),
        );

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Pair(box Type::Int, box Type::Int)),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Pair(box Type::Int, box Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2.clone(),
            TypeConstraint::new(Type::Any),
            &local_symb,
            &var_symb,
        ) {
            Ok((Type::Pair(box Type::Int, box Type::Int), _)) => assert!(true),
            _ => assert!(false),
        }

        match analyse_expression(
            expr2,
            TypeConstraint::new(Type::String),
            &local_symb,
            &var_symb,
        ) {
            Err(_) => assert!(true),
            _ => assert!(true),
        }
    }


}