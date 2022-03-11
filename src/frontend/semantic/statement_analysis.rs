//! Analyse a block of instructions to get all errors, return either a modified
//! AST or the errors produced.

use crate::frontend::ast::ExprWrap;

use super::{
    super::ast::{ASTWrapper, AssignLhs, AssignRhs, Stat, StatWrap, Type},
    expression_analysis::analyse_expression,
    semantic_errors::{SemanticError, StatementErrors},
    symbol_table::{FunctionSymbolTable, LocalSymbolTable, VariableSymbolTable},
    type_constraints::{can_coerce, de_index},
};

/// Analyse a vector of statements.
/// Parameters:
/// - stats: the vector of statements (some of which can be blocks).
/// - local_symb: the local symbol table for the block's scope.
/// - var_symb: the renamed, flat symbol table. Local symbol table is used as a
///   key to access.
/// - ret_type (the return type, if there is none (e,.g main block), then it is
///   a None).
/// - errors (the vector of errors to add to, each elements is a statement and
///   contains the semantic errors occuring in that statement).
///
/// If the block was well formed, returns the renamed block and a boolean
/// determining if all paths through the block terminated.
pub fn analyse_block<'a, 'b>(
    stats: Vec<StatWrap<&'a str, &'a str>>,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
    ret_type: &Option<Type>,
    must_ret: bool,
    errors: &mut Vec<StatementErrors<'a>>,
) -> Option<Vec<StatWrap<Option<Type>, usize>>> {
    let mut any_errors = false;
    let mut correct: Vec<StatWrap<Option<Type>, usize>> = vec![];

    let mut stat_iter = stats.into_iter().peekable();
    while let Some(stat) = stat_iter.next() {
        if stat_iter.peek().is_none() && must_ret {
            let ASTWrapper(span, inner_stat) = stat;
            match inner_stat {
                Stat::While(_, _) => {
                    errors.push(ASTWrapper(
                        span,
                        vec![SemanticError::FunctionLastStatIsWhile(
                            span,
                            ret_type
                                .clone()
                                .expect("If it must return, it must be a function."),
                        )],
                    ));
                    analyse_statement(
                        ASTWrapper(span, inner_stat),
                        fun_symb,
                        local_symb,
                        var_symb,
                        ret_type,
                        true,
                        errors,
                    );
                    any_errors = true;
                }
                Stat::If(_, _, _) | Stat::Exit(_) | Stat::Return(_) | Stat::Block(_) => {
                    match analyse_statement(
                        ASTWrapper(span, inner_stat),
                        fun_symb,
                        local_symb,
                        var_symb,
                        ret_type,
                        true,
                        errors,
                    ) {
                        Some(renamed_ast) => correct.push(renamed_ast),
                        None => any_errors = true,
                    }
                }
                other => {
                    analyse_statement(
                        ASTWrapper(span, other),
                        fun_symb,
                        local_symb,
                        var_symb,
                        ret_type,
                        true,
                        errors,
                    );
                    errors.push(ASTWrapper(
                        span,
                        vec![SemanticError::FunctionNoReturnOrExit(
                            span,
                            ret_type
                                .clone()
                                .expect("If it must return, it must be a function."),
                        )],
                    ));
                    any_errors = true
                }
            }
        } else {
            match analyse_statement(
                stat, fun_symb, local_symb, var_symb, ret_type, false, errors,
            ) {
                Some(renamed_ast) => correct.push(renamed_ast),
                None => any_errors = true,
            }
        }
    }

    if any_errors {
        None
    } else {
        Some(correct)
    }
}

/// Analyse a statement, resulting in either the errors for a statement or a
/// correct renamed ast.
fn analyse_statement<'a, 'b>(
    ASTWrapper(span, stat): StatWrap<&'a str, &'a str>,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
    ret_type: &Option<Type>,
    must_ret: bool,
    errors: &mut Vec<StatementErrors<'a>>,
) -> Option<StatWrap<Option<Type>, usize>> {
    let mut add_error = |err| {
        errors.push(err);
        None
    };

    match stat {
        Stat::Skip => Some(ASTWrapper(None, Stat::Skip)),
        Stat::Def(var_type, name, rhs) => {
            match var_symb.def_var(name, &var_type, span, local_symb) {
                Ok(rename) => {
                    let mut rhs_errors = vec![];
                    match analyse_rhs(
                        &var_type,
                        rhs,
                        fun_symb,
                        local_symb,
                        var_symb,
                        &mut rhs_errors,
                    ) {
                        Some(renamed_rhs) => {
                            Some(ASTWrapper(None, Stat::Def(var_type, rename, renamed_rhs)))
                        }
                        None => add_error(ASTWrapper(span, rhs_errors)),
                    }
                }
                Err(err) => add_error(ASTWrapper(span, vec![err])),
            }
        }
        Stat::Assign(lhs, rhs) => {
            let mut stat_errors = Vec::with_capacity(0);
            match analyse_lhs(lhs, var_symb, local_symb, &mut stat_errors) {
                Some((left_type, lhs_ast)) => {
                    match analyse_rhs(
                        &left_type,
                        rhs,
                        fun_symb,
                        local_symb,
                        var_symb,
                        &mut stat_errors,
                    ) {
                        Some(rhs_ast) => Some(ASTWrapper(None, Stat::Assign(lhs_ast, rhs_ast))),
                        None => add_error(ASTWrapper(span, stat_errors)),
                    }
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::Read(lhs) => {
            let mut stat_errors = vec![];
            match analyse_lhs(lhs, var_symb, local_symb, &mut stat_errors) {
                Some((lhs_type, lhs_ast)) => {
                    if can_coerce(&Type::Int, &lhs_type) || can_coerce(&Type::Char, &lhs_type) {
                        Some(ASTWrapper(None, Stat::Read(lhs_ast)))
                    } else {
                        add_error(ASTWrapper(
                            span,
                            vec![SemanticError::ReadStatementMismatch(span, lhs_type)],
                        ))
                    }
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        // If the expression is valid, check the type can coerce to some
        // pointer type, and if so add to correct statements
        //
        // If an invalid expression or wrong expression type, add to errors
        // for statement.
        Stat::Free(expr @ ASTWrapper(expr_span, _)) => {
            let mut stat_errors = vec![];
            match analyse_expression(expr, local_symb, var_symb, &mut stat_errors) {
                Some(ASTWrapper(Some(expr_type), expr_ast)) => {
                    if can_coerce(&Type::Pair(box Type::Any, box Type::Any), &expr_type)
                        || can_coerce(&Type::Array(box Type::Any, 1), &expr_type)
                    {
                        Some(ASTWrapper(
                            None,
                            Stat::Free(ASTWrapper(Some(expr_type), expr_ast)),
                        ))
                    } else {
                        add_error(ASTWrapper(
                            span,
                            vec![SemanticError::FreeStatementMismatch(expr_span, expr_type)],
                        ))
                    }
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::Return(expr @ ASTWrapper(expr_span, _)) => {
            let mut stat_errors = vec![];
            match analyse_expression(expr, local_symb, var_symb, &mut stat_errors) {
                Some(ASTWrapper(Some(expr_type), expr_ast)) => match &ret_type {
                    Some(return_type) => {
                        if can_coerce(return_type, &expr_type) {
                            Some(ASTWrapper(
                                None,
                                Stat::Return(ASTWrapper(Some(expr_type), expr_ast)),
                            ))
                        } else {
                            add_error(ASTWrapper(
                                span,
                                vec![SemanticError::InvalidFunctionReturn(
                                    expr_span,
                                    return_type.clone(),
                                    expr_type,
                                )],
                            ))
                        }
                    }
                    None => add_error(ASTWrapper(
                        span,
                        vec![SemanticError::ReturnStatementMisplaced(span)],
                    )),
                },
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::Exit(expr @ ASTWrapper(expr_span, _)) => {
            let mut stat_errors = vec![];
            match analyse_expression(expr, local_symb, var_symb, &mut stat_errors) {
                Some(ASTWrapper(Some(expr_type), expr_ast)) => {
                    if can_coerce(&Type::Int, &expr_type) {
                        Some(ASTWrapper(
                            None,
                            Stat::Exit(ASTWrapper(Some(expr_type), expr_ast)),
                        ))
                    } else {
                        add_error(ASTWrapper(
                            span,
                            vec![SemanticError::ExitStatementMismatch(expr_span, expr_type)],
                        ))
                    }
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::Print(expr) => {
            let mut stat_errors = vec![];
            match analyse_expression(expr, local_symb, var_symb, &mut stat_errors) {
                Some(expr_renamed) => {
                    // any type can be printed, so no coercion check
                    Some(ASTWrapper(None, Stat::Print(expr_renamed)))
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::PrintLn(expr) => {
            let mut stat_errors = vec![];
            match analyse_expression(expr, local_symb, var_symb, &mut stat_errors) {
                Some(expr_renamed) => {
                    // any type can be printed, so no coercion check
                    Some(ASTWrapper(None, Stat::PrintLn(expr_renamed)))
                }
                None => add_error(ASTWrapper(span, stat_errors)),
            }
        }
        Stat::If(cond @ ASTWrapper(cond_span, _), if_block, else_block) => {
            let mut stat_errors = vec![];
            let cond_valid = match analyse_expression(cond, local_symb, var_symb, &mut stat_errors)
            {
                Some(ASTWrapper(Some(cond_type), cond_ast)) => {
                    if can_coerce(&Type::Bool, &cond_type) {
                        Some(ASTWrapper(Some(Type::Bool), cond_ast))
                    } else {
                        errors.push(ASTWrapper(
                            span,
                            vec![SemanticError::InvalidIfCondition(cond_span, cond_type)],
                        ));
                        None
                    }
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => {
                    errors.push(ASTWrapper(span, stat_errors));
                    None
                }
            };

            let if_block = analyse_block(
                if_block,
                fun_symb,
                &mut LocalSymbolTable::new_child(local_symb),
                var_symb,
                ret_type,
                must_ret,
                errors,
            );

            let else_block = analyse_block(
                else_block,
                fun_symb,
                &mut LocalSymbolTable::new_child(local_symb),
                var_symb,
                ret_type,
                must_ret,
                errors,
            );

            if let (Some(cond_ast), Some(if_block_ast), Some(else_block_ast)) =
                (cond_valid, if_block, else_block)
            {
                Some(ASTWrapper(
                    None,
                    Stat::If(cond_ast, if_block_ast, else_block_ast),
                ))
            } else {
                None
            }
        }
        Stat::While(cond @ ASTWrapper(cond_span, _), loop_block) => {
            let mut stat_errors = vec![];
            let condition_valid =
                match analyse_expression(cond, local_symb, var_symb, &mut stat_errors) {
                    Some(ASTWrapper(Some(cond_type), cond_ast)) => {
                        if can_coerce(&Type::Bool, &cond_type) {
                            Some(ASTWrapper(Some(Type::Bool), cond_ast))
                        } else {
                            errors.push(ASTWrapper(
                                span,
                                vec![SemanticError::InvalidWhileCondition(cond_span, cond_type)],
                            ));
                            None
                        }
                    }
                    Some(ASTWrapper(None, _)) => {
                        panic!("Correctly typed expressions must always have a type")
                    }
                    None => {
                        errors.push(ASTWrapper(span, stat_errors));
                        None
                    }
                };

            if let (Some(cond), Some(block_ast)) = (
                condition_valid,
                analyse_block(
                    loop_block,
                    fun_symb,
                    &mut LocalSymbolTable::new_child(local_symb),
                    var_symb,
                    ret_type,
                    must_ret,
                    errors,
                ),
            ) {
                Some(ASTWrapper(None, Stat::While(cond, block_ast)))
            } else {
                None
            }
        }
        Stat::Block(block) => analyse_block(
            block,
            fun_symb,
            &mut LocalSymbolTable::new_child(local_symb),
            var_symb,
            ret_type,
            must_ret,
            errors,
        )
        .map(|block_ast| ASTWrapper(None, Stat::Block(block_ast))),
        Stat::VoidCall(ASTWrapper(fun_name, fun_name_string), args) => {
            let mut stat_errors = vec![];
            let void_call = analyse_call(fun_name, fun_name_string, args, &Type::Any, fun_symb, local_symb, var_symb, &mut stat_errors).map(|(t, exprs)| ASTWrapper(None, Stat::VoidCall(t, exprs)));
            errors.push(ASTWrapper(span, stat_errors));
            void_call
        },
    }
}

/// Analyse the right hand side of a statement (e.g an expression, function
/// call or array literal).
///
/// Renames the variables in this part of the ast, and updates the variable and
/// local symbol tables.
///
/// On success returns an option of the renamed right hand side (variable renaming).
///
/// Adds any errors to the vector of semantic errors (representing the semantic
/// errors in a single statement).
fn analyse_rhs<'a, 'b>(
    expected: &Type,
    rhs: AssignRhs<&'a str, &'a str>,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
    errors: &mut Vec<SemanticError<'a>>,
) -> Option<AssignRhs<Option<Type>, usize>> {
    match rhs {
        AssignRhs::Expr(expr @ ASTWrapper(expr_span, _)) => {
            match analyse_expression(expr, local_symb, var_symb, errors) {
                Some(ASTWrapper(Some(expr_t), expr_ast)) => {
                    if can_coerce(expected, &expr_t) {
                        // if the type is coercible, the entire expression should go to this type
                        Some(AssignRhs::Expr(ASTWrapper(
                            Some(expected.clone()),
                            expr_ast,
                        )))
                    } else {
                        errors.push(SemanticError::InvalidType(
                            expr_span,
                            expected.clone(),
                            expr_t,
                        ));
                        None
                    }
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => None,
            }
        }
        AssignRhs::Array(ASTWrapper(span, vals)) => {
            // check if assignment is possible
            let vals_len = vals.len();

            // check the inner expressions
            let renamed_vals = vals
                .into_iter()
                .filter_map(|expr| analyse_expression(expr, local_symb, var_symb, errors))
                .collect::<Vec<_>>();

            if renamed_vals.len() == vals_len {
                // all expressions are valid, get the type they can coerce to.

                // the array literal type, used as a temporary to hold the types
                // of the expressions in the literal.
                let mut arr_lit_type = None;

                for ASTWrapper(expr_type, _) in renamed_vals.iter() {
                    let expr_type = expr_type
                        .clone()
                        .expect("Correctly typed expressions must always have a type");
                    match &arr_lit_type {
                        Some(t) => {
                            if can_coerce(t, &expr_type) {
                            } else if can_coerce(&expr_type, t) {
                                arr_lit_type = Some(expr_type.clone())
                            } else {
                                errors.push(SemanticError::InvalidArrayLiteral(
                                    span,
                                    renamed_vals.into_iter().map(|ASTWrapper(expr_t, _)| expr_t.expect("Correctly typed expressions must always have a type")).collect(),
                                ));
                                return None;
                            }
                        }
                        None => arr_lit_type = Some(expr_type.clone()),
                    }
                }

                // determine the array type, either an array of the common
                // coerce type, or if the array is empty then any array
                let array_lit_type = match arr_lit_type {
                    Some(t) => t,
                    None => Type::Any,
                };

                // check the type of the array literal can coerce to the type
                // of the assignment.

                if let Some(expected_item_type) = de_index(expected, 1) {
                    if can_coerce(&expected_item_type, &array_lit_type) {
                        Some(AssignRhs::Array(ASTWrapper(
                            Some(expected.clone()),
                            renamed_vals
                                .into_iter()
                                .map(|ASTWrapper(_, item_ast)| {
                                    ASTWrapper(Some(expected_item_type.clone()), item_ast)
                                })
                                .collect::<Vec<_>>(),
                        )))
                    } else {
                        errors.push(SemanticError::InvalidType(
                            span,
                            expected.clone(),
                            array_lit_type,
                        ));
                        None
                    }
                } else {
                    None
                }
            } else {
                // there are errors in the expressions, so cannot proceed further
                None
            }
        }
        AssignRhs::Call(ASTWrapper(fun_name, fun_name_string), args) => {
            analyse_call(fun_name, fun_name_string, args, expected, fun_symb, local_symb, var_symb, errors).map(|(t, exprs)| AssignRhs::Call(t, exprs))
        }
    }
}

fn analyse_call<'a, 'b>(fun_name: &'a str, fun_name_string: String, args: Vec<ExprWrap<&'a str, &'a str>>, expected: &Type,
    fun_symb: &FunctionSymbolTable<'a>,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
    errors: &mut Vec<SemanticError<'a>>,) -> Option<(ASTWrapper<Option<Type>, String>, Vec<ExprWrap<Option<Type>, usize>>)> {
    match fun_symb.get_fun(fun_name) {
        Some((ret_type, param_types)) => {
            let mut correct = vec![];
            let args_len = args.len();

            if !can_coerce(expected, &ret_type) {
                errors.push(SemanticError::InvalidCallType(
                    fun_name,
                    expected.clone(),
                    ret_type,
                ));
            }

            if param_types.len() != args_len {
                errors.push(SemanticError::FunctionParametersLengthMismatch(
                    fun_name,
                    param_types.len(),
                    args_len,
                ));
            } else {
                for (expr @ ASTWrapper(param_span, _), (param_name, param_type)) in
                    args.into_iter().zip(param_types.into_iter())
                {
                    if let Some(ASTWrapper(Some(expr_type), param_ast)) =
                        analyse_expression(expr, local_symb, var_symb, errors)
                    {
                        if can_coerce(&param_type, &expr_type) {
                            correct.push(ASTWrapper(Some(param_type.clone()), param_ast))
                        } else {
                            errors.push(SemanticError::FunctionArgumentTypeInvalid(
                                param_span, param_name, param_type, expr_type,
                            ))
                        }
                    }
                }
            }

            if errors.is_empty() {
                Some((
                    ASTWrapper(Some(expected.clone()), fun_name_string),
                    correct)
                )
            } else {
                None
            }
        }
        None => {
            // even if the function is undefined, we can find issues in its arguments
            for expr in args.into_iter() {
                analyse_expression(expr, local_symb, var_symb, errors);
            }
            errors.push(SemanticError::UndefinedFunction(fun_name));
            None
        }
    }
}

/// Analyse the left hand side of a statement (e.g a variable assignment)
///
/// Renames the variables in this part of the ast, and updates the variable and
/// local symbol tables.
///
/// On success returns an option of:
/// - Type expected from any rhs assigning to it.
/// - The renamed left hand side (variable renaming).
///
/// Adds any errors to the vector of semantic errors (representing the semantic
/// errors in a single statement).
fn analyse_lhs<'a, 'b>(
    lhs: AssignLhs<&'a str, &'a str>,
    var_symb: &mut VariableSymbolTable,
    local_symb: &LocalSymbolTable<'a, 'b>,
    errors: &mut Vec<SemanticError<'a>>,
) -> Option<(Type, AssignLhs<Option<Type>, usize>)> {
    match lhs {
        AssignLhs::Var(var_name) => match var_symb.get_type(var_name, local_symb) {
            Some((rename, var_type)) => Some((var_type, AssignLhs::Var(rename))),
            None => {
                errors.push(SemanticError::UndefinedVariableAssignment(var_name));
                None
            }
        },
        AssignLhs::ArrayElem(var_name, index_exprs) => {
            let mut correct = vec![];
            let index_exprs_len = index_exprs.len();
            let dim = index_exprs.len();

            // check each expression in the indexes (adding expression errors
            // and checking type).
            for expr @ ASTWrapper(expr_span, _) in index_exprs.into_iter() {
                if let Some(ASTWrapper(Some(expr_type), expr_ast)) =
                    analyse_expression(expr, local_symb, var_symb, errors)
                {
                    if can_coerce(&Type::Int, &expr_type) {
                        correct.push(ASTWrapper(Some(expr_type), expr_ast));
                    } else {
                        errors.push(SemanticError::InvalidIndex(expr_span, expr_type.clone()))
                    }
                }
            }

            // Check the variable being indexed is correct.
            match var_symb.get_type(var_name, local_symb) {
                Some((rename, var_type)) => match de_index(&var_type, dim) {
                    Some(expected_type) => {
                        if correct.len() == index_exprs_len {
                            Some((expected_type, AssignLhs::ArrayElem(rename, correct)))
                        } else {
                            None
                        }
                    }
                    // the variable is invalidly indexed (e.g bool a = true; a[0] = ...)
                    None => {
                        errors.push(SemanticError::InvalidVariableType(
                            var_name,
                            Type::Array(box Type::Any, dim),
                            var_type,
                        ));
                        None
                    }
                },
                None => {
                    // the variable is undefined
                    errors.push(SemanticError::UndefinedVariableAssignment(var_name));
                    None
                }
            }
        }
        AssignLhs::PairFst(expr @ ASTWrapper(expr_span, _)) => {
            match analyse_expression(expr, local_symb, var_symb, errors) {
                Some(ASTWrapper(Some(Type::Pair(box t, t2)), renamed_ast)) => Some((
                    t.clone(),
                    AssignLhs::PairFst(ASTWrapper(Some(Type::Pair(box t, t2)), renamed_ast)),
                )),
                Some(ASTWrapper(Some(index_type), _)) => {
                    errors.push(SemanticError::InvalidType(
                        expr_span,
                        Type::Pair(box Type::Any, box Type::Any),
                        index_type,
                    ));
                    None
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => None,
            }
        }
        AssignLhs::PairSnd(expr @ ASTWrapper(expr_span, _)) => {
            match analyse_expression(expr, local_symb, var_symb, errors) {
                Some(ASTWrapper(Some(Type::Pair(t2, box t)), renamed_ast)) => Some((
                    t.clone(),
                    AssignLhs::PairSnd(ASTWrapper(Some(Type::Pair(t2, box t)), renamed_ast)),
                )),
                Some(ASTWrapper(Some(index_type), _)) => {
                    errors.push(SemanticError::InvalidType(
                        expr_span,
                        Type::Pair(box Type::Any, box Type::Any),
                        index_type,
                    ));
                    None
                }
                Some(ASTWrapper(None, _)) => {
                    panic!("Correctly typed expressions must always have a type")
                }
                None => None,
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
                    ASTWrapper("fun1", String::from("fun1")),
                    vec![ASTWrapper("1", Expr::Int(3)), ASTWrapper("4", Expr::Int(4))]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Call(
                ASTWrapper(Some(Type::Int), String::from("fun1")),
                vec![
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(4))
                ]
            ))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Int,
                AssignRhs::Call(ASTWrapper("fun2", String::from("fun2")), vec![]),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Call(
                ASTWrapper(Some(Type::Int), String::from("fun2")),
                vec![]
            ))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Char,
                AssignRhs::Call(
                    ASTWrapper("fun3", String::from("fun3")),
                    vec![ASTWrapper(
                        "newpair(5,5)",
                        Expr::BinOp(
                            box ASTWrapper("5", Expr::Int(5)),
                            BinOp::Newpair,
                            box ASTWrapper("5", Expr::Int(5))
                        )
                    )]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Call(
                ASTWrapper(Some(Type::Char), String::from("fun3")),
                vec![ASTWrapper(
                    Some(Type::Pair(box Type::Int, box Type::Int)),
                    Expr::BinOp(
                        box ASTWrapper(Some(Type::Int), Expr::Int(5)),
                        BinOp::Newpair,
                        box ASTWrapper(Some(Type::Int), Expr::Int(5))
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
                AssignRhs::Call(
                    ASTWrapper("fun3", String::from("fun3")),
                    vec![ASTWrapper("a", Expr::Var("a"))]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Call(
                ASTWrapper(Some(Type::Char), String::from("fun3")),
                vec![ASTWrapper(
                    Some(Type::Pair(box Type::Int, box Type::Int)),
                    Expr::Var(0)
                )]
            ))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Int,
                AssignRhs::Call(
                    ASTWrapper("fun4", String::from("fun4")),
                    vec![ASTWrapper(
                        "fst a",
                        Expr::UnOp(UnOp::Fst, box ASTWrapper("a", Expr::Var("a")))
                    )]
                ),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Call(
                ASTWrapper(Some(Type::Int), String::from("fun4")),
                vec![ASTWrapper(
                    Some(Type::Int),
                    Expr::UnOp(
                        UnOp::Fst,
                        box ASTWrapper(
                            Some(Type::Pair(box Type::Int, box Type::Int)),
                            Expr::Var(0)
                        )
                    )
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
        let mut errors1 = vec![];
        match analyse_rhs(
            &Type::Char,
            AssignRhs::Call(
                ASTWrapper("fun1", String::from("fun1")),
                vec![ASTWrapper("pair_a", Expr::Var("pair_a"))],
            ),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
            &mut errors1,
        ) {
            None => {
                assert!(errors1.contains(&SemanticError::InvalidCallType(
                    "fun1",
                    Type::Char,
                    Type::Int
                )));
                assert!(
                    errors1.contains(&SemanticError::FunctionParametersLengthMismatch(
                        "fun1", 2, 1
                    ))
                );
            }
            _ => assert!(false),
        }

        // Errors:
        // - The first argument is a pair(int,int) should be an int
        // - The second argument is a bool, should be an int
        let mut errors2 = vec![];
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Call(
                ASTWrapper("fun1", String::from("fun1")),
                vec![
                    ASTWrapper("pair_a", Expr::Var("pair_a")),
                    ASTWrapper("true", Expr::Bool(true)),
                ],
            ),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
            &mut errors2,
        ) {
            None => {
                // FunctionArgumentTypeInvalid("a", "a", Int, Pair(Int, Int)), FunctionArgumentTypeInvalid("true", "b", Int, Bool)
                assert!(
                    errors2.contains(&SemanticError::FunctionArgumentTypeInvalid(
                        "pair_a",
                        "a",
                        Type::Int,
                        Type::Pair(box Type::Int, box Type::Int)
                    ))
                );
                assert!(
                    errors2.contains(&SemanticError::FunctionArgumentTypeInvalid(
                        "true",
                        "b",
                        Type::Int,
                        Type::Bool
                    ))
                );
            }
            _ => assert!(false),
        }

        // Errors:
        // - The first argument is a pair(int,int) should be an int
        // - The second argument is a bool, should be an int
        let mut errors3 = vec![];
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Call(
                ASTWrapper("fun1", String::from("fun1")),
                vec![
                    ASTWrapper("pair_j", Expr::Var("pair_j")),
                    ASTWrapper("true", Expr::Bool(true)),
                ],
            ),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
            &mut errors3,
        ) {
            None => {
                assert!(errors3.contains(&SemanticError::UndefinedVariableUse("pair_j")));
                assert!(
                    errors3.contains(&SemanticError::FunctionArgumentTypeInvalid(
                        "true",
                        "b",
                        Type::Int,
                        Type::Bool
                    ))
                );
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
                AssignRhs::Array(ASTWrapper(
                    "[3,3,3,3]",
                    vec![
                        ASTWrapper("3", Expr::Int(3)),
                        ASTWrapper("3", Expr::Int(3)),
                        ASTWrapper("3", Expr::Int(3)),
                        ASTWrapper("3", Expr::Int(3))
                    ]
                )),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Array(ASTWrapper(
                Some(Type::Array(box Type::Int, 1)),
                vec![
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3))
                ]
            )))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Array(box Type::Int, 1),
                AssignRhs::Array(ASTWrapper(
                    "[a,3,3,3]",
                    vec![
                        ASTWrapper("a", Expr::Var("a")),
                        ASTWrapper("3", Expr::Int(3)),
                        ASTWrapper("3", Expr::Int(3)),
                        ASTWrapper("3", Expr::Int(3))
                    ]
                )),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Array(ASTWrapper(
                Some(Type::Array(box Type::Int, 1)),
                vec![
                    ASTWrapper(Some(Type::Int), Expr::Var(0)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3)),
                    ASTWrapper(Some(Type::Int), Expr::Int(3))
                ]
            )))
        );
        assert_eq!(
            analyse_rhs(
                &Type::Array(box Type::Char, 1),
                AssignRhs::Array(ASTWrapper("[b]", vec![ASTWrapper("b", Expr::Var("b"))])),
                &fun_symb,
                &mut local_symb,
                &mut var_symb,
                &mut vec![]
            ),
            Some(AssignRhs::Array(ASTWrapper(
                Some(Type::Array(box Type::Char, 1)),
                vec![ASTWrapper(Some(Type::Char), Expr::Var(1))]
            )))
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
        let mut errors1 = vec![];
        match analyse_rhs(
            &Type::Int,
            AssignRhs::Array(ASTWrapper(
                "[3,3,3,3]",
                vec![
                    ASTWrapper("3", Expr::Int(3)),
                    ASTWrapper("3", Expr::Int(3)),
                    ASTWrapper("3", Expr::Int(3)),
                    ASTWrapper("3", Expr::Int(3)),
                ],
            )),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
            &mut errors1,
        ) {
            None => {
                assert!(errors1.contains(&SemanticError::InvalidType(
                    "[3,3,3,3]",
                    Type::Int,
                    Type::Array(box Type::Int, 1)
                )))
            }
            _ => assert!(false),
        }

        // variable b is a char not an int
        let mut errors2 = vec![];
        match analyse_rhs(
            &Type::Array(box Type::Int, 1),
            AssignRhs::Array(ASTWrapper(
                "[b,3,3,3]",
                vec![
                    ASTWrapper("b", Expr::Var("b")),
                    ASTWrapper("3", Expr::Int(3)),
                    ASTWrapper("3", Expr::Int(3)),
                    ASTWrapper("3", Expr::Int(3)),
                ],
            )),
            &fun_symb,
            &mut local_symb,
            &mut var_symb,
            &mut errors2,
        ) {
            None => {
                assert!(errors2.contains(&SemanticError::InvalidArrayLiteral(
                    "[b,3,3,3]",
                    vec![Type::Char, Type::Int, Type::Int, Type::Int]
                )))
            }
            _ => assert!(false),
        }
    }
}
