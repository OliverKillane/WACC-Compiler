//! Analysis of functions using the statement analysis
//!
//! Parameters have their own scope and are declared as errors separately

use super::{
    super::ast::{Function, Param, WrapSpan},
    semantic_errors::SemanticError,
    statement_analysis::analyse_block,
    symbol_table::{FunctionSymbolTable, LocalSymbolTable, VariableSymbolTable},
};

/// Determine if there are any semantic errors in a function:
/// - If correct, return a renamed ast and update the variable symbol table
/// - If incorrect return function  name and the statements with errors
///   attached.
pub fn analyse_function<'a, 'b>(
    WrapSpan(def_span, Function(ret_type, name, parameters, block)): WrapSpan<
        'a,
        Function<'a, &'a str>,
    >,
    fun_symb: &FunctionSymbolTable<'a>,
) -> Result<
    (WrapSpan<'a, Function<'a, usize>>, VariableSymbolTable),
    (&'a str, Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>),
> {
    let mut var_symb = VariableSymbolTable::new();
    let mut function_errors = Vec::with_capacity(0);
    let mut param_correct = Vec::with_capacity(0);
    let mut errors = Vec::with_capacity(0);

    // create a new local symbol table at the top of the scope
    let mut local_symb = LocalSymbolTable::new_root();

    // add each parameter, checking the names
    for WrapSpan(param_span, Param(param_type, param_name)) in parameters {
        match var_symb.def_var(param_name, &param_type, param_span, &mut local_symb) {
            Ok(rename) => param_correct.push(WrapSpan(param_span, Param(param_type, rename))),
            Err(err) => function_errors.push(err),
        }
    }

    match analyse_block(
        block,
        fun_symb,
        &mut LocalSymbolTable::new_child(&local_symb),
        &mut var_symb,
        &Some(ret_type.clone()),
        &mut errors,
    ) {
        Some((block_ast, terminated)) => {
            if !terminated {
                function_errors.push(SemanticError::FunctionNoReturnOrExit(name))
            }

            if function_errors.len() == 0 {
                Ok((
                    WrapSpan(def_span, Function(ret_type, name, param_correct, block_ast)),
                    var_symb,
                ))
            } else {
                Err((name, vec![WrapSpan(def_span, function_errors)]))
            }
        }
        None => {
            errors.push(WrapSpan(def_span, function_errors));
            Err((name, errors))
        }
    }
}
