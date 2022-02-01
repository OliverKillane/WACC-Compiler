//! Analysis of functions using the statement analysis
//!
//! Parameters have their own scope and are declared as errors separately

use super::{
    super::ast::{WrapSpan, Function, Param},
    semantic_errors::SemanticError,
    statement_analysis::analyse_block,
    symbol_table::{VariableSymbolTable, FunctionSymbolTable, LocalSymbolTable},
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
    var_symb: &mut VariableSymbolTable,
    fun_symb: &FunctionSymbolTable<'a>,
) -> Result<WrapSpan<'a, Function<'a, usize>>, (&'a str, Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>)>
{
    let mut param_errors = Vec::new();
    let mut param_correct = Vec::new();

    // create a new local symbol table at the top of the scope
    let mut local_symb = LocalSymbolTable::new_root();

    // add each parameter, checking the names
    for WrapSpan(param_span, Param(param_type, param_name)) in parameters {
        match var_symb.def_var(param_name, &param_type, param_span, &mut local_symb) {
            Ok(rename) => param_correct.push(WrapSpan(param_span, Param(param_type, rename))),
            Err(err) => param_errors.push(err),
        }
    }

    match analyse_block(
        block,
        fun_symb,
        &mut LocalSymbolTable::new_child(&local_symb),
        var_symb,
        &Some(ret_type.clone()),
    ) {
        Ok((block_ast, terminated)) => {
            if !terminated {
                param_errors.push(SemanticError::FunctionNoReturnOrExit(name))
            }

            if param_errors.len() == 0 {
                Ok(WrapSpan(
                    def_span,
                    Function(ret_type, name, param_correct, block_ast),
                ))
            } else {
                Err((name, vec![WrapSpan(def_span, param_errors)]))
            }
        }
        Err(mut errs) => {
            errs.push(WrapSpan(def_span, param_errors));
            Err((name, errs))
        }
    }
}
