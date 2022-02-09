//! Analysis of functions (parameters and declaration as well as contained
//! block).
//!
//! Parameters have their own scope and are declared as errors separately.
//!
//! each function has its own flat symbol table created and checked through
//! successive chained local symbol tables. Errors in the parameters and the
//! function declaration is general are associated with the span of the
//! function.
//!
//! All other errors are associated with spans of statements within the
//! function.

use super::{
    super::ast::{Function, Param, WrapSpan},
    semantic_errors::{SemanticError, StatementErrors},
    statement_analysis::analyse_block,
    symbol_table::{FunctionSymbolTable, LocalSymbolTable, VariableSymbolTable},
};

type FunctionErrors<'a> = (&'a str, Vec<StatementErrors<'a>>);
type FunctionAnalysis<'a> = (WrapSpan<'a, Function<'a, usize>>, VariableSymbolTable);

/// Determine if there are any semantic errors in a function:
/// - If correct, return a renamed ast and update the variable symbol table
/// - If incorrect return function  name and the statements with errors
///   attached.
pub fn analyse_function<'a>(
    WrapSpan(def_span, Function(ret_type, name, parameters, block)): WrapSpan<
        'a,
        Function<'a, &'a str>,
    >,
    fun_symb: &FunctionSymbolTable<'a>,
) -> Result<FunctionAnalysis<'a>, FunctionErrors<'a>> {
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

            if function_errors.is_empty() {
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
