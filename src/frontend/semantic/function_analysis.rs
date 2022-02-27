//! Analysis of functions (parameters and declaration as well as contained
//! block).
//!
//! Parameters have their own scope and are declared as errors separately.
//!
//! Each function has its own flat symbol table created and checked through
//! successive chained local symbol tables. Errors in the parameters and the
//! function declaration is general are associated with the span of the
//! function.
//!
//! All other errors are associated with spans of statements within the
//! function.

use crate::frontend::ast::FunWrap;

use super::{
    super::ast::{ASTWrapper, Function, Param, Type},
    semantic_errors::StatementErrors,
    statement_analysis::analyse_block,
    symbol_table::{FunctionSymbolTable, LocalSymbolTable, VariableSymbolTable},
};

/// An error result from a function analysis, containing the function name, and
/// the statement errors contained.
type FunctionErrors<'a> = (&'a str, Vec<StatementErrors<'a>>);

/// A correct result from a function analysis, containing the function, as well
/// as the flat symbol table associated with it.
type FunctionAnalysis<'a> = (FunWrap<Option<Type>, usize>, VariableSymbolTable);

/// Determine if there are any semantic errors in a function:
/// - If correct, return a renamed ast and update the variable symbol table
/// - If incorrect return function  name and the statements with errors
///   attached.
pub fn analyse_function<'a>(
    ASTWrapper(def_span, Function(ret_type, ASTWrapper(name, string_name), parameters, block)): FunWrap<&'a str, &'a str>,
    fun_symb: &FunctionSymbolTable<'a>,
) -> Result<FunctionAnalysis<'a>, FunctionErrors<'a>> {
    let mut var_symb = VariableSymbolTable::new();
    let mut function_errors = Vec::new();
    let mut param_correct = Vec::new();
    let mut errors = Vec::new();

    // create a new local symbol table at the top of the scope
    let mut local_symb = LocalSymbolTable::new_root();

    // add each parameter, checking the names
    for ASTWrapper(param_span, Param(param_type, param_name)) in parameters {
        match var_symb.def_var(param_name, &param_type, param_span, &mut local_symb) {
            Ok(rename) => param_correct.push(ASTWrapper(None, Param(param_type, rename))),
            Err(err) => function_errors.push(err),
        }
    }

    if !function_errors.is_empty() {
        errors.push(ASTWrapper(def_span, function_errors))
    }

    match analyse_block(
        block,
        fun_symb,
        &mut LocalSymbolTable::new_child(&local_symb),
        &mut var_symb,
        &Some(ret_type.clone()),
        true,
        &mut errors,
    ) {
        Some(block_ast) => {
            if errors.is_empty() {
                Ok((
                    ASTWrapper(
                        None,
                        Function(
                            ret_type,
                            ASTWrapper(None, string_name),
                            param_correct,
                            block_ast,
                        ),
                    ),
                    var_symb,
                ))
            } else {
                Err((name, errors))
            }
        }
        None => Err((name, errors)),
    }
}
