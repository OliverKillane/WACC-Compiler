//! Analyses the ast and flattens naming to produce a variable symbol table.
//!
//! ## Errors
//! All error types are contained within the [semantic errors](semantic_errors)
//! submodule.
//!
//! When any errors occurs the semantic analyser outputs a tuple:
//! (function definition errors, main body errors, errors per function)
//!
//! Push forward is deeply engrained into the analysis, a single statement or
//! expression can contain multiple errors, and every effort is made to
//! determine errors in the absence of information due to other errors (e.g
//! expression errors in assignments where the variable is undefined).
//!
//! The spans associated with errors are returned, this ensures that the span
//! of erroneous code in the source is kept for error printing later.
//!
//! If no errors occur, the types (if relevant) of reach ast node is used in
//! the [ASTWrapper]. The type is contained for:
//! - Function names in calls
//! - All expressions (will be some)
//! - Right hand sides of assignments
//!
//! For each statement the errors is given as:
//! (span of statement, [list of semantic errors (can contain internal spans)])
//! this is covered in more detail in [semantic errors](semantic_errors).
//!
//! ## Functions
//! Functions definitions are analysed by the function symbol table generation in
//! [symbol tables](symbol_table). However the parameters are analysed in
//! [function analysis](function_analysis) which creates the root local scope
//! symbol table and makes use of statement block analysis.
//!
//! ## Statement blocks
//! Parsed in collections of blocks recursively, with termination and return
//! type checking. Uses the push forwards strategy extensively.
//!
//! ## Expressions
//! Expressions are analysed through the operator and type matching in
//! [type constraints](type_constraints). This allows for new operators to be
//! implemented by simply adding their types to the relevant table.
//!
//! Expressions are also push forwards, and propagate back many errors through
//! the expression tree traversal.

mod error_conversion;
mod expression_analysis;
mod function_analysis;
pub mod semantic_errors;
mod statement_analysis;
pub mod symbol_table;
mod type_constraints;

use std::collections::HashMap;
use rayon::prelude::*;

use self::{
    error_conversion::convert_errors,
    function_analysis::analyse_function,
    statement_analysis::analyse_block,
    symbol_table::{get_fn_symbols, LocalSymbolTable, VariableSymbolTable},
};

use super::{
    ast::{ASTWrapper, Function, Program, Type},
    error::Summary,
};

/// Analyses a program, either returning a flat variable and function symbol table
/// and an ast, or a tuple of error (function defs, main body, function errors)
///
/// On success returns:
/// - Main program body and variable symbol table
/// - Hashmap of string function identifier to the function body and its
///   variable symbol table
///
/// On Failure Returns:
/// - Vector of semantic error summary cells
/// - Vector of syntax error summary cells
#[allow(clippy::type_complexity)]
pub fn analyse_semantics<'a>(
    Program(fn_defs, main_block): Program<&'a str, &'a str>,
) -> Result<
    (
        Program<Option<Type>, usize>,
        HashMap<String, VariableSymbolTable>,
        VariableSymbolTable,
    ),
    Summary<'a>,
> {
    // get function definitions
    let (fun_symb, filtered_fn_defs, fun_def_errs) = get_fn_symbols(fn_defs);

    // traverse and analyse functions
    let (e, (fst, fs)): (Vec<_>, (Vec<_>, Vec<_>)) = filtered_fn_defs.into_par_iter().map(|ASTWrapper(fun_name, fun)| {
        match analyse_function(ASTWrapper(fun_name, fun), &fun_symb) {
            Ok((function, var_symb)) => {
                let ASTWrapper(_, Function(_, ASTWrapper(_, fun_name), _, _)) = &function;
                (None, (Some((fun_name.clone(), var_symb)), Some(function)))
            }
            Err(fun_err) => (Some(fun_err), (None, None)),
        }
    }).unzip();

    let errors: Vec<_> = e.into_iter().filter_map(|x| x).collect();
    let fun_symbol_tables: HashMap<_, _> = fst.into_iter().filter_map(|x| x).collect();
    let functions: Vec<_> = fs.into_iter().filter_map(|x| x).collect();

    // analyse main code block
    let mut main_var_symb = VariableSymbolTable::new();
    let mut main_errors = Vec::new();

    match analyse_block(
        main_block,
        &fun_symb,
        &mut LocalSymbolTable::new_root(),
        &mut main_var_symb,
        &None,
        false,
        &mut main_errors,
    ) {
        Some(block_ast) => {
            if errors.is_empty() && fun_def_errs.is_empty() {
                Ok((
                    Program(functions, block_ast),
                    fun_symbol_tables,
                    main_var_symb,
                ))
            } else {
                Err(convert_errors(fun_def_errs, vec![], errors))
            }
        }
        None => Err(convert_errors(fun_def_errs, main_errors, errors)),
    }
}
