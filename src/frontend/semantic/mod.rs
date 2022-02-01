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

mod expression_analysis;
mod function_analysis;
mod type_constraints;
mod statement_analysis;
pub mod semantic_errors;
pub mod symbol_table;

use self::{
    function_analysis::analyse_function,
    semantic_errors::SemanticError,
    statement_analysis::analyse_block,
    symbol_table::{get_fn_symbols, LocalSymbolTable, VariableSymbolTable},
};

use super::ast::{Program, WrapSpan};

/// Analyses a program, either returning a flat variable and function symbol table and an ast, or a tuple of error (function defs, main body, function errors)
pub fn analyse_semantics<'a>(
    Program(fn_defs, main_block): Program<'a, &'a str>,
) -> Result<
    (Program<'a, usize>, VariableSymbolTable),
    (
        Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>,
        Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>,
        Vec<(&str, Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>)>,
    ),
> {
    // get function definitions
    let (fun_symb, filtered_fn_defs, fun_def_errs) = get_fn_symbols(fn_defs);

    // create flat symbol table
    let mut var_symb = VariableSymbolTable::new();

    let mut errors = Vec::new();
    let mut correct = Vec::new();

    // traverse and analyse functions
    for function in filtered_fn_defs {
        match analyse_function(function, &mut var_symb, &fun_symb) {
            Ok(fun_ast) => correct.push(fun_ast),
            Err(fun_err) => errors.push(fun_err),
        }
    }

    // analyse main code block
    match analyse_block(
        main_block,
        &fun_symb,
        &mut LocalSymbolTable::new_root(),
        &mut var_symb,
        &None,
    ) {
        Ok((block_ast, _)) => {
            if errors.len() == 0 && fun_def_errs.len() == 0 {
                Ok((Program(correct, block_ast), var_symb))
            } else {
                Err((fun_def_errs, vec![], errors))
            }
        }
        Err(main_errs) => Err((fun_def_errs, main_errs, errors)),
    }
}
