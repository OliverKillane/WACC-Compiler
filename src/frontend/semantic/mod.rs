pub mod expression_analysis;
pub mod function_analysis;
pub mod semantic_errors;
pub mod statement_analysis;
pub mod symbol_table;
pub mod type_constraints;

use crate::frontend::semantic::{function_analysis::analyse_function, statement_analysis::analyse_block};

use self::{symbol_table::{get_fn_symbols, VariableSymbolTable, LocalSymbolTable}, semantic_errors::{SemanticErrorSummary, SemanticError}};

use super::ast::*;

/// Analyses a program, either returning a flat variable and function symbol table and an ast, or a tuple of error (function defs, main body, function errors)
pub fn analyse_semantics<'a>(Program(fn_defs, main_block): Program<'a, &'a str>) -> Result<(Program<'a, usize>, VariableSymbolTable), (Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>, Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>, Vec<(&str, Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>)>)> {
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
    match analyse_block(main_block, &fun_symb, &mut LocalSymbolTable::new_root(), &mut var_symb, &None) {
        Ok((block_ast, _)) => {
            if errors.len() == 0 && fun_def_errs.len() == 0 {
                Ok((Program(correct, block_ast), var_symb))
            } else {
                Err((fun_def_errs, vec![], errors))
            }
        },
        Err(main_errs) => Err((fun_def_errs, main_errs, errors)),
    }
}

