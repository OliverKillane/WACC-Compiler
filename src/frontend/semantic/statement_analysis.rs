//! Analyse a block of instructions to get all errors, return either a modified
//! AST or the errors produced.
use crate::frontend::ast::*;
use crate::frontend::semantic::expression_analysis::*;
use crate::frontend::semantic::semantic_errors::*;
use crate::frontend::semantic::symbol_table::*;
use crate::frontend::semantic::type_constraints::*;

/// Analyse a vector of statements
fn analyse_block<'a, 'b>(
    stats: Vec<StatSpan<'a, Stat<'a, &'a str>>>,
    fun_symb: &FunctionSymbolTable,
    local_symb: &mut LocalSymbolTable<'a, 'b>,
    var_symb: &mut VariableSymbolTable,
    ret_type: Option<Type>,
) -> Result<(Vec<StatSpan<'a, Stat<'a, usize>>>, bool), SemanticErrorSummary<'a>> {
    for WrapSpan(span, stat) in stats {
        match stat {
            Skip => (),
            _ => (),
        }
    }
    todo!()
}

// fn analyse_assign_rhs<'a, 'b>(rhs: AssignRhs<'a, &'a str>, expected_type: &Type,  fun_symb: &FunctionSymbolTable,
// local_symb: &mut LocalSymbolTable<'a, 'b>,
// var_symb: &mut VariableSymbolTable,) -> RhsReturn<'a> {
//     match rhs {
//         AssignRhs::Expr(orig_expr) =>
//             match analyse_expression(orig_expr, TypeConstraint::new(expected_type.clone()), local_symb, var_symb) {
//                 Ok((_, new_expr)) => RhsReturn::Correct(AssignRhs::Expr(new_expr)),
//                 Err(e) => RhsReturn::ExprErr(e),
//             },
//         AssignRhs::Array(exprs) => {
//             let mut errors = Vec::new();
//             let mut clean = Vec::new();

//             exprs.into_iter().for_each(|expr_res| match analyse_expression(expr_res, TypeConstraint::new(expected_type.clone()),  local_symb, var_symb) {
//                 Ok((_, new_expr)) => clean.push(new_expr),
//                 Err(err) => errors.push(err),
//             });

//             if errors.len() == 0 {
//                 RhsReturn::Correct(AssignRhs::Array(clean))
//             } else {
//                 RhsReturn::ExprErr(errors.into_iter().reduce(|err1, err2| err1.add_errs(err2)).expect("errors len > 0"))
//             }
//         },
//         AssignRhs::NewPair(left_expr,right_expr ) => {
//             match &expected_type {

//             }
//         },
//         AssignRhs::PairFst(_) => todo!(),
//         AssignRhs::PairSnd(_) => todo!(),
//         AssignRhs::Call(_, _) => todo!(),
//     }
// }
