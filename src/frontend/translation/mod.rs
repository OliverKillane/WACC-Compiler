mod trans_expr;

use super::ast::{ASTWrapper, AssignLhs, AssignRhs, Stat, StatWrap, Type};
use crate::intermediate::{self as ir, BlockId, DataRef, VarRepr};
use crate::{
    frontend::{ast, semantic::symbol_table::VariableSymbolTable},
    intermediate::BlockEnding,
};
use std::collections::HashMap;
use std::mem;
use trans_expr::translate_expr;

fn translate_rhs(
    assign_rhs: AssignRhs<Option<Type>, usize>,
    var_symb: &VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> ir::Expr {
    todo!()
}

fn translate_lhs(
    assign_lhs: AssignLhs<Option<Type>, usize>,
    var_symb: &VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> (ir::PtrExpr, Type) {
    todo!()
}

fn translate_stat(
    ASTWrapper(_, stat): StatWrap<Option<ast::Type>, usize>,
    block_stats: &mut Vec<ir::Stat>,
    block_graph: &mut Vec<ir::Block>,
    prev_blocks: &mut Vec<BlockId>,
    free_var: &mut VarRepr,
    ir_vars: &mut HashMap<VarRepr, ir::Type>,
    var_symb: &VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) {
    assert!(prev_blocks.len() > 0);
    match stat {
        Stat::Skip => {}
        Stat::Def(_, var, assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(assign_rhs, var_symb, data_ref_map),
            ));
        }
        Stat::Assign(AssignLhs::Var(var), assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(assign_rhs, var_symb, data_ref_map),
            ));
        }
        Stat::Assign(assign_lhs, assign_rhs) => {
            let (ptr_expr, _) = translate_lhs(assign_lhs, var_symb, data_ref_map);
            block_stats.push(ir::Stat::AssignPtr(
                ptr_expr,
                translate_rhs(assign_rhs, var_symb, data_ref_map),
            ));
        }
        Stat::Read(AssignLhs::Var(var)) => {
            block_stats.push(
                match var_symb.get_type_from_id(var).expect("Variable not found") {
                    Type::Int => ir::Stat::ReadIntVar(var),
                    Type::Char => ir::Stat::ReadCharVar(var),
                    _ => panic!("Reads only supported on ints or characters"),
                },
            );
        }
        Stat::Read(assign_lhs) => {
            let (ptr_expr, expr_type) = translate_lhs(assign_lhs, var_symb, data_ref_map);
            block_stats.push(match expr_type {
                Type::Int => ir::Stat::ReadIntPtr(ptr_expr),
                Type::Char => ir::Stat::ReadCharPtr(ptr_expr),
                _ => panic!("Reads only supported on ints or characters"),
            });
        }
        Stat::Free(ASTWrapper(expr_type, expr)) => {
            let ptr_expr = if let ir::Expr::Ptr(ptr_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            ) {
                ptr_expr
            } else {
                panic!("Expected a pointer expression")
            };
            block_stats.push(ir::Stat::Free(ptr_expr));
        }
        Stat::Return(ASTWrapper(expr_type, expr)) => {
            let mut tmp_block_stats = Vec::new();
            mem::swap(block_stats, &mut tmp_block_stats);
            let block_graph_len = block_graph.len();
            block_graph.push(ir::Block(
                vec![block_graph_len],
                tmp_block_stats,
                BlockEnding::Return(translate_expr(
                    expr,
                    &expr_type.expect("Expected a type for an expression"),
                    var_symb,
                    data_ref_map,
                )),
            ));
        }
        Stat::Exit(ASTWrapper(expr_type, expr)) => {
            let mut tmp_block_stats = Vec::new();
            mem::swap(block_stats, &mut tmp_block_stats);
            if let ir::Expr::Num(num_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            ) {
                let mut tmp_prev_blocks = Vec::new();
                mem::swap(prev_blocks, &mut tmp_prev_blocks);
                block_graph.push(ir::Block(
                    tmp_prev_blocks,
                    tmp_block_stats,
                    BlockEnding::Exit(num_expr),
                ));
            } else {
                panic!("Expected a numeric expression");
            }
        }
        print_stat @ (Stat::Print(_) | Stat::PrintLn(_)) => {
            let (new_line, ASTWrapper(expr_type, expr)) = if let Stat::PrintLn(expr) = print_stat {
                (true, expr)
            } else if let Stat::Print(expr) = print_stat {
                (false, expr)
            } else {
                panic!("Expected a print statement")
            };
            let expr_type = expr_type.expect("Expected a type for an expression");
            let expr = translate_expr(expr, &expr_type, var_symb, data_ref_map);
            match (expr_type, expr) {
                (Type::Int | Type::Bool | Type::Pair(_, _) | Type::Array(_, _), expr) => {
                    block_stats.push(ir::Stat::PrintExpr(expr));
                }
                (Type::Char, ir::Expr::Num(num_expr)) => {
                    block_stats.push(ir::Stat::PrintChar(num_expr))
                }
                (Type::String, expr @ ir::Expr::Ptr(_)) => {
                    ir_vars.insert(*free_var, ir::Type::Ptr);
                    block_stats.push(ir::Stat::AssignVar(*free_var, expr));
                    block_stats.push(ir::Stat::PrintStr(
                        ir::PtrExpr::Offset(
                            box ir::PtrExpr::Var(*free_var),
                            box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
                        ),
                        ir::NumExpr::Deref(ir::NumSize::DWord, ir::PtrExpr::Var(*free_var)),
                    ));
                    *free_var += 1;
                }
                (Type::Generic(_) | Type::Any, _) => panic!("Expected a concrete type"),
                _ => panic!("Type does not match the expression"),
            }
            if new_line {
                block_stats.push(ir::Stat::PrintEol());
            }
        }
        Stat::If(ASTWrapper(expr_type, expr), true_block, false_block) => {
            // Conditional block
            let cond_block_id = block_graph.len();
            let mut tmp_prev_blocks = Vec::new();
            mem::swap(prev_blocks, &mut tmp_prev_blocks);
            let mut tmp_block_stats = Vec::new();
            mem::swap(block_stats, &mut tmp_block_stats);
            let bool_expr = if let ir::Expr::Bool(bool_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            ) {
                bool_expr
            } else {
                panic!("Expected a boolean expression as a condition")
            };
            block_graph.push(ir::Block(
                tmp_prev_blocks,
                tmp_block_stats,
                ir::BlockEnding::CondJumps(
                    vec![(bool_expr, cond_block_id + 1)],
                    0, // Dummy to fill in later
                ),
            ));

            // True branch
            let mut true_block_stats = Vec::new();
            let mut true_prev_blocks = vec![cond_block_id];
            translate_block(
                true_block,
                var_symb,
                data_ref_map,
                &mut true_block_stats,
                block_graph,
                &mut true_prev_blocks,
            );
            let true_block_id = if true_prev_blocks.len() > 0 {
                prev_blocks.push(block_graph.len());
                block_graph.push(ir::Block(
                    true_prev_blocks,
                    true_block_stats,
                    ir::BlockEnding::CondJumps(Vec::new(), 0), // Dummy to fill later
                ));
                Some(block_graph.len())
            } else {
                None
            };

            // Filling in conditional block
            let false_branch_id = block_graph.len();
            if let ir::Block(_, _, ir::BlockEnding::CondJumps(_, else_jump)) =
                block_graph.get_mut(cond_block_id).unwrap()
            {
                *else_jump = false_branch_id; // Dummy filled in
            } else {
                panic!("Expected a conditional jump ending");
            }

            // False branch
            let mut false_block_stats = Vec::new();
            let mut false_prev_blocks = vec![cond_block_id];
            translate_block(
                false_block,
                var_symb,
                data_ref_map,
                &mut false_block_stats,
                block_graph,
                &mut false_prev_blocks,
            );
            if false_prev_blocks.len() > 0 {
                let false_block_id = block_graph.len();
                prev_blocks.push(false_block_id);
                block_graph.push(ir::Block(
                    false_prev_blocks,
                    false_block_stats,
                    ir::BlockEnding::CondJumps(Vec::new(), false_block_id + 1),
                ));
            }

            // Filling in true branch
            let after_jump_id = block_graph.len();
            if let Some(true_block_id) = true_block_id {
                if let ir::Block(_, _, ir::BlockEnding::CondJumps(_, else_jump)) =
                    block_graph.get_mut(true_block_id).unwrap()
                {
                    *else_jump = after_jump_id; // Dummy filled in
                } else {
                    panic!("Expected a conditional jump ending");
                }
            }
        }
        Stat::While(ASTWrapper(expr_type, expr), block) => {
            // Preceding block
            let mut tmp_prev_blocks = Vec::new();
            mem::swap(prev_blocks, &mut tmp_prev_blocks);
            let mut tmp_block_stats = Vec::new();
            mem::swap(block_stats, &mut tmp_block_stats);
            let pre_block_id = block_graph.len();
            let cond_block_id = block_graph.len() + 1;
            block_graph.push(ir::Block(
                tmp_prev_blocks,
                tmp_block_stats,
                ir::BlockEnding::CondJumps(vec![], cond_block_id),
            ));

            // Conditional block
            let bool_expr = if let ir::Expr::Bool(bool_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            ) {
                bool_expr
            } else {
                panic!("Expected a boolean expression as a condition")
            };
            block_graph.push(ir::Block(
                vec![pre_block_id],
                Vec::new(),
                ir::BlockEnding::CondJumps(
                    vec![(bool_expr, cond_block_id + 1)],
                    0, // Dummy to fill later
                ),
            ));

            // While loop block
            let mut while_block_stats = Vec::new();
            let mut while_prev_blocks = vec![cond_block_id];
            translate_block(
                block,
                var_symb,
                data_ref_map,
                &mut while_block_stats,
                block_graph,
                &mut while_prev_blocks,
            );
            if while_prev_blocks.len() > 0 {
                let while_block_id = block_graph.len();
                block_graph.push(ir::Block(
                    while_prev_blocks,
                    while_block_stats,
                    ir::BlockEnding::CondJumps(vec![], cond_block_id),
                ));
                let ir::Block(cond_prev_blocks, _, _) = block_graph.get_mut(cond_block_id).unwrap();
                cond_prev_blocks.push(while_block_id);
            }
            let after_jump_id = block_graph.len();
            if let ir::Block(_, _, ir::BlockEnding::CondJumps(_, else_jump)) =
                block_graph.get_mut(cond_block_id).unwrap()
            {
                *else_jump = after_jump_id;
            } else {
                panic!("Expected a conditional jump ending");
            }
        }
        Stat::Block(block) => translate_block(
            block,
            var_symb,
            data_ref_map,
            block_stats,
            block_graph,
            prev_blocks,
        ),
    }
}

fn translate_block(
    block: Vec<StatWrap<Option<ast::Type>, usize>>,
    var_symb: &VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
    block_stats: &mut Vec<ir::Stat>,
    block_graph: &mut Vec<ir::Block>,
    prev_blocks: &mut Vec<BlockId>,
) {
    todo!()
}
