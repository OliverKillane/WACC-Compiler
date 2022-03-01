mod trans_expr;

use super::ast::{ASTWrapper, AssignLhs, AssignRhs, ExprWrap, Stat, StatWrap, Type};
use crate::intermediate::{self as ir, BlockId, DataRef, VarRepr};
use crate::{
    frontend::{ast, semantic::symbol_table::VariableSymbolTable},
    intermediate::BlockEnding,
};
use std::collections::HashMap;
use std::mem;
use trans_expr::{array_index_fname, translate_expr};

impl From<&Type> for ir::Type {
    fn from(ast_type: &Type) -> Self {
        match ast_type {
            ast::Type::Int => ir::Type::Num(ir::NumSize::DWord),
            ast::Type::Char => ir::Type::Num(ir::NumSize::Byte),
            ast::Type::Bool => ir::Type::Bool,
            ast::Type::String | ast::Type::Pair(_, _) => ir::Type::Ptr,
            ast::Type::Array(_, _) => panic!("Nested array type"),
            ast::Type::Generic(_) | ast::Type::Any => {
                panic!("Expected a concrete type")
            }
        }
    }
}

fn translate_rhs(
    assign_rhs: AssignRhs<Option<Type>, usize>,
    var_symb: &VariableSymbolTable,
    function_types: &HashMap<String, Type>,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> ir::Expr {
    match assign_rhs {
        AssignRhs::Expr(ASTWrapper(expr_type, expr)) => translate_expr(
            expr,
            &expr_type.expect("Expected a type for an expression"),
            var_symb,
            data_ref_map,
        ),
        AssignRhs::Call(ASTWrapper(_, name), args) => {
            let args = args
                .into_iter()
                .map(|ASTWrapper(arg_type, arg_expr)| {
                    translate_expr(
                        arg_expr,
                        &arg_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                    )
                })
                .collect();
            match function_types[&name] {
                Type::Int | Type::Char => ir::Expr::Num(ir::NumExpr::Call(name, args)),
                Type::Bool => ir::Expr::Bool(ir::BoolExpr::Call(name, args)),
                Type::String | Type::Pair(_, _) | Type::Array(_, _) => {
                    ir::Expr::Ptr(ir::PtrExpr::Call(name, args))
                }
                Type::Generic(_) | Type::Any => panic!("Expected a concrete type"),
            }
        }
        AssignRhs::Array(ASTWrapper(_, fields)) => ir::Expr::Ptr(ir::PtrExpr::Malloc(
            vec![ir::Expr::Num(ir::NumExpr::Const(
                ir::NumSize::DWord,
                fields.len() as i32,
            ))]
            .into_iter()
            .chain(
                fields
                    .into_iter()
                    .map(|ASTWrapper(sub_expr_type, sub_expr)| {
                        translate_expr(
                            sub_expr,
                            &sub_expr_type.expect("Expected a type for an expression"),
                            var_symb,
                            data_ref_map,
                        )
                    }),
            )
            .collect(),
        )),
    }
}

fn translate_lhs<'l>(
    assign_lhs: AssignLhs<Option<Type>, usize>,
    var_symb: &'l VariableSymbolTable,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> (ir::PtrExpr, ir::Type) {
    match assign_lhs {
        AssignLhs::ArrayElem(var, mut indices) => {
            let (fields_type, &num_indices) =
                if let ast::Type::Array(box fields_type, num_indices) =
                    var_symb.get_type_from_id(var).expect("Variable not found")
                {
                    (fields_type, num_indices)
                } else {
                    panic!("Expected an array expression");
                };
            assert_eq!(num_indices, indices.len());
            let ASTWrapper(last_index_type, last_index) = indices.remove(num_indices - 1);
            let last_index = translate_expr(
                last_index,
                &last_index_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
            );

            let single_dim_ptr = indices
                .into_iter()
                .map(|ASTWrapper(sub_expr_type, sub_expr)| {
                    translate_expr(
                        sub_expr,
                        &sub_expr_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                    )
                })
                .fold(
                    ir::Expr::Ptr(ir::PtrExpr::Var(var)),
                    |data_ptr, sub_index| {
                        ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                            array_index_fname.to_string(),
                            vec![
                                data_ptr,
                                sub_index,
                                ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Ptr)),
                            ],
                        )))
                    },
                );
            return (
                ir::PtrExpr::Call(
                    array_index_fname.to_string(),
                    vec![
                        single_dim_ptr,
                        last_index,
                        ir::Expr::Num(ir::NumExpr::SizeOf(fields_type.into())),
                    ],
                ),
                fields_type.into(),
            );
        }
        AssignLhs::PairFst(ASTWrapper(expr_type, expr)) => {
            let expr_type = expr_type.expect("Expected a type for an expression");
            (
                if let ir::Expr::Ptr(ptr_expr) =
                    translate_expr(expr, &expr_type, var_symb, data_ref_map)
                {
                    ptr_expr
                } else {
                    panic!("Expected a pointer expression")
                },
                (&expr_type).into(),
            )
        }
        AssignLhs::PairSnd(ASTWrapper(expr_type, expr)) => {
            let expr_type = expr_type.expect("Expected a type for an expression");
            (
                if let ir::Expr::Ptr(ptr_expr) =
                    translate_expr(expr, &expr_type, var_symb, data_ref_map)
                {
                    ir::PtrExpr::Offset(box ptr_expr, box ir::NumExpr::SizeOfWideAlloc)
                } else {
                    panic!("Expected a pointer expression")
                },
                (&expr_type).into(),
            )
        }
        AssignLhs::Var(_) => panic!("Expected an AssignLhs that can be translated into a pointer"),
    }
}

fn translate_stat(
    ASTWrapper(_, stat): StatWrap<Option<ast::Type>, usize>,
    block_stats: &mut Vec<ir::Stat>,
    block_graph: &mut Vec<ir::Block>,
    prev_blocks: &mut Vec<BlockId>,
    free_var: &mut VarRepr,
    ir_vars: &mut HashMap<VarRepr, ir::Type>,
    var_symb: &VariableSymbolTable,
    function_types: &HashMap<String, Type>,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) {
    assert!(prev_blocks.len() > 0);
    match stat {
        Stat::Skip => {}
        Stat::Def(_, var, assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(assign_rhs, var_symb, function_types, data_ref_map),
            ));
        }
        Stat::Assign(AssignLhs::Var(var), assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(assign_rhs, var_symb, function_types, data_ref_map),
            ));
        }
        Stat::Assign(assign_lhs, assign_rhs) => {
            let (ptr_expr, _) = translate_lhs(assign_lhs, var_symb, data_ref_map);
            block_stats.push(ir::Stat::AssignPtr(
                ptr_expr,
                translate_rhs(assign_rhs, var_symb, function_types, data_ref_map),
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
                ir::Type::Num(ir::NumSize::DWord) => ir::Stat::ReadIntPtr(ptr_expr),
                ir::Type::Num(ir::NumSize::Byte) => ir::Stat::ReadCharPtr(ptr_expr),
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
                &mut true_block_stats,
                block_graph,
                &mut true_prev_blocks,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
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
                &mut false_block_stats,
                block_graph,
                &mut false_prev_blocks,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
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
                &mut while_block_stats,
                block_graph,
                &mut while_prev_blocks,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
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
            block_stats,
            block_graph,
            prev_blocks,
            free_var,
            ir_vars,
            var_symb,
            function_types,
            data_ref_map,
        ),
    }
}

fn translate_block(
    block: Vec<StatWrap<Option<ast::Type>, usize>>,
    block_stats: &mut Vec<ir::Stat>,
    block_graph: &mut Vec<ir::Block>,
    prev_blocks: &mut Vec<BlockId>,
    free_var: &mut VarRepr,
    ir_vars: &mut HashMap<VarRepr, ir::Type>,
    var_symb: &VariableSymbolTable,
    function_types: &HashMap<String, Type>,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) {
    for stat in block {
        translate_stat(
            stat,
            block_stats,
            block_graph,
            prev_blocks,
            free_var,
            ir_vars,
            var_symb,
            function_types,
            data_ref_map,
        );
        if prev_blocks.len() == 0 {
            break;
        }
    }
}
