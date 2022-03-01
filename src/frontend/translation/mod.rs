mod helper_funcs;
mod trans_expr;

use super::ast::{
    ASTWrapper, AssignLhs, AssignRhs, Function, Param, Program, Stat, StatWrap, Type,
};
use crate::intermediate::{self as ir, BlockId, DataRef, VarRepr};
use crate::{
    frontend::{ast, semantic::symbol_table::VariableSymbolTable},
    intermediate::BlockEnding,
};
use helper_funcs::*;
use std::collections::{HashMap, LinkedList};
use std::mem;
use trans_expr::translate_expr;

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
    helper_function_flags: &mut HelperFunctionFlags,
) -> ir::Expr {
    match assign_rhs {
        AssignRhs::Expr(ASTWrapper(expr_type, expr)) => translate_expr(
            expr,
            &expr_type.expect("Expected a type for an expression"),
            var_symb,
            data_ref_map,
            helper_function_flags,
        ),
        AssignRhs::Call(ASTWrapper(_, name), args) => {
            let name = "f_".to_string() + &name;
            let args = args
                .into_iter()
                .map(|ASTWrapper(arg_type, arg_expr)| {
                    translate_expr(
                        arg_expr,
                        &arg_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                        helper_function_flags,
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
                            helper_function_flags,
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
    helper_function_flags: &mut HelperFunctionFlags,
) -> (ir::PtrExpr, ir::Type) {
    match assign_lhs {
        AssignLhs::ArrayElem(var, mut indices) => {
            helper_function_flags.array_indexing = true;
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
                helper_function_flags,
            );

            let single_dim_ptr = indices
                .into_iter()
                .map(|ASTWrapper(sub_expr_type, sub_expr)| {
                    translate_expr(
                        sub_expr,
                        &sub_expr_type.expect("Expected a type for an expression"),
                        var_symb,
                        data_ref_map,
                        helper_function_flags,
                    )
                })
                .fold(
                    ir::Expr::Ptr(ir::PtrExpr::Var(var)),
                    |data_ptr, sub_index| {
                        ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::Call(
                            ARRAY_INDEX_FNAME.to_string(),
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
                    ARRAY_INDEX_FNAME.to_string(),
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
            helper_function_flags.check_null = true;
            let expr_type = expr_type.expect("Expected a type for an expression");
            (
                ir::PtrExpr::Call(
                    CHECK_NULL_FNAME.to_string(),
                    vec![translate_expr(
                        expr,
                        &expr_type,
                        var_symb,
                        data_ref_map,
                        helper_function_flags,
                    )],
                ),
                (&expr_type).into(),
            )
        }
        AssignLhs::PairSnd(ASTWrapper(expr_type, expr)) => {
            helper_function_flags.check_null = true;
            let expr_type = expr_type.expect("Expected a type for an expression");
            (
                ir::PtrExpr::Offset(
                    box ir::PtrExpr::Call(
                        CHECK_NULL_FNAME.to_string(),
                        vec![translate_expr(
                            expr,
                            &expr_type,
                            var_symb,
                            data_ref_map,
                            helper_function_flags,
                        )],
                    ),
                    box ir::NumExpr::SizeOfWideAlloc,
                ),
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
    helper_function_flags: &mut HelperFunctionFlags,
) {
    match stat {
        Stat::Skip => {}
        Stat::Def(_, var, assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(
                    assign_rhs,
                    var_symb,
                    function_types,
                    data_ref_map,
                    helper_function_flags,
                ),
            ));
        }
        Stat::Assign(AssignLhs::Var(var), assign_rhs) => {
            block_stats.push(ir::Stat::AssignVar(
                var,
                translate_rhs(
                    assign_rhs,
                    var_symb,
                    function_types,
                    data_ref_map,
                    helper_function_flags,
                ),
            ));
        }
        Stat::Assign(assign_lhs, assign_rhs) => {
            let (ptr_expr, _) =
                translate_lhs(assign_lhs, var_symb, data_ref_map, helper_function_flags);
            block_stats.push(ir::Stat::AssignPtr(
                ptr_expr,
                translate_rhs(
                    assign_rhs,
                    var_symb,
                    function_types,
                    data_ref_map,
                    helper_function_flags,
                ),
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
            let (ptr_expr, expr_type) =
                translate_lhs(assign_lhs, var_symb, data_ref_map, helper_function_flags);
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
                helper_function_flags,
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
                    helper_function_flags,
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
                helper_function_flags,
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
            let expr = translate_expr(
                expr,
                &expr_type,
                var_symb,
                data_ref_map,
                helper_function_flags,
            );
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
                helper_function_flags,
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
            let true_block_id = if translate_block_jumping(
                true_block,
                vec![cond_block_id],
                None,
                block_graph,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
                helper_function_flags,
            ) {
                let true_block_id = block_graph.len() - 1;
                prev_blocks.push(true_block_id);
                Some(true_block_id)
            } else {
                None
            };

            let false_branch_id = block_graph.len();
            if let ir::Block(_, _, ir::BlockEnding::CondJumps(_, else_jump)) =
                block_graph.get_mut(cond_block_id).unwrap()
            {
                *else_jump = false_branch_id; // Dummy filled in
            } else {
                panic!("Expected a conditional jump ending");
            }

            if translate_block_jumping(
                false_block,
                vec![cond_block_id],
                None,
                block_graph,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
                helper_function_flags,
            ) {
                prev_blocks.push(block_graph.len());
            }

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

            let bool_expr = if let ir::Expr::Bool(bool_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
                helper_function_flags,
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

            if translate_block_jumping(
                block,
                vec![cond_block_id],
                Some(cond_block_id),
                block_graph,
                free_var,
                ir_vars,
                var_symb,
                function_types,
                data_ref_map,
                helper_function_flags,
            ) {
                let while_block_id = block_graph.len() - 1;
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
            helper_function_flags,
        ),
    }
}

fn translate_block_jumping(
    block: Vec<StatWrap<Option<ast::Type>, usize>>,
    mut prev_blocks: Vec<BlockId>,
    next_jump: Option<BlockId>,
    block_graph: &mut Vec<ir::Block>,
    free_var: &mut VarRepr,
    ir_vars: &mut HashMap<VarRepr, ir::Type>,
    var_symb: &VariableSymbolTable,
    function_types: &HashMap<String, Type>,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
    helper_function_flags: &mut HelperFunctionFlags,
) -> bool {
    let mut block_stats = Vec::new();
    translate_block(
        block,
        &mut block_stats,
        block_graph,
        &mut prev_blocks,
        free_var,
        ir_vars,
        var_symb,
        function_types,
        data_ref_map,
        helper_function_flags,
    );
    if prev_blocks.len() > 0 {
        let next_block_id = block_graph.len() + 1;
        block_graph.push(ir::Block(
            prev_blocks,
            block_stats,
            BlockEnding::CondJumps(Vec::new(), next_jump.unwrap_or(next_block_id)),
        ));
        true
    } else {
        false
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
    helper_function_flags: &mut HelperFunctionFlags,
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
            helper_function_flags,
        );
        if prev_blocks.len() == 0 && block_stats.len() == 0 {
            break;
        }
    }
}

fn translate_function(
    ret_type: &Type,
    args: Vec<ASTWrapper<Option<Type>, Param<usize>>>,
    block: Vec<StatWrap<Option<Type>, usize>>,
    var_symb @ VariableSymbolTable(var_map): &VariableSymbolTable,
    function_types: &HashMap<String, Type>,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
    helper_function_flags: &mut HelperFunctionFlags,
) -> ir::Function {
    let mut ir_vars = var_map
        .iter()
        .map(|(&var, var_type)| (var, var_type.into()))
        .collect();
    let mut block_graph = Vec::new();
    assert!(!translate_block_jumping(
        block,
        vec![],
        None,
        &mut block_graph,
        &mut var_map.keys().max().map(|x| *x).unwrap_or(0),
        &mut ir_vars,
        var_symb,
        function_types,
        data_ref_map,
        helper_function_flags
    ));
    ir::Function(
        ret_type.into(),
        args.iter()
            .map(|ASTWrapper(_, Param(arg_type, arg))| (arg_type.into(), *arg))
            .collect(),
        ir_vars,
        block_graph,
    )
}

fn translate_ast(
    Program(functions, block): Program<Option<Type>, usize>,
    function_symbol_tables: HashMap<String, VariableSymbolTable>,
    program_symbol_table: VariableSymbolTable,
) -> ir::Program {
    let (functions, function_types): (LinkedList<_>, HashMap<_, _>) = functions
        .into_iter()
        .map(
            |ASTWrapper(_, Function(ret_type, ASTWrapper(_, fname), args, block))| {
                ((fname.clone(), args, block), (fname, ret_type))
            },
        )
        .unzip();
    let mut data_ref_map = HashMap::new();
    let mut helper_function_flags = HelperFunctionFlags::default();
    let mut functions_map: HashMap<_, _> = functions
        .into_iter()
        .map(|(fname, args, block)| {
            let ret_type = function_types.get(&fname).unwrap();
            let var_symb = function_symbol_tables
                .get(&fname)
                .expect("No symbol table for a function");
            (
                fname,
                translate_function(
                    ret_type,
                    args,
                    block,
                    var_symb,
                    &function_types,
                    &mut data_ref_map,
                    &mut helper_function_flags,
                ),
            )
        })
        .collect();
    let VariableSymbolTable(var_map) = &program_symbol_table;
    let mut free_var = &mut var_map.keys().max().map(|x| *x).unwrap_or(0);
    let mut ir_vars = var_map
        .iter()
        .map(|(&var, var_type)| (var, var_type.into()))
        .collect();
    let mut block_graph = Vec::new();
    assert!(!translate_block_jumping(
        block,
        vec![],
        None,
        &mut block_graph,
        &mut free_var,
        &mut ir_vars,
        &program_symbol_table,
        &function_types,
        &mut data_ref_map,
        &mut helper_function_flags
    ));
    helper_function_flags.generate_functions(&mut functions_map, &mut data_ref_map);
    ir::Program(
        functions_map,
        ir_vars,
        block_graph,
        data_ref_map,
        Some(OVERFLOW_HANDLER_FNAME.to_string()),
    )
}
