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
            ast::Type::Array(box Type::Array(_, _), _) => panic!("Nested Array Type"),
            ast::Type::Array(_, _) => ir::Type::Ptr,
            ast::Type::Generic(_) | ast::Type::Any => {
                panic!("Expected a concrete type")
            }
        }
    }
}

const FUNCTION_NAME_PREFIX: &str = "f_";
fn prefix_function_name(fname: &str) -> String {
    FUNCTION_NAME_PREFIX.to_string() + fname
}

/// Translation for the [assign rhs node](AssignRhs). The arguments are as follows:
///  - The assign rhs node to be translated.
///  - Symbol table for this particular node.
///  - The return types of all the functions.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
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
            let prefixed_name = prefix_function_name(&name);
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
                Type::Int | Type::Char => ir::Expr::Num(ir::NumExpr::Call(prefixed_name, args)),
                Type::Bool => ir::Expr::Bool(ir::BoolExpr::Call(prefixed_name, args)),
                Type::String | Type::Pair(_, _) | Type::Array(_, _) => {
                    ir::Expr::Ptr(ir::PtrExpr::Call(prefixed_name, args))
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

/// Translation for [assign lhs node](AssignLhs). The arguments are as follows:
///  - The assign lhs node to be translated.
///  - Symbol table for this particular node.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
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
            if let ast::Type::Array(_, _) = fields_type {
                panic!("Nested array type");
            }
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
            (
                ir::PtrExpr::Call(
                    ARRAY_INDEX_FNAME.to_string(),
                    vec![
                        single_dim_ptr,
                        last_index,
                        ir::Expr::Num(ir::NumExpr::SizeOf(fields_type.into())),
                    ],
                ),
                fields_type.into(),
            )
        }
        AssignLhs::PairFst(ASTWrapper(expr_type, expr)) => {
            helper_function_flags.check_null = true;
            let expr_type = expr_type.expect("Expected a type for an expression");
            let fst_type = if let Type::Pair(box ref fst_type, _) = expr_type {
                fst_type
            } else {
                panic!("Expected a pair type annotation")
            };
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
                fst_type.into(),
            )
        }
        AssignLhs::PairSnd(ASTWrapper(expr_type, expr)) => {
            helper_function_flags.check_null = true;
            let expr_type = expr_type.expect("Expected a type for an expression");
            let snd_type = if let Type::Pair(_, box ref snd_type) = expr_type {
                snd_type
            } else {
                panic!("Expected a pair type annotation")
            };
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
                snd_type.into(),
            )
        }
        AssignLhs::Var(_) => panic!("Expected an AssignLhs that can be translated into a pointer"),
    }
}

/// Translation of a single code statement. The arguments are as follows:
///  - The statement to be translated.
///  - The statements to be included in the next intermediate representation block.
///  - The compound block graph for the function/main program code.
///  - The first free variable representation such that all variable representations
///    after it are not used.
///  - The map of all variables used for that block of code. It is not to be actually
///    indexed; rather, it should have items put into it in conjunction with incrementing
///    the free variable argument.
///  - Symbol table for this particular statement.
///  - The return types of all the functions.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
/// If a statement translation ended with a final statement, for example an exit
/// or a return, then this is signified by the previous blocks and the new statements
/// vectors being empty.
#[allow(clippy::too_many_arguments)]
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
            let mut tmp_block_stats = vec![];
            mem::swap(block_stats, &mut tmp_block_stats);
            let mut tmp_prev_blocks = vec![];
            mem::swap(prev_blocks, &mut tmp_prev_blocks);
            block_graph.push(ir::Block(
                tmp_prev_blocks,
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
            let mut tmp_block_stats = vec![];
            mem::swap(block_stats, &mut tmp_block_stats);
            if let ir::Expr::Num(num_expr) = translate_expr(
                expr,
                &expr_type.expect("Expected a type for an expression"),
                var_symb,
                data_ref_map,
                helper_function_flags,
            ) {
                let mut tmp_prev_blocks = vec![];
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
            let mut tmp_prev_blocks = vec![];
            mem::swap(prev_blocks, &mut tmp_prev_blocks);
            let mut tmp_block_stats = vec![];
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
            let mut tmp_prev_blocks = vec![];
            mem::swap(prev_blocks, &mut tmp_prev_blocks);
            let mut tmp_block_stats = vec![];
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
                vec![],
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

#[allow(clippy::too_many_arguments)]
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
    let mut block_stats = vec![];
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
    if !block_stats.is_empty() || !prev_blocks.is_empty() {
        let next_block_id = block_graph.len() + 1;
        block_graph.push(ir::Block(
            prev_blocks,
            block_stats,
            BlockEnding::CondJumps(vec![], next_jump.unwrap_or(next_block_id)),
        ));
        true
    } else {
        false
    }
}

/// Translation of a code block. The arguments are as follows:
///  - The code block to be translated.
///  - The statements to be included in the next intermediate representation block.
///  - The compound block graph for the function/main program code.
///  - The first free variable representation such that all variable representations
///    after it are not used.
///  - The map of all variables used for that block of code. It is not to be actually
///    indexed; rather, it should have items put into it in conjunction with incrementing
///    the free variable argument.
///  - Symbol table for this particular block of code.
///  - The return types of all the functions.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
#[allow(clippy::too_many_arguments)]
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
        if prev_blocks.is_empty() && block_stats.is_empty() {
            break;
        }
    }
}

/// Translation of a single function. The arguments are as follows:
///  - Specification of the return type of the function.
///  - Specification of the arguments to the function.
///  - Main code block of the function.
///  - Symbol table for this particular function.
///  - The return types of all the functions.
///  - The map of all static data references in the program. The data references
///    are assumed to be consecutive, i.e. the smallest data reference not already
///    in the map is equal to the length of the map.
///  - The flags for helper functions. For more information see
///    [the documenation for the struct](HelperFunctionFlags).
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
    let mut block_graph = vec![];
    assert!(!translate_block_jumping(
        block,
        vec![],
        None,
        &mut block_graph,
        &mut var_map.keys().max().copied().unwrap_or(0),
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

/// Translation of the whole AST. The arguments are as follows:
///  - The whole AST.
///  - The symbol tables for separate functions.
///  - The main program symbol table.
pub(super) fn translate_ast(
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
                prefix_function_name(&fname),
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
    let free_var = &mut var_map.keys().max().copied().unwrap_or(0);
    let mut ir_vars = var_map
        .iter()
        .map(|(&var, var_type)| (var, var_type.into()))
        .collect();
    let mut block_graph = vec![];
    if translate_block_jumping(
        block,
        vec![],
        None,
        &mut block_graph,
        free_var,
        &mut ir_vars,
        &program_symbol_table,
        &function_types,
        &mut data_ref_map,
        &mut helper_function_flags,
    ) {
        let last_block_id = block_graph.len() - 1;
        block_graph.push(ir::Block(
            vec![last_block_id],
            vec![],
            ir::BlockEnding::Exit(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
        ));
    }
    helper_function_flags.generate_functions(&mut functions_map, &mut data_ref_map);
    ir::Program(
        functions_map,
        ir_vars,
        block_graph,
        data_ref_map,
        Some(OVERFLOW_HANDLER_FNAME.to_string()),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intermediate::{self as ir, NumExpr, NumSize};

    #[test]
    fn test_exit() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        if let ir::Expr::Num(inner_expr) = ir_expr {
            let ast_stat = ast::Stat::Exit(ast_expr_wrap);

            translate_stat(
                ASTWrapper(None, ast_stat),
                &mut block_stats,
                &mut block_graph,
                &mut prev_blocks,
                &mut free_var,
                &mut ir_vars,
                &mut var_symb,
                &mut function_types,
                &mut data_ref_map,
                &mut helper_function_flags,
            );

            let ref_block_stats: Vec<ir::Stat> = vec![];
            let mut ref_block_graph: Vec<ir::Block> = vec![];
            let ref_prev_blocks: Vec<BlockId> = vec![];

            ref_block_graph.push(ir::Block(
                ref_prev_blocks,
                ref_block_stats,
                BlockEnding::Exit(inner_expr),
            ));

            assert_eq!(ref_block_graph, block_graph);
        } else {
            panic!("Test expression is not a numerical expression");
        }
    }

    #[test]
    #[should_panic(expected = "Expected a type for an expression")]
    fn test_exit_no_type() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(None, ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        if let ir::Expr::Num(inner_expr) = ir_expr {
            let ast_stat = ast::Stat::Exit(ast_expr_wrap);

            translate_stat(
                ASTWrapper(None, ast_stat),
                &mut block_stats,
                &mut block_graph,
                &mut prev_blocks,
                &mut free_var,
                &mut ir_vars,
                &mut var_symb,
                &mut function_types,
                &mut data_ref_map,
                &mut helper_function_flags,
            );

            let ref_block_stats: Vec<ir::Stat> = vec![];
            let mut ref_block_graph: Vec<ir::Block> = vec![];
            let ref_prev_blocks: Vec<BlockId> = vec![];

            ref_block_graph.push(ir::Block(
                ref_prev_blocks,
                ref_block_stats,
                BlockEnding::Exit(inner_expr),
            ));

            assert_eq!(ref_block_graph, block_graph);
        } else {
            panic!("Test expression is not a numerical expression");
        }
    }

    #[test]
    #[should_panic(expected = "Expected a numeric expression")]
    fn test_exit_non_num_expr() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Bool(true);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::Exit(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    fn test_return() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::Return(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ref_block_stats: Vec<ir::Stat> = vec![];
        let mut ref_block_graph: Vec<ir::Block> = vec![];

        ref_block_graph.push(ir::Block(
            vec![],
            ref_block_stats,
            BlockEnding::Return(ir_expr),
        ));

        assert_eq!(ref_block_graph, block_graph);
    }

    #[test]
    #[should_panic(expected = "Expected a type for an expression")]
    fn test_return_no_type() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(None, ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::Return(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ref_block_stats: Vec<ir::Stat> = vec![];
        let mut ref_block_graph: Vec<ir::Block> = vec![];
        let ref_block_graph_len = ref_block_graph.len();

        ref_block_graph.push(ir::Block(
            vec![ref_block_graph_len],
            ref_block_stats,
            BlockEnding::Return(ir_expr),
        ));

        assert_eq!(ref_block_graph, block_graph);
    }

    #[test]
    fn test_print_int() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    fn test_print_char() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Char('a');
        let ast_expr_type = ast::Type::Char;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        if let ir::Expr::Num(inner_expr) = ir_expr {
            let ast_stat = ast::Stat::Print(ast_expr_wrap);

            translate_stat(
                ASTWrapper(None, ast_stat),
                &mut block_stats,
                &mut block_graph,
                &mut prev_blocks,
                &mut free_var,
                &mut ir_vars,
                &mut var_symb,
                &mut function_types,
                &mut data_ref_map,
                &mut helper_function_flags,
            );

            let mut ref_block_stats: Vec<ir::Stat> = vec![];

            ref_block_stats.push(ir::Stat::PrintChar(inner_expr));

            assert_eq!(ref_block_stats, block_stats);
        } else {
            panic!("Test expression is not a numerical expression");
        }
    }

    #[test]
    fn test_print_bool() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Bool(true);
        let ast_expr_type = ast::Type::Bool;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    fn test_print_pair() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::BinOp(
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(
                        Some(ast::Type::Int),
                        ast::Expr::UnOp(
                            ast::UnOp::Ord,
                            box ASTWrapper(Some(ast::Type::Char), ast::Expr::Char('b')),
                        ),
                    ),
                    ast::BinOp::Eq,
                    box ASTWrapper(Some(ast::Type::Int), ast::Expr::Int(65)),
                ),
            ),
            ast::BinOp::Newpair,
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(true)),
                    ast::BinOp::And,
                    box ASTWrapper(
                        Some(ast::Type::Bool),
                        ast::Expr::UnOp(
                            ast::UnOp::Neg,
                            box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        let ast_expr_type = ast::Type::Pair(box ast::Type::Bool, box ast::Type::Bool);
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_print_array() {
        todo!();
    }

    #[test]
    fn test_print_string() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let test_str = "test".to_string();

        let ast_expr = ast::Expr::String(test_str.clone());
        let ast_expr_type = ast::Type::String;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];
        let ref_free_var: VarRepr = 0;

        ref_block_stats.push(ir::Stat::AssignVar(ref_free_var, ir_expr));

        ref_block_stats.push(ir::Stat::PrintStr(
            ir::PtrExpr::Offset(
                box ir::PtrExpr::Var(ref_free_var),
                box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
            ),
            ir::NumExpr::Deref(ir::NumSize::DWord, ir::PtrExpr::Var(ref_free_var)),
        ));

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    fn test_println_int() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Int(0);
        let ast_expr_type = ast::Type::Int;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));
        ref_block_stats.push(ir::Stat::PrintEol());

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    fn test_println_char() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Char('a');
        let ast_expr_type = ast::Type::Char;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        if let ir::Expr::Num(inner_expr) = ir_expr {
            let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

            translate_stat(
                ASTWrapper(None, ast_stat),
                &mut block_stats,
                &mut block_graph,
                &mut prev_blocks,
                &mut free_var,
                &mut ir_vars,
                &mut var_symb,
                &mut function_types,
                &mut data_ref_map,
                &mut helper_function_flags,
            );

            let mut ref_block_stats: Vec<ir::Stat> = vec![];

            ref_block_stats.push(ir::Stat::PrintChar(inner_expr));
            ref_block_stats.push(ir::Stat::PrintEol());

            assert_eq!(ref_block_stats, block_stats);
        } else {
            panic!("Test expression is not a numerical expression");
        }
    }

    #[test]
    fn test_println_bool() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Bool(true);
        let ast_expr_type = ast::Type::Bool;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));
        ref_block_stats.push(ir::Stat::PrintEol());

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    fn test_println_pair() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::BinOp(
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(
                        Some(ast::Type::Int),
                        ast::Expr::UnOp(
                            ast::UnOp::Ord,
                            box ASTWrapper(Some(ast::Type::Char), ast::Expr::Char('b')),
                        ),
                    ),
                    ast::BinOp::Eq,
                    box ASTWrapper(Some(ast::Type::Int), ast::Expr::Int(65)),
                ),
            ),
            ast::BinOp::Newpair,
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(true)),
                    ast::BinOp::And,
                    box ASTWrapper(
                        Some(ast::Type::Bool),
                        ast::Expr::UnOp(
                            ast::UnOp::Neg,
                            box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        let ast_expr_type = ast::Type::Pair(box ast::Type::Bool, box ast::Type::Bool);
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];

        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr));
        ref_block_stats.push(ir::Stat::PrintEol());

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    #[should_panic(expected = "not yet implemented")]
    fn test_println_array() {
        todo!();
    }

    #[test]
    fn test_println_string() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let test_str = "test".to_string();

        let ast_expr = ast::Expr::String(test_str.clone());
        let ast_expr_type = ast::Type::String;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];
        let ref_free_var: VarRepr = 0;

        ref_block_stats.push(ir::Stat::AssignVar(ref_free_var, ir_expr));

        ref_block_stats.push(ir::Stat::PrintStr(
            ir::PtrExpr::Offset(
                box ir::PtrExpr::Var(ref_free_var),
                box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
            ),
            ir::NumExpr::Deref(ir::NumSize::DWord, ir::PtrExpr::Var(ref_free_var)),
        ));
        ref_block_stats.push(ir::Stat::PrintEol());

        assert_eq!(ref_block_stats, block_stats);
    }

    #[test]
    #[should_panic(expected = "Expected a concrete type")]
    fn test_print_generic() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Null;
        let ast_expr_type = ast::Type::Generic(0);
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    #[should_panic(expected = "Expected a concrete type")]
    fn test_print_any() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Null;
        let ast_expr_type = ast::Type::Any;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    #[should_panic(expected = "Expected a concrete type")]
    fn test_println_generic() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Null;
        let ast_expr_type = ast::Type::Generic(0);
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    #[should_panic(expected = "Expected a concrete type")]
    fn test_println_any() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Null;
        let ast_expr_type = ast::Type::Any;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    #[should_panic(expected = "Type does not match the expression")]
    fn test_print_mismatched_types2() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let ast_expr = ast::Expr::Null;
        let ast_expr_type = ast::Type::Char;
        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());

        let ast_stat = ast::Stat::Print(ast_expr_wrap);

        translate_stat(
            ASTWrapper(None, ast_stat),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );
    }

    #[test]
    fn test_simple_block() {
        let mut block_stats: Vec<ir::Stat> = vec![];
        let mut block_graph: Vec<ir::Block> = vec![];
        let mut prev_blocks: Vec<BlockId> = vec![];
        let mut free_var: VarRepr = 0;
        let mut ir_vars: HashMap<VarRepr, ir::Type> = HashMap::new();
        let mut var_symb: VariableSymbolTable = VariableSymbolTable::new();
        let mut function_types: HashMap<String, Type> = HashMap::new();
        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();
        let mut helper_function_flags: HelperFunctionFlags = HelperFunctionFlags::default();

        let test_str = "test".to_string();

        let ast_expr = ast::Expr::String(test_str.clone());

        let ast_expr2: ast::Expr<Option<ast::Type>, usize> = ast::Expr::BinOp(
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(
                        Some(ast::Type::Int),
                        ast::Expr::UnOp(
                            ast::UnOp::Ord,
                            box ASTWrapper(Some(ast::Type::Char), ast::Expr::Char('b')),
                        ),
                    ),
                    ast::BinOp::Eq,
                    box ASTWrapper(Some(ast::Type::Int), ast::Expr::Int(65)),
                ),
            ),
            ast::BinOp::Newpair,
            box ASTWrapper(
                Some(ast::Type::Bool),
                ast::Expr::BinOp(
                    box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(true)),
                    ast::BinOp::And,
                    box ASTWrapper(
                        Some(ast::Type::Bool),
                        ast::Expr::UnOp(
                            ast::UnOp::Neg,
                            box ASTWrapper(Some(ast::Type::Bool), ast::Expr::Bool(false)),
                        ),
                    ),
                ),
            ),
        );

        let ast_expr_type = ast::Type::String;
        let ast_expr_type2 = ast::Type::Pair(box ast::Type::Bool, box ast::Type::Bool);

        let ast_expr_wrap = ASTWrapper(Some(ast_expr_type.clone()), ast_expr.clone());
        let ast_expr_wrap2 = ASTWrapper(Some(ast_expr_type2.clone()), ast_expr2.clone());

        let ast_stat = ast::Stat::PrintLn(ast_expr_wrap);
        let ast_stat2 = ast::Stat::Print(ast_expr_wrap2);

        let ast_block = ast::Stat::Block(vec![
            ASTWrapper(None, ast_stat),
            ASTWrapper(None, ast_stat2),
        ]);

        let ir_expr = translate_expr(
            ast_expr,
            &ast_expr_type,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let ir_expr2 = translate_expr(
            ast_expr2,
            &ast_expr_type2,
            &var_symb,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut data_ref_map: HashMap<DataRef, Vec<ir::Expr>> = HashMap::new();

        translate_stat(
            ASTWrapper(None, ast_block),
            &mut block_stats,
            &mut block_graph,
            &mut prev_blocks,
            &mut free_var,
            &mut ir_vars,
            &mut var_symb,
            &mut function_types,
            &mut data_ref_map,
            &mut helper_function_flags,
        );

        let mut ref_block_stats: Vec<ir::Stat> = vec![];
        let ref_free_var: VarRepr = 0;

        ref_block_stats.push(ir::Stat::AssignVar(ref_free_var, ir_expr));

        ref_block_stats.push(ir::Stat::PrintStr(
            ir::PtrExpr::Offset(
                box ir::PtrExpr::Var(ref_free_var),
                box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
            ),
            ir::NumExpr::Deref(ir::NumSize::DWord, ir::PtrExpr::Var(ref_free_var)),
        ));
        ref_block_stats.push(ir::Stat::PrintEol());
        ref_block_stats.push(ir::Stat::PrintExpr(ir_expr2));

        assert_eq!(ref_block_stats, block_stats);
    }
}
