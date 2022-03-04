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
                prev_blocks.push(block_graph.len() - 1);
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
            prev_blocks.push(cond_block_id);
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
    let mut ir_vars: HashMap<usize, ir::Type> = var_map
        .iter()
        .map(|(&var, var_type)| (var, var_type.into()))
        .collect();
    for ASTWrapper(_, Param(_, arg)) in &args {
        ir_vars.remove(arg);
    }
    let mut block_graph = vec![];
    assert!(!translate_block_jumping(
        block,
        vec![],
        None,
        &mut block_graph,
        &mut var_map.keys().max().copied().map(|k| k + 1).unwrap_or(0),
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
        &mut var_map.keys().max().copied().map(|k| k + 1).unwrap_or(0),
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
    } else if block_graph.is_empty() {
        block_graph.push(ir::Block(
            vec![],
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
    use super::{
        super::ast::{ASTWrapper, AssignLhs, AssignRhs, Expr, Stat, Type},
        super::semantic::symbol_table::VariableSymbolTable,
        helper_funcs::{HelperFunctionFlags, ARRAY_INDEX_FNAME},
        translate_stat,
    };
    use crate::intermediate::{self as ir, DataRef};
    use std::collections::HashMap;

    fn test_statement(
        stat: Stat<Option<Type>, usize>,
        true_block_stats: Vec<ir::Stat>,
        true_block_graph: Vec<ir::Block>,
        var_types: HashMap<usize, Type>,
        data_ref_strings_map: HashMap<DataRef, String>,
        function_types: HashMap<String, Type>,
    ) {
        let mut block_stats = vec![];
        let mut block_graph = vec![];
        let mut data_ref_map = HashMap::new();
        translate_stat(
            ASTWrapper(None, stat),
            &mut block_stats,
            &mut block_graph,
            &mut vec![],
            &mut var_types.keys().map(|k| *k).max().unwrap_or(0),
            &mut HashMap::new(),
            &VariableSymbolTable(var_types),
            &function_types,
            &mut data_ref_map,
            &mut HelperFunctionFlags::default(),
        );
        assert_eq!(block_stats, true_block_stats);
        assert_eq!(block_graph, true_block_graph);
        assert_eq!(
            data_ref_map,
            data_ref_strings_map
                .into_iter()
                .map(|(data_ref, string)| (
                    data_ref,
                    vec![ir::Expr::Num(ir::NumExpr::Const(
                        ir::NumSize::DWord,
                        string.len() as i32
                    ))]
                    .into_iter()
                    .chain(
                        string
                            .as_bytes()
                            .iter()
                            .map(|c| ir::Expr::Num(ir::NumExpr::Const(
                                ir::NumSize::Byte,
                                *c as i32
                            )))
                    )
                    .collect()
                ))
                .collect()
        );
    }

    #[test]
    fn test_skip() {
        test_statement(
            Stat::Skip,
            vec![],
            vec![],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
        );
    }

    #[test]
    fn test_def() {
        test_statement(
            Stat::Def(
                Type::String,
                0,
                AssignRhs::Expr(ASTWrapper(
                    Some(Type::String),
                    Expr::String("abc".to_string()),
                )),
            ),
            vec![ir::Stat::AssignVar(
                0,
                ir::Expr::Ptr(ir::PtrExpr::DataRef(0)),
            )],
            vec![],
            HashMap::from([(0, Type::String)]),
            HashMap::from([(0, "abc".to_string())]),
            HashMap::new(),
        );
    }

    #[test]
    fn test_assign_var() {
        test_statement(
            Stat::Assign(
                AssignLhs::Var(0),
                AssignRhs::Array(ASTWrapper(
                    Some(Type::Array(box Type::Char, 1)),
                    vec![
                        ASTWrapper(Some(Type::Char), Expr::Char('a')),
                        ASTWrapper(Some(Type::Char), Expr::Char('b')),
                    ],
                )),
            ),
            vec![ir::Stat::AssignVar(
                0,
                ir::Expr::Ptr(ir::PtrExpr::Malloc(vec![
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 2)),
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, 'a' as u8 as i32)),
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, 'b' as u8 as i32)),
                ])),
            )],
            vec![],
            HashMap::from([(0, Type::Array(box Type::Int, 1))]),
            HashMap::new(),
            HashMap::new(),
        );
    }

    #[test]
    fn test_assign_array_elem() {
        test_statement(
            Stat::Assign(
                AssignLhs::ArrayElem(0, vec![ASTWrapper(Some(Type::Int), Expr::Int(0))]),
                AssignRhs::Expr(ASTWrapper(Some(Type::Char), Expr::Char('z'))),
            ),
            vec![ir::Stat::AssignPtr(
                ir::PtrExpr::Call(
                    ARRAY_INDEX_FNAME.to_string(),
                    vec![
                        ir::Expr::Ptr(ir::PtrExpr::Var(0)),
                        ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
                        ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                    ],
                ),
                ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, 'z' as u8 as i32)),
            )],
            vec![],
            HashMap::from([(0, Type::Array(box Type::Char, 1))]),
            HashMap::new(),
            HashMap::new(),
        );
    }

    #[test]
    fn test_assign_array_elem_ptr() {
        test_statement(
            Stat::Assign(
                AssignLhs::ArrayElem(0, vec![ASTWrapper(Some(Type::Int), Expr::Int(0))]),
                AssignRhs::Expr(ASTWrapper(
                    Some(Type::Char),
                    Expr::ArrayElem(1, vec![ASTWrapper(Some(Type::Int), Expr::Int(3))]),
                )),
            ),
            vec![ir::Stat::AssignPtr(
                ir::PtrExpr::Call(
                    ARRAY_INDEX_FNAME.to_string(),
                    vec![
                        ir::Expr::Ptr(ir::PtrExpr::Var(0)),
                        ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
                        ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                    ],
                ),
                ir::Expr::Num(ir::NumExpr::Deref(
                    ir::NumSize::Byte,
                    ir::PtrExpr::Call(
                        ARRAY_INDEX_FNAME.to_string(),
                        vec![
                            ir::Expr::Ptr(ir::PtrExpr::Var(1)),
                            ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 3)),
                            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                        ],
                    ),
                )),
            )],
            vec![],
            HashMap::from([
                (0, Type::Array(box Type::Char, 1)),
                (1, Type::Array(box Type::Char, 1)),
            ]),
            HashMap::new(),
            HashMap::new(),
        );
    }

    #[test]
    fn test_read_var() {
        test_statement(
            Stat::Read(AssignLhs::Var(0)),
            vec![ir::Stat::ReadIntVar(0)],
            vec![],
            HashMap::from([(0, Type::Int)]),
            HashMap::new(),
            HashMap::new(),
        );
    }

    #[test]
    fn test_read_ptr() {
        test_statement(
            Stat::Read(AssignLhs::ArrayElem(
                0,
                vec![ASTWrapper(Some(Type::Char), Expr::Int(0))],
            )),
            vec![ir::Stat::ReadCharPtr(ir::PtrExpr::Call(
                ARRAY_INDEX_FNAME.to_string(),
                vec![
                    ir::Expr::Ptr(ir::PtrExpr::Var(0)),
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
                    ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::Byte))),
                ],
            ))],
            vec![],
            HashMap::from([(0, Type::Array(box Type::Char, 1))]),
            HashMap::new(),
            HashMap::new(),
        );
    }
}
