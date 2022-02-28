use core::panic;
use std::collections::HashMap;

use super::{
    super::ast::{ASTWrapper, Stat, StatWrap, Type as ASTType},
    trans_expr::translate_expr,
};
use crate::{
    frontend::{ast, semantic::symbol_table::VariableSymbolTable},
    intermediate::{BlockEnding, BoolExpr, DataRef, NumExpr, NumSize, PtrExpr},
};

use super::super::super::intermediate::{Expr as IRExpr, Stat as IRStat, Type as IRType};
use crate::intermediate::{Expr::*, NumSize::*, PtrExpr::Malloc, Stat::AssignVar, Stat::*};

#[inline]
pub fn exit(num_expr: NumExpr) -> BlockEnding {
    BlockEnding::Exit(num_expr)
}

#[inline]
pub fn ret(expr: IRExpr) -> BlockEnding {
    BlockEnding::Return(expr)
}

//use this to decide size when freeing
fn get_ir_type_from_ast_type(ast_type: ASTType) -> IRType {
    match ast_type {
        ASTType::Int => IRType::Num(NumSize::DWord),
        ASTType::Bool => IRType::Bool,
        ASTType::Char => IRType::Num(NumSize::Byte),
        ASTType::Generic(_) => panic!(
            "What are you even doing with your life by this point Oli? Semantic analyzer broken!"
        ),
        _ => IRType::Ptr,
    }
}

// will need to make use of a block graph
pub fn translate_stat(
    ASTWrapper(_, ast_stat): StatWrap<Option<ast::Type>, usize>,
    var_symb: &VariableSymbolTable,
    dataref_map: &mut HashMap<DataRef, Vec<IRExpr>>,
    ir_stats: &mut Vec<IRStat>,
) {
    match ast_stat {
        Stat::Skip => (),
        Stat::Def(typ, var, asn_rhs) => match asn_rhs {
            ast::AssignRhs::Expr(expr_span) => ir_stats.push(AssignVar(
                var,
                translate_expr(expr_span, var_symb, dataref_map),
            )),
            ast::AssignRhs::Array(ASTWrapper(_, arr_elems)) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();
                for expr_span in arr_elems {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }
                ir_expr_vec.insert(0, Num(NumExpr::Const(DWord, ir_expr_vec.len() as i32)));
                ir_stats.push(AssignVar(var, Ptr(Malloc(ir_expr_vec))))
            }
            ast::AssignRhs::Call(ASTWrapper(_, fname), args) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();

                for expr_span in args {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }

                match typ {
                    ast::Type::Int | ast::Type::Char => {
                        ir_stats.push(AssignVar(var, Num(NumExpr::Call(fname, ir_expr_vec))))
                    }
                    ast::Type::Bool => {
                        ir_stats.push(AssignVar(var, Bool(BoolExpr::Call(fname, ir_expr_vec))))
                    }
                    ast::Type::String
                    | ast::Type::Any
                    | ast::Type::Pair(_, _)
                    | ast::Type::Array(_, _) => {
                        ir_stats.push(AssignVar(var, Ptr(PtrExpr::Call(fname, ir_expr_vec))))
                    }
                    _ => panic!("Why send me a Generic dumb Oli?"),
                }
            }
        },
        Stat::Assign(_asn_lhs, _asn_rhs) => {
            todo!()
        }
        Stat::Read(_) => todo!(),
        Stat::Free(ast_expr) => {
            if let Ptr(ptr_expr) = translate_expr(ast_expr.clone(), var_symb, dataref_map) {
                if let ASTWrapper(Some(ast_type), _) = ast_expr {
                    ir_stats.push(Free(
                        ptr_expr,
                        NumExpr::SizeOf(get_ir_type_from_ast_type(ast_type)),
                    ))
                } else {
                    panic!("Tried to free undefined type!")
                }
            } else {
                panic!("Tried to free a non-ptr expression!")
            }
        }
        Stat::Return(ast_expr) => {
            ret(translate_expr(ast_expr, var_symb, dataref_map));
        }
        Stat::Exit(ast_expr) => {
            if let Num(num_expr) = translate_expr(ast_expr, var_symb, dataref_map) {
                exit(num_expr);
            } else {
                panic!("Tried to exit without a valid code!")
            }
        }
        Stat::Print(ast_expr) => {
            let ir_expr = translate_expr(ast_expr.clone(), var_symb, dataref_map);
            match ast_expr.0 {
                None => panic!("Tried to print undefined type!"),
                Some(typ) => match typ {
                    ast::Type::Char => {
                        if let Num(num_expr) = ir_expr {
                            ir_stats.push(PrintChar(num_expr))
                        } else {
                            panic!("Tried to print non-char as char!")
                        }
                    }
                    ast::Type::String => {
                        if let Ptr(ptr_expr) = ir_expr {
                            if let ast::Expr::String(str) = ast_expr.1 {
                                ir_stats.push(PrintStr(
                                    ptr_expr,
                                    NumExpr::Const(DWord, str.len() as i32),
                                ))
                            }
                        } else {
                            panic!("Tried to print non-string as a string!")
                        }
                    }
                    ast::Type::Generic(_) => panic!("Why send me a Generic dumb Oli?"),
                    _ => ir_stats.push(PrintExpr(ir_expr)),
                },
            }
        }
        Stat::PrintLn(ast_expr) => {
            translate_stat(
                ASTWrapper(None, Stat::Print(ast_expr)),
                var_symb,
                dataref_map,
                ir_stats,
            );
            ir_stats.push(PrintEol())
        }
        Stat::If(_, _, _) => todo!(),
        Stat::While(_, _) => todo!(),
        Stat::Block(_) => todo!(),
    }
}
