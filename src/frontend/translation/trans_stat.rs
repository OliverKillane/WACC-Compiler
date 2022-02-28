use std::collections::HashMap;

use super::{
    super::ast::{ASTWrapper, Stat, StatWrap},
    trans_expr::translate_expr,
};
use crate::{
    frontend::{ast, semantic::symbol_table::VariableSymbolTable},
    intermediate::{BoolExpr, DataRef, NumExpr, PtrExpr},
};

use super::super::super::intermediate::Expr as IRExpr;
use super::super::super::intermediate::Stat as IRStat;
use crate::intermediate::{Expr::*, NumSize::*, PtrExpr::Malloc, Stat::AssignVar};

pub fn translate_stat(
    ASTWrapper(_, ast_stat): StatWrap<Option<ast::Type>, usize>,
    var_symb: &VariableSymbolTable,
    dataref_map: &mut HashMap<DataRef, Vec<IRExpr>>,
) -> Option<IRStat> {
    match ast_stat {
        Stat::Skip => None,
        Stat::Def(typ, var, asn_rhs) => match asn_rhs {
            ast::AssignRhs::Expr(expr_span) => Some(AssignVar(
                var,
                translate_expr(expr_span, var_symb, dataref_map),
            )),
            ast::AssignRhs::Array(ASTWrapper(_, arr_elems)) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();
                for expr_span in arr_elems {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }
                ir_expr_vec.insert(0, Num(NumExpr::Const(DWord, ir_expr_vec.len() as i32)));
                Some(AssignVar(var, Ptr(Malloc(ir_expr_vec))))
            }
            ast::AssignRhs::Call(ASTWrapper(_, fname), args) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();

                for expr_span in args {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }

                match typ {
                    ast::Type::Int | ast::Type::Char => {
                        Some(AssignVar(var, Num(NumExpr::Call(fname, ir_expr_vec))))
                    }
                    ast::Type::Bool => {
                        Some(AssignVar(var, Bool(BoolExpr::Call(fname, ir_expr_vec))))
                    }
                    ast::Type::String
                    | ast::Type::Any
                    | ast::Type::Pair(_, _)
                    | ast::Type::Array(_, _) => {
                        Some(AssignVar(var, Ptr(PtrExpr::Call(fname, ir_expr_vec))))
                    }
                    _ => panic!("Why send me a Generic dumb Oli?"),
                }
            }
        },
        Stat::Assign(_, _) => todo!(),
        Stat::Read(_) => todo!(),
        Stat::Free(_) => todo!(),
        Stat::Return(_) => todo!(),
        Stat::Exit(_) => todo!(),
        Stat::Print(_) => todo!(),
        Stat::PrintLn(_) => todo!(),
        Stat::If(_, _, _) => todo!(),
        Stat::While(_, _) => todo!(),
        Stat::Block(_) => todo!(),
    }
}
