use std::collections::HashMap;

use crate::{frontend::{ast, semantic::symbol_table::VariableSymbolTable}, intermediate::{DataRef, NumExpr, BoolExpr}};
use super::{super::ast::{Stat, StatSpan, WrapSpan}, trans_expr::translate_expr};

use crate::intermediate::Stat::AssignVar;
use crate::intermediate::PtrExpr::Malloc;
use crate::intermediate::Expr::*;
use super::super::super::intermediate::Stat as IRStat;
use super::super::super::intermediate::Expr as IRExpr;

pub fn translate_stat<'a>(
    WrapSpan(_, ast_stat): StatSpan<'a, usize>,
    var_symb: &VariableSymbolTable,
    dataref_map: &mut HashMap<DataRef, Vec<IRExpr>>
) -> Option<IRStat> {
    match ast_stat{
        Stat::Skip => None,
        Stat::Def(typ, var, asn_rhs) => match asn_rhs {
            ast::AssignRhs::Expr(expr_span) => Some(AssignVar(var, translate_expr(expr_span, var_symb, dataref_map))),
            ast::AssignRhs::Array(WrapSpan(_, arr_elems)) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();
                for expr_span in arr_elems {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }
                Some(AssignVar(var, Ptr(Malloc(ir_expr_vec))))
            },
            ast::AssignRhs::Call(fname, args) => {
                let mut ir_expr_vec: Vec<IRExpr> = Vec::new();

                for expr_span in args {
                    ir_expr_vec.push(translate_expr(expr_span, var_symb, dataref_map));
                }

                match typ {
                ast::Type::Int | ast::Type::Char  => Some(AssignVar(var, Num(NumExpr::Call(String::from(fname), ir_expr_vec)))),
                ast::Type::Bool => Some(AssignVar(var, Bool(BoolExpr::Call(String::from(fname), ir_expr_vec)))),
                ast::Type::String => todo!(),
                ast::Type::Any => todo!(),
                ast::Type::Pair(_, _) => todo!(),
                ast::Type::Array(_, _) => todo!(),
                _ => panic!("Why send me a Generic dumb Oli?")
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