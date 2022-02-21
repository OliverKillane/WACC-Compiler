use super::{
    super::Options,
    expr::{translate_bool_expr, translate_expr, translate_num_expr, translate_ptr_expr},
    OpSrc, Size, StatCode, StatGraph, StatNode, StatType,
};
use crate::graph::{Deleted, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;

pub(super) fn get_type_width(expr_type: ir::Type) -> Size {
    match expr_type {
        ir::Type::Num(ir::NumSize::DWord) => Size::DWord,
        ir::Type::Num(ir::NumSize::Word) => Size::Word,
        ir::Type::Num(ir::NumSize::Byte) => Size::Byte,
        ir::Type::Bool => Size::Byte,
        ir::Type::Ptr => Size::DWord,
    }
}

impl Into<i32> for Size {
    fn into(self) -> i32 {
        match self {
            Size::DWord => 4,
            Size::Word => 2,
            Size::Byte => 1,
        }
    }
}

struct PrintFmtDataRefFlags {
    integer: Option<DataRef>,
    ptr: Option<DataRef>,
    bool_true: Option<DataRef>,
    bool_false: Option<DataRef>,
    string: Option<DataRef>,
    char: Option<DataRef>,
    eol: Option<DataRef>,
}

fn push_stats_statgraph(
    stats: Vec<StatCode>,
    stat_graph: &mut StatGraph,
    end: &mut Option<StatNode>,
) {
    for stat in stats {
        let new_node = stat_graph.graph.new_node(StatType::new_final(stat));
        if let Some(end) = end {
            let mut end_node = end.set(StatType::deleted());
            if let StatType::Final(incoming, stat_code) = end_node {
                end_node = StatType::Simple(incoming, stat_code, new_node.clone());
            } else {
                panic!("End node not a terminal node")
            }
            end.set(end_node);
            new_node.get_mut().add_incoming(end.clone());
        } else {
            stat_graph.start = Some(new_node.clone());
        }
        *end = Some(new_node);
    }
}

fn ensure_format(
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, Vec<u8>>,
    format: &str,
    fmt_flag: &mut Option<DataRef>,
) -> DataRef {
    if let Some(format) = *fmt_flag {
        format
    } else {
        *fmt_flag = Some(*free_data_ref);
        let mut format = format.to_owned().into_bytes();
        format.push(0);
        data_refs
            .insert(*free_data_ref, format)
            .map(|_| panic!("Data reference already used"));
        let data_ref = *free_data_ref;
        *free_data_ref += 1;
        data_ref
    }
}

fn translate_statement(
    stat: ir::Stat,
    free_var: VarRepr,
    stat_graph: &mut StatGraph,
    end_stat: &mut Option<StatNode>,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, Vec<u8>>,
    print_fmt_flags: &mut PrintFmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) {
    match stat {
        ir::Stat::AssignVar(var, expr) => {
            let mut stats = Vec::new();
            translate_expr(expr, var, &mut stats, vars, functions);
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::AssignPtr(ptr_expr, expr) => {
            let mut stats = Vec::new();
            let store_width =
                get_type_width(translate_expr(expr, free_var, &mut stats, vars, functions));
            let ptr_const = translate_ptr_expr(ptr_expr, free_var + 1, &mut stats, vars, functions);
            stats.push(StatCode::StoreVar(free_var + 1, free_var, store_width));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::Free(ptr_expr, _) => {
            let mut stats = Vec::new();
            translate_ptr_expr(ptr_expr, free_var, &mut stats, vars, functions);
            stats.push(StatCode::Call(free_var, "free".to_string(), vec![free_var]));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintExpr(ir::Expr::Num(num_expr)) => {
            let mut stats = Vec::new();
            translate_num_expr(num_expr, free_var, &mut stats, vars, functions);
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%d", &mut print_fmt_flags.integer);
            stats.push(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stats.push(StatCode::Call(
                free_var,
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintExpr(ir::Expr::Bool(bool_expr)) => {
            let mut stats = Vec::new();
            translate_bool_expr(bool_expr, free_var, &mut stats, vars, functions);
            push_stats_statgraph(stats, stat_graph, end_stat);
            let true_format = ensure_format(
                free_data_ref,
                data_refs,
                "true",
                &mut print_fmt_flags.bool_true,
            );
            let false_format = ensure_format(
                free_data_ref,
                data_refs,
                "false",
                &mut print_fmt_flags.bool_false,
            );

            let print_node = stat_graph
                .graph
                .new_node(StatType::new_final(StatCode::Call(
                    free_var,
                    "printf".to_string(),
                    vec![free_var + 1],
                )));
            let true_set = stat_graph.graph.new_node(StatType::new_simple(
                StatCode::Assign(free_var + 1, OpSrc::DataRef(true_format, 0)),
                print_node.clone(),
            ));
            let false_set = stat_graph.graph.new_node(StatType::new_simple(
                StatCode::Assign(free_var + 1, OpSrc::DataRef(false_format, 0)),
                print_node.clone(),
            ));
            print_node.get_mut().add_incoming(true_set.clone());
            print_node.get_mut().add_incoming(false_set.clone());

            let branch_node = stat_graph.graph.new_node(StatType::new_branch(
                free_var,
                true_set.clone(),
                false_set.clone(),
            ));
            true_set.get_mut().add_incoming(branch_node.clone());
            false_set.get_mut().add_incoming(branch_node.clone());
            if let Some(end_stat) = end_stat {
                let mut end_node = end_stat.set(StatType::deleted());
                if let StatType::Final(incoming, stat_code) = end_node {
                    end_node = StatType::Simple(incoming, stat_code, branch_node.clone());
                } else {
                    unreachable!()
                }
                end_stat.set(end_node);
                branch_node.get_mut().add_incoming(end_stat.clone());
            } else {
                panic!("No calculation steps for the boolean constant");
            }

            *end_stat = Some(print_node);
        }
        ir::Stat::PrintExpr(ir::Expr::Ptr(ptr_expr)) => {
            let mut stats = Vec::new();
            translate_ptr_expr(ptr_expr, free_var, &mut stats, vars, functions);
            let hex_format =
                ensure_format(free_data_ref, data_refs, "%x", &mut print_fmt_flags.ptr);
            stats.push(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(hex_format, 0),
            ));
            stats.push(StatCode::Call(
                free_var,
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintChar(num_expr) => {
            let mut stats = Vec::new();
            translate_num_expr(num_expr, free_var, &mut stats, vars, functions);
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%c", &mut print_fmt_flags.char);
            stats.push(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stats.push(StatCode::Call(
                free_var,
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintStr(ptr_expr, num_expr) => {
            let mut stats = Vec::new();
            translate_ptr_expr(ptr_expr, free_var, &mut stats, vars, functions);
            translate_num_expr(num_expr, free_var + 1, &mut stats, vars, functions);
            let string_format = ensure_format(
                free_data_ref,
                data_refs,
                "%.*s",
                &mut print_fmt_flags.string,
            );
            stats.push(StatCode::Assign(
                free_var + 2,
                OpSrc::DataRef(string_format, 0),
            ));
            stats.push(StatCode::Call(
                free_var,
                "printf".to_string(),
                vec![free_var + 2, free_var + 1, free_var],
            ));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintEol() => {
            let mut stats = Vec::new();
            let eol_format =
                ensure_format(free_data_ref, data_refs, "\n", &mut print_fmt_flags.eol);
            stats.push(StatCode::Assign(free_var, OpSrc::DataRef(eol_format, 0)));
            stats.push(StatCode::Call(
                free_var,
                "printf".to_string(),
                vec![free_var],
            ));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        _ => todo!(),
    }
}
