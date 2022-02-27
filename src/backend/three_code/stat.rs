use super::{
    super::Options,
    expr::{translate_bool_expr, translate_expr, translate_num_expr, translate_ptr_expr},
    DataRefType, OpSrc, Size, StatCode, StatLine, StatType,
};
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

#[derive(Default)]
pub(super) struct FmtDataRefFlags {
    integer: Option<DataRef>,
    ptr: Option<DataRef>,
    bool_true: Option<DataRef>,
    bool_false: Option<DataRef>,
    string: Option<DataRef>,
    char: Option<DataRef>,
    eol: Option<DataRef>,
}

fn ensure_format(
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
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
            .insert(*free_data_ref, DataRefType::String(format))
            .map(|_| panic!("Data reference already used"));
        let data_ref = *free_data_ref;
        *free_data_ref += 1;
        data_ref
    }
}

pub(super) fn translate_statement(
    stat: ir::Stat,
    free_var: VarRepr,
    stat_line: &mut StatLine,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut Option<DataRef>,
    fmt_flags: &mut FmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) {
    match stat {
        ir::Stat::AssignVar(var, expr) => {
            translate_expr(expr, var, stat_line, vars, function_types, options);
        }
        ir::Stat::AssignPtr(ptr_expr, expr) => {
            let store_width = get_type_width(translate_expr(
                expr,
                free_var,
                stat_line,
                vars,
                function_types,
                options,
            ));
            let ptr_const = translate_ptr_expr(
                ptr_expr,
                free_var + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::Store(free_var + 1, free_var, store_width));
        }
        ir::Stat::Free(ptr_expr, _) => {
            translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::VoidCall("free".to_string(), vec![free_var]));
        }
        ir::Stat::ReadIntVar(var) => {
            let read_ref = *read_ref.get_or_insert_with(|| {
                let data_ref = *free_data_ref;
                *free_data_ref += 1;
                data_ref
            });
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%d", &mut fmt_flags.integer);

            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::DataRef(read_ref, 0)));
            stat_line.add_stat(StatCode::Store(free_var, var, Size::DWord));
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
            stat_line.add_stat(StatCode::Load(var, free_var, Size::DWord));
        }
        ir::Stat::ReadIntPtr(ptr_expr) => {
            translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options);
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%d", &mut fmt_flags.integer);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
        }
        ir::Stat::ReadCharVar(var) => {
            let read_ref = *read_ref.get_or_insert_with(|| {
                let data_ref = *free_data_ref;
                *free_data_ref += 1;
                data_ref
            });
            let char_format = ensure_format(free_data_ref, data_refs, "%c", &mut fmt_flags.char);
            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::DataRef(read_ref, 0)));
            stat_line.add_stat(StatCode::Store(free_var, var, Size::Byte));
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(char_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
            stat_line.add_stat(StatCode::Load(var, free_var, Size::Byte));
        }
        ir::Stat::ReadCharPtr(ptr_expr) => {
            translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options);
            let char_format = ensure_format(free_data_ref, data_refs, "%c", &mut fmt_flags.char);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(char_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
        }
        ir::Stat::PrintExpr(ir::Expr::Num(num_expr)) => {
            translate_num_expr(num_expr, free_var, stat_line, vars, function_types, options);
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%d", &mut fmt_flags.integer);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
        }
        ir::Stat::PrintExpr(ir::Expr::Bool(bool_expr)) => {
            translate_bool_expr(
                bool_expr,
                free_var,
                stat_line,
                vars,
                function_types,
                options,
            );
            let true_format =
                ensure_format(free_data_ref, data_refs, "true", &mut fmt_flags.bool_true);
            let false_format =
                ensure_format(free_data_ref, data_refs, "false", &mut fmt_flags.bool_false);

            let print_node =
                stat_line
                    .graph()
                    .borrow_mut()
                    .new_node(StatType::new_final(StatCode::VoidCall(
                        "printf".to_string(),
                        vec![free_var + 1],
                    )));
            let true_set = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_simple(
                    StatCode::Assign(free_var + 1, OpSrc::DataRef(true_format, 0)),
                    print_node.clone(),
                ));
            let false_set = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_simple(
                    StatCode::Assign(free_var + 1, OpSrc::DataRef(false_format, 0)),
                    print_node.clone(),
                ));
            print_node.get_mut().add_incoming(true_set.clone());
            print_node.get_mut().add_incoming(false_set.clone());

            let branch_node = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_branch(
                    free_var,
                    true_set.clone(),
                    false_set.clone(),
                ));
            true_set.get_mut().add_incoming(branch_node.clone());
            false_set.get_mut().add_incoming(branch_node.clone());

            stat_line.splice(branch_node, print_node);
        }
        ir::Stat::PrintExpr(ir::Expr::Ptr(ptr_expr)) => {
            translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options);
            let hex_format = ensure_format(free_data_ref, data_refs, "%x", &mut fmt_flags.ptr);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(hex_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
        }
        ir::Stat::PrintChar(num_expr) => {
            translate_num_expr(num_expr, free_var, stat_line, vars, function_types, options);
            let integer_format = ensure_format(free_data_ref, data_refs, "%c", &mut fmt_flags.char);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
        }
        ir::Stat::PrintStr(ptr_expr, num_expr) => {
            translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options);
            translate_num_expr(
                num_expr,
                free_var + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            let string_format =
                ensure_format(free_data_ref, data_refs, "%.*s", &mut fmt_flags.string);
            stat_line.add_stat(StatCode::Assign(
                free_var + 2,
                OpSrc::DataRef(string_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 2, free_var + 1, free_var],
            ));
        }
        ir::Stat::PrintEol() => {
            let eol_format = ensure_format(free_data_ref, data_refs, "\n", &mut fmt_flags.eol);
            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::DataRef(eol_format, 0)));
            stat_line.add_stat(StatCode::VoidCall("printf".to_string(), vec![free_var]));
        }
    }
}
