//! Translation of [ir statements](ir::Stat) into [threecode statements](StatCode)
//!
//! Flattening allows for better analysis for optimisations.
//!
//! The [ir](ir) is constructed as a block graph, and is split into nodes per
//! statement.

use super::{
    super::Options,
    expr::{
        assign_op_src, translate_bool_expr, translate_expr, translate_num_expr, translate_ptr_expr,
    },
    DataRefType, OpSrc, Size, StatCode, StatLine, StatType,
};
use crate::{
    graph::Deleted,
    intermediate::{self as ir, DataRef, VarRepr},
};
use std::{
    collections::HashMap,
    iter::{successors, zip},
};

/// Converts the [intermediate representation type](ir::Type) to a
/// [three code size](Size).
pub(super) fn get_type_width(expr_type: ir::Type) -> Size {
    match expr_type {
        ir::Type::Num(ir::NumSize::DWord) => Size::DWord,
        ir::Type::Num(ir::NumSize::Word) => Size::Word,
        ir::Type::Num(ir::NumSize::Byte) => Size::Byte,
        ir::Type::Bool => Size::Byte,
        ir::Type::Ptr => Size::DWord,
    }
}

impl From<Size> for i32 {
    fn from(s: Size) -> i32 {
        match s {
            Size::DWord => 4,
            Size::Word => 2,
            Size::Byte => 1,
        }
    }
}

/// Stores the information about static strings for printf and scanf already
/// present in the dataref map.
#[derive(Default)]
pub(super) struct FmtDataRefFlags {
    /// "%d" null-terminated string
    integer: Option<DataRef>,
    /// "%p" null-terminated string
    ptr: Option<DataRef>,
    /// "true" null-terminated string
    bool_true: Option<DataRef>,
    /// "false" null-terminated string
    bool_false: Option<DataRef>,
    /// "%.*s" null-terminated string
    string: Option<DataRef>,
    /// " %c" null-terminated string
    char_in: Option<DataRef>,
    /// "%c" null-terminated string
    char_out: Option<DataRef>,
    /// "\n" null-terminated string
    eol: Option<DataRef>,
}

/// If a format string  is not present in the static data references then adds
/// it and returns its [reference](DataRef)
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
        if data_refs
            .insert(*free_data_ref, DataRefType::String(format))
            .is_some()
        {
            panic!("Data reference already used")
        }
        let data_ref = *free_data_ref;
        *free_data_ref += 1;
        data_ref
    }
}

/// Adds a call to a flush function
fn add_flush(free_var: VarRepr, stat_line: &mut StatLine) {
    stat_line.add_stat(StatCode::Assign(free_var, OpSrc::Const(0)));
    stat_line.add_stat(StatCode::VoidCall("fflush".to_string(), vec![free_var]));
}

/// Translates a single [statement](ir::Stat) into a
/// [statement graph with a single start and end node](StatLine). The arguments
/// are as follows:
///  - The statement to be translated.
///  - Some variable not used in the program. It is assumed that all
///    variables superceding are not used either.
///  - The graph to which the put the statements to.
///  - Some data reference not used in the program. It is assumed that all data
///    references superceding are not used either. Will be automatically updated
///    if a data reference is placed under it.
///  - All static data references used in the program.
///  - An optional data reference to a read field. The field is supposed to be
///    4 bytes long. The initial value of that field should be set to Null.
///    Updated if a data reference is needed.
///  - [Format flags](FmtDataRefFlags). See the type comment for more detail.
///    Updated if necessary.
///  - Types of variables used within the intermediate representation.
///  - Types of return values of functions used within the intermediate
///    representation.
///  - [Compilation options](Options).
#[allow(clippy::too_many_arguments)]
pub(super) fn translate_statement(
    stat: ir::Stat,
    free_var: VarRepr,
    stat_line: &mut StatLine,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut bool,
    fmt_flags: &mut FmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) {
    match stat {
        ir::Stat::AssignVar(var, expr) => {
            let (_, op_src) =
                translate_expr(expr, free_var, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Assign(var, op_src));
        }
        ir::Stat::AssignPtr(ptr_expr, expr) => {
            let (expr_type, op_src) =
                translate_expr(expr, free_var, stat_line, vars, function_types, options);
            assign_op_src(free_var, op_src, stat_line);
            let store_width = get_type_width(expr_type);
            let op_src = translate_ptr_expr(
                ptr_expr,
                free_var + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::Store(op_src, free_var, store_width));
        }
        ir::Stat::Free(ptr_expr) => {
            assign_op_src(
                free_var,
                translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options),
                stat_line,
            );
            stat_line.add_stat(StatCode::VoidCall("free".to_string(), vec![free_var]));
        }
        ir::Stat::Call(name, args) => {
            let stat_code = StatCode::VoidCall(
                name,
                zip(
                    args,
                    successors(Some(free_var), |arg_result| Some(*arg_result + 1)),
                )
                .map(|(expr, arg_result)| {
                    let (_, op_src) =
                        translate_expr(expr, arg_result, stat_line, vars, function_types, options);
                    assign_op_src(arg_result, op_src, stat_line);
                    arg_result
                })
                .collect(),
            );
            stat_line.add_stat(stat_code);
        }
        ir::Stat::ReadIntVar(var) => {
            *read_ref = true;
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%d", &mut fmt_flags.integer);
            stat_line.add_stat(StatCode::Store(OpSrc::ReadRef, var, Size::DWord));
            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::ReadRef));
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
            stat_line.add_stat(StatCode::Load(var, OpSrc::ReadRef, Size::DWord));
        }
        ir::Stat::ReadIntPtr(ptr_expr) => {
            assign_op_src(
                free_var,
                translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options),
                stat_line,
            );
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
            *read_ref = true;
            let char_format =
                ensure_format(free_data_ref, data_refs, " %c", &mut fmt_flags.char_in);
            stat_line.add_stat(StatCode::Store(OpSrc::ReadRef, var, Size::Byte));
            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::ReadRef));
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(char_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "scanf".to_string(),
                vec![free_var + 1, free_var],
            ));
            stat_line.add_stat(StatCode::Load(var, OpSrc::ReadRef, Size::Byte));
        }
        ir::Stat::ReadCharPtr(ptr_expr) => {
            assign_op_src(
                free_var,
                translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options),
                stat_line,
            );
            let char_format =
                ensure_format(free_data_ref, data_refs, " %c", &mut fmt_flags.char_in);
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
            let (_, op_src) =
                translate_num_expr(num_expr, free_var, stat_line, vars, function_types, options);
            assign_op_src(free_var, op_src, stat_line);
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
            add_flush(free_var, stat_line);
        }
        ir::Stat::PrintExpr(ir::Expr::Bool(bool_expr)) => {
            let op_src = translate_bool_expr(
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

            let dummy_node = stat_line.graph().borrow_mut().new_node(StatType::deleted());
            let print_node = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_simple(
                    StatCode::VoidCall("printf".to_string(), vec![free_var]),
                    dummy_node.clone(),
                ));
            let true_set = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_simple(
                    StatCode::Assign(free_var, OpSrc::DataRef(true_format, 0)),
                    print_node.clone(),
                ));
            let false_set = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_simple(
                    StatCode::Assign(free_var, OpSrc::DataRef(false_format, 0)),
                    print_node.clone(),
                ));
            dummy_node.get_mut().add_incoming(print_node.clone());
            print_node.get_mut().add_incoming(true_set.clone());
            print_node.get_mut().add_incoming(false_set.clone());

            let branch_node = stat_line
                .graph()
                .borrow_mut()
                .new_node(StatType::new_branch(
                    op_src,
                    true_set.clone(),
                    false_set.clone(),
                ));
            true_set.get_mut().add_incoming(branch_node.clone());
            false_set.get_mut().add_incoming(branch_node.clone());

            stat_line.splice(branch_node, print_node);
            add_flush(free_var, stat_line);
        }
        ir::Stat::PrintExpr(ir::Expr::Ptr(ptr_expr)) => {
            assign_op_src(
                free_var,
                translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options),
                stat_line,
            );
            let hex_format = ensure_format(free_data_ref, data_refs, "%p", &mut fmt_flags.ptr);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(hex_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
            add_flush(free_var, stat_line);
        }
        ir::Stat::PrintChar(num_expr) => {
            let (_, op_src) =
                translate_num_expr(num_expr, free_var, stat_line, vars, function_types, options);
            assign_op_src(free_var, op_src, stat_line);
            let integer_format =
                ensure_format(free_data_ref, data_refs, "%c", &mut fmt_flags.char_out);
            stat_line.add_stat(StatCode::Assign(
                free_var + 1,
                OpSrc::DataRef(integer_format, 0),
            ));
            stat_line.add_stat(StatCode::VoidCall(
                "printf".to_string(),
                vec![free_var + 1, free_var],
            ));
            add_flush(free_var, stat_line);
        }
        ir::Stat::PrintStr(ptr_expr, num_expr) => {
            assign_op_src(
                free_var,
                translate_ptr_expr(ptr_expr, free_var, stat_line, vars, function_types, options),
                stat_line,
            );
            let (_, op_src) = translate_num_expr(
                num_expr,
                free_var + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            assign_op_src(free_var + 1, op_src, stat_line);
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
            add_flush(free_var, stat_line);
        }
        ir::Stat::PrintEol() => {
            let eol_format = ensure_format(free_data_ref, data_refs, "\n", &mut fmt_flags.eol);
            stat_line.add_stat(StatCode::Assign(free_var, OpSrc::DataRef(eol_format, 0)));
            stat_line.add_stat(StatCode::VoidCall("printf".to_string(), vec![free_var]));
            add_flush(free_var, stat_line);
        }
    }
}

#[cfg(test)]
pub(super) mod tests {
    use super::super::{super::Options, DataRefType, OpSrc, Size, StatCode, StatNode, StatType};
    use super::translate_statement;
    use crate::{
        backend::three_code::{stat::FmtDataRefFlags, StatLine},
        graph::{Deleted, Graph},
        intermediate::{self as ir, DataRef, VarRepr},
    };
    use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

    fn match_graph_dfs(
        exec_node: StatNode,
        template_node: StatNode,
        visited_map: &mut HashMap<StatNode, StatNode>,
    ) {
        if let Some(other_template_node) = visited_map.get(&exec_node) {
            assert_eq!(other_template_node, &template_node);
            return;
        }
        visited_map.insert(exec_node.clone(), template_node.clone());
        match (exec_node.get().deref(), template_node.get().deref()) {
            (StatType::Dummy(_), StatType::Dummy(_)) => {}
            (
                StatType::Simple(_, exec_stat_code, next_exec_node),
                StatType::Simple(_, template_stat_code, next_template_node),
            ) => {
                assert_eq!(exec_stat_code, template_stat_code);
                match_graph_dfs(
                    next_exec_node.clone(),
                    next_template_node.clone(),
                    visited_map,
                )
            }
            (
                StatType::Branch(_, exec_branch_var, next_true_exec_node, next_false_exec_node),
                StatType::Branch(
                    _,
                    template_branch_var,
                    next_true_template_node,
                    next_false_template_node,
                ),
            ) => {
                assert_eq!(exec_branch_var, template_branch_var);
                match_graph_dfs(
                    next_true_exec_node.clone(),
                    next_true_template_node.clone(),
                    visited_map,
                );
                match_graph_dfs(
                    next_false_exec_node.clone(),
                    next_false_template_node.clone(),
                    visited_map,
                );
            }
            (StatType::Loop(_), StatType::Loop(_)) => {}
            (StatType::Return(_, exec_ret_val), StatType::Return(_, template_ret_val)) => {
                assert_eq!(exec_ret_val, template_ret_val);
            }
            _ => panic!(
                "Stat type mismatch: expected {:?}, but got {:?}",
                template_node.get().deref(),
                exec_node.get().deref()
            ),
        }
    }

    pub(in super::super) fn match_graph(exec_start: StatNode, template_start: StatNode) {
        let mut visited_map = HashMap::new();
        match_graph_dfs(exec_start, template_start, &mut visited_map);
    }

    #[allow(clippy::too_many_arguments)]
    fn match_line_stat_vec(
        stat: ir::Stat,
        free_var: VarRepr,
        stats: Vec<StatCode>,
        vars: HashMap<VarRepr, ir::Type>,
        function_types: HashMap<String, Option<ir::Type>>,
        mut initial_data_refs: HashMap<DataRef, DataRefType>,
        final_data_refs: HashMap<DataRef, DataRefType>,
        has_read_ref: bool,
    ) {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut stat_line = StatLine::new(graph.clone());
        let mut free_data_ref = initial_data_refs.keys().max().map(|x| *x).unwrap_or(0);
        let mut read_ref = false;
        let mut fmt_flags = FmtDataRefFlags::default();
        translate_statement(
            stat,
            free_var,
            &mut stat_line,
            &mut free_data_ref,
            &mut initial_data_refs,
            &mut read_ref,
            &mut fmt_flags,
            &vars,
            &function_types,
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
                show_optimised_three_code: false,
            },
        );

        assert_eq!(read_ref, has_read_ref);
        assert_eq!(initial_data_refs, final_data_refs);

        let mut node = stat_line.start_node().expect("No start node");
        let mut stats = stats.into_iter();
        while node != stat_line.end_node().expect("No end node") {
            let next_node =
                if let StatType::Simple(_, node_stat_code, next_node) = node.get().deref() {
                    assert_eq!(
                        *node_stat_code,
                        stats.next().expect("More statements than expected")
                    );
                    next_node.clone()
                } else {
                    panic!("Expected a simple statement")
                };
            node = next_node;
        }
        if let StatType::Simple(_, node_stat_code, _) = node.get().deref() {
            assert_eq!(
                *node_stat_code,
                stats.next().expect("More statements than expected")
            );
        } else {
            panic!("Expected a final statement")
        };
        assert_eq!(stats.next(), None)
    }

    #[test]
    fn translate_assign_var_stat() {
        match_line_stat_vec(
            ir::Stat::AssignVar(0, ir::Expr::Bool(ir::BoolExpr::Const(true))),
            1,
            vec![StatCode::Assign(0, OpSrc::Const(1))],
            HashMap::from([(0, ir::Type::Bool)]),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            false,
        )
    }

    #[test]
    fn translate_assign_ptr_stat() {
        match_line_stat_vec(
            ir::Stat::AssignPtr(ir::PtrExpr::Null, ir::Expr::Bool(ir::BoolExpr::Const(true))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Store(OpSrc::Const(0), 0, Size::Byte),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            false,
        )
    }

    #[test]
    fn translate_read_int_var_stat() {
        match_line_stat_vec(
            ir::Stat::ReadIntVar(0),
            1,
            vec![
                StatCode::Store(OpSrc::ReadRef, 0, Size::DWord),
                StatCode::Assign(1, OpSrc::ReadRef),
                StatCode::Assign(2, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![2, 1]),
                StatCode::Load(0, OpSrc::ReadRef, Size::DWord),
            ],
            HashMap::from([(0, ir::Type::Num(ir::NumSize::DWord))]),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%d\0".as_bytes().to_vec()))]),
            true,
        )
    }

    #[test]
    fn translate_read_char_var_stat() {
        match_line_stat_vec(
            ir::Stat::ReadCharVar(0),
            1,
            vec![
                StatCode::Store(OpSrc::ReadRef, 0, Size::Byte),
                StatCode::Assign(1, OpSrc::ReadRef),
                StatCode::Assign(2, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![2, 1]),
                StatCode::Load(0, OpSrc::ReadRef, Size::Byte),
            ],
            HashMap::from([(0, ir::Type::Num(ir::NumSize::Byte))]),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String(" %c\0".as_bytes().to_vec()))]),
            true,
        )
    }

    #[test]
    fn translate_read_int_ptr_stat() {
        match_line_stat_vec(
            ir::Stat::ReadIntPtr(ir::PtrExpr::Null),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![1, 0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%d\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_read_char_ptr_stat() {
        match_line_stat_vec(
            ir::Stat::ReadCharPtr(ir::PtrExpr::Null),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![1, 0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String(" %c\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_free_stat() {
        match_line_stat_vec(
            ir::Stat::Free(ir::PtrExpr::Null),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("free".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            false,
        )
    }

    #[test]
    fn translate_print_num_expr_stat() {
        match_line_stat_vec(
            ir::Stat::PrintExpr(ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 0))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("printf".to_string(), vec![1, 0]),
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("fflush".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%d\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_print_bool_expr_stat() {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut stat_line = StatLine::new(graph);
        let mut data_refs = HashMap::new();
        translate_statement(
            ir::Stat::PrintExpr(ir::Expr::Bool(ir::BoolExpr::Const(true))),
            0,
            &mut stat_line,
            &mut 0,
            &mut data_refs,
            &mut false,
            &mut FmtDataRefFlags::default(),
            &HashMap::new(),
            &HashMap::new(),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
                show_optimised_three_code: false,
            },
        );

        assert_eq!(
            data_refs,
            HashMap::from([
                (0, DataRefType::String("true\0".as_bytes().to_vec())),
                (1, DataRefType::String("false\0".as_bytes().to_vec()))
            ])
        );

        let mut template_graph = Graph::new();
        let dummy_node = template_graph.new_node(StatType::deleted());
        let flush_node = template_graph.new_node(StatType::new_simple(
            StatCode::VoidCall("fflush".to_string(), vec![0]),
            dummy_node,
        ));
        let stdout_set_node = template_graph.new_node(StatType::new_simple(
            StatCode::Assign(0, OpSrc::Const(0)),
            flush_node,
        ));
        let call_node = template_graph.new_node(StatType::new_simple(
            StatCode::VoidCall("printf".to_string(), vec![0]),
            stdout_set_node,
        ));

        let true_branch_node = template_graph.new_node(StatType::new_simple(
            StatCode::Assign(0, OpSrc::DataRef(0, 0)),
            call_node.clone(),
        ));
        let false_branch_node = template_graph.new_node(StatType::new_simple(
            StatCode::Assign(0, OpSrc::DataRef(1, 0)),
            call_node,
        ));
        let branch_node = template_graph.new_node(StatType::new_branch(
            OpSrc::Const(1),
            true_branch_node,
            false_branch_node,
        ));

        match_graph(stat_line.start_node().expect("No start node"), branch_node);
    }

    #[test]
    fn translate_print_ptr_expr_stat() {
        match_line_stat_vec(
            ir::Stat::PrintExpr(ir::Expr::Ptr(ir::PtrExpr::Null)),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("printf".to_string(), vec![1, 0]),
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("fflush".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%p\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_print_char_stat() {
        match_line_stat_vec(
            ir::Stat::PrintChar(ir::NumExpr::Const(ir::NumSize::Byte, 1)),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("printf".to_string(), vec![1, 0]),
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("fflush".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%c\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_print_str_stat() {
        match_line_stat_vec(
            ir::Stat::PrintStr(ir::PtrExpr::Null, ir::NumExpr::Const(ir::NumSize::DWord, 4)),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::Const(4)),
                StatCode::Assign(2, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("printf".to_string(), vec![2, 1, 0]),
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("fflush".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("%.*s\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_print_eol_stat() {
        match_line_stat_vec(
            ir::Stat::PrintEol(),
            0,
            vec![
                StatCode::Assign(0, OpSrc::DataRef(0, 0)),
                StatCode::VoidCall("printf".to_string(), vec![0]),
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::VoidCall("fflush".to_string(), vec![0]),
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("\n\0".as_bytes().to_vec()))]),
            false,
        )
    }
}
