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
            translate_expr(expr, free_var, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Assign(var, OpSrc::Var(free_var)));
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
                        vec![free_var],
                    )));
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
            let hex_format = ensure_format(free_data_ref, data_refs, "%p", &mut fmt_flags.ptr);
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

#[cfg(test)]
mod tests {
    use super::super::{
        super::{Options, PropagationOpt},
        DataRefType, OpSrc, Size, StatCode, StatType,
    };
    use super::translate_statement;
    use crate::{
        backend::three_code::{stat::FmtDataRefFlags, StatLine},
        graph::Graph,
        intermediate::{self as ir, DataRef, VarRepr},
    };
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    fn match_line_stat_vec(
        stat: ir::Stat,
        free_var: VarRepr,
        stats: Vec<StatCode>,
        vars: HashMap<VarRepr, ir::Type>,
        function_types: HashMap<String, ir::Type>,
        mut initial_data_refs: HashMap<DataRef, DataRefType>,
        final_data_refs: HashMap<DataRef, DataRefType>,
        has_read_ref: bool,
    ) {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut stat_line = StatLine::new(graph.clone());
        let mut free_data_ref = initial_data_refs.keys().max().map(|x| *x).unwrap_or(0);
        let mut read_ref = None;
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
                sethi_ullman_weights: false,
                dead_code_removal: false,
                propagation: PropagationOpt::None,
                inlining: false,
                tail_call: false,
                hoisting: false,
                strength_reduction: false,
                loop_unrolling: false,
                common_expressions: false,
            },
        );

        assert_eq!(read_ref.map(|_| true).unwrap_or(false), has_read_ref);
        assert_eq!(initial_data_refs, final_data_refs);

        let mut node = stat_line.start_node().expect("No start node");
        let mut stats = stats.into_iter();
        while node != stat_line.end_node().expect("No end node") {
            let next_node = if let StatType::Simple(_, node_stat_code, next_node) = &*node.get() {
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
        if let StatType::Final(_, node_stat_code) = &*node.get() {
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
            vec![
                StatCode::Assign(1, OpSrc::Const(1)),
                StatCode::Assign(0, OpSrc::Var(1)),
            ],
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
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::Store(1, 0, Size::Byte),
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
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::Store(1, 0, Size::DWord),
                StatCode::Assign(2, OpSrc::DataRef(1, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![2, 1]),
                StatCode::Load(0, 1, Size::DWord),
            ],
            HashMap::from([(0, ir::Type::Num(ir::NumSize::DWord))]),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(1, DataRefType::String("%d\0".as_bytes().to_vec()))]),
            true,
        )
    }

    #[test]
    fn translate_read_char_var_stat() {
        match_line_stat_vec(
            ir::Stat::ReadCharVar(0),
            1,
            vec![
                StatCode::Assign(1, OpSrc::DataRef(0, 0)),
                StatCode::Store(1, 0, Size::Byte),
                StatCode::Assign(2, OpSrc::DataRef(1, 0)),
                StatCode::VoidCall("scanf".to_string(), vec![2, 1]),
                StatCode::Load(0, 1, Size::Byte),
            ],
            HashMap::from([(0, ir::Type::Num(ir::NumSize::Byte))]),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(1, DataRefType::String("%c\0".as_bytes().to_vec()))]),
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
            HashMap::from([(0, DataRefType::String("%c\0".as_bytes().to_vec()))]),
            false,
        )
    }

    #[test]
    fn translate_free_stat() {
        match_line_stat_vec(
            ir::Stat::Free(ir::PtrExpr::Null, ir::NumExpr::Const(ir::NumSize::DWord, 0)),
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
            &mut None,
            &mut FmtDataRefFlags::default(),
            &HashMap::new(),
            &HashMap::new(),
            &Options {
                sethi_ullman_weights: false,
                dead_code_removal: false,
                propagation: PropagationOpt::None,
                inlining: false,
                tail_call: false,
                hoisting: false,
                strength_reduction: false,
                loop_unrolling: false,
                common_expressions: false,
            },
        );

        assert_eq!(
            data_refs,
            HashMap::from([
                (0, DataRefType::String("true\0".as_bytes().to_vec())),
                (1, DataRefType::String("false\0".as_bytes().to_vec()))
            ])
        );

        let node = stat_line.start_node().expect("No start node");
        let node =
            if let StatType::Simple(_, StatCode::Assign(0, OpSrc::Const(1)), node) = &*node.get() {
                node.clone()
            } else {
                panic!("Node type mismatch")
            };
        let (node1, node2) = if let StatType::Branch(_, 0, node1, node2) = &*node.get() {
            (node1.clone(), node2.clone())
        } else {
            panic!("Node type mismatch")
        };
        let node = if let StatType::Simple(_, StatCode::Assign(0, OpSrc::DataRef(0, 0)), node) =
            &*node1.get()
        {
            node.clone()
        } else {
            panic!("Node type mismatch")
        };
        if let StatType::Simple(_, StatCode::Assign(0, OpSrc::DataRef(1, 0)), other_node) =
            &*node2.get()
        {
            assert_eq!(&node, other_node);
        } else {
            panic!("Node type mismatch")
        }
        if let StatType::Final(_, StatCode::VoidCall(fname, args)) = &*node.get() {
            assert_eq!(fname, "printf");
            assert_eq!(args, &vec![0]);
        } else {
            panic!("Node type mismatch")
        };
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
            ],
            HashMap::new(),
            HashMap::new(),
            HashMap::new(),
            HashMap::from([(0, DataRefType::String("\n\0".as_bytes().to_vec()))]),
            false,
        )
    }
}
