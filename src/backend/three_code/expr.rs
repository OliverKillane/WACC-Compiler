use super::{super::Options, stat::get_type_width, BinOp, OpSrc, Size, StatCode, StatLine};
use crate::intermediate::{self as ir, VarRepr};
use std::{
    collections::HashMap,
    iter::{successors, zip},
};

impl From<i32> for OpSrc {
    fn from(num: i32) -> Self {
        OpSrc::Const(num)
    }
}

/// Helper function for translating function call expressions.
fn translate_function_call(
    name: String,
    args: Vec<ir::Expr>,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) {
    let stat_code = StatCode::Call(
        result,
        name,
        zip(
            args,
            successors(Some(result), |arg_result| Some(*arg_result + 1)),
        )
        .map(|(expr, arg_result)| {
            translate_expr(expr, arg_result, stat_line, vars, function_types, options);
            arg_result
        })
        .collect(),
    );
    stat_line.add_stat(stat_code);
}

impl From<ir::NumSize> for Size {
    fn from(size: ir::NumSize) -> Self {
        match size {
            ir::NumSize::DWord => Size::DWord,
            ir::NumSize::Word => Size::Word,
            ir::NumSize::Byte => Size::Byte,
        }
    }
}

impl From<ir::ArithOp> for BinOp {
    fn from(arith_op: ir::ArithOp) -> Self {
        match arith_op {
            ir::ArithOp::Add => BinOp::Add,
            ir::ArithOp::Sub => BinOp::Sub,
            ir::ArithOp::Mul => BinOp::Mul,
            ir::ArithOp::Div => BinOp::Div,
            ir::ArithOp::Mod => BinOp::Mod,
        }
    }
}

/// Translates a numerical expression into a series of statements. The result of the
/// expression tree is placed in the result field. Returns the size of the resulting
/// numerical expression, i.e. the size of the variable placed in result.
/// It is assumed that no variables after the result variable are used.
pub(super) fn translate_num_expr(
    num_expr: ir::NumExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) -> ir::NumSize {
    match num_expr {
        ir::NumExpr::SizeOf(expr_type) => {
            let type_width: i32 = get_type_width(expr_type).into();
            stat_line.add_stat(StatCode::Assign(result, OpSrc::from(type_width)));
            ir::NumSize::DWord
        }
        ir::NumExpr::SizeOfWideAlloc => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::from(4)));
            ir::NumSize::DWord
        }
        ir::NumExpr::Const(size, val) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::from(val)));
            size
        }
        ir::NumExpr::Var(var) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
            let size = match vars.get(&var).expect("Variable not found") {
                ir::Type::Num(size) => *size,
                _ => panic!("Variable of a wrong type"),
            };
            size.into()
        }
        ir::NumExpr::Deref(size, ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, result, size.into()));
            size.into()
        }
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let size1 =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let size2 = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                arith_op.into(),
                OpSrc::Var(result + 1),
            ));
            size1
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let old_size =
                translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            match (size, old_size) {
                (ir::NumSize::Byte, ir::NumSize::DWord | ir::NumSize::Word) => {
                    stat_line.add_stat(StatCode::AssignOp(
                        result,
                        OpSrc::Var(result),
                        BinOp::And,
                        OpSrc::from(0xFF),
                    ));
                }
                (ir::NumSize::Word, ir::NumSize::DWord) => {
                    stat_line.add_stat(StatCode::AssignOp(
                        result,
                        OpSrc::Var(result),
                        BinOp::And,
                        OpSrc::from(0xFFFF),
                    ));
                }
                _ => {}
            }
            size.into()
        }
        ir::NumExpr::Call(name, args) => {
            let size = if let ir::Type::Num(size) =
                function_types.get(&name).expect("Function not found")
            {
                *size
            } else {
                panic!("Function has a wrong return type")
            };
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
            size.into()
        }
    }
}

impl From<ir::BoolOp> for BinOp {
    fn from(bool_op: ir::BoolOp) -> Self {
        match bool_op {
            ir::BoolOp::And => BinOp::And,
            ir::BoolOp::Or => BinOp::Or,
            ir::BoolOp::Xor => BinOp::Xor,
        }
    }
}

/// Translates a boolean expression into a series of statements. The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used.
pub(super) fn translate_bool_expr(
    bool_expr: ir::BoolExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) {
    match bool_expr {
        ir::BoolExpr::Const(bool_const) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::from(bool_const as i32)));
        }
        ir::BoolExpr::Var(var) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
        }
        ir::BoolExpr::Deref(ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, result, Size::Byte));
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::And,
                OpSrc::from(0x01),
            ));
        }
        ir::BoolExpr::TestZero(num_expr) => {
            translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Eq,
                OpSrc::from(0x00),
            ));
        }
        ir::BoolExpr::TestPositive(num_expr) => {
            translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Gt,
                OpSrc::from(0x00),
            ));
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            translate_ptr_expr(ptr_expr1, result, stat_line, vars, function_types, options);
            translate_ptr_expr(
                ptr_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Eq,
                OpSrc::Var(result + 1),
            ));
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            translate_bool_expr(bool_expr1, result, stat_line, vars, function_types, options);
            translate_bool_expr(
                bool_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                bool_op.into(),
                OpSrc::Var(result + 1),
            ));
        }
        ir::BoolExpr::Not(box bool_expr) => {
            let bool_const =
                translate_bool_expr(bool_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Xor,
                OpSrc::from(0x01),
            ));
        }
        ir::BoolExpr::Call(name, args) => {
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
        }
    }
}

/// Translates a pointer expression into a series of statements. The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used.
pub(super) fn translate_ptr_expr(
    ptr_expr: ir::PtrExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) {
    match ptr_expr {
        ir::PtrExpr::Null => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Const(0)));
        }
        ir::PtrExpr::DataRef(data_ref) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::DataRef(data_ref, 0)));
        }
        ir::PtrExpr::Var(var) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
        }
        ir::PtrExpr::Deref(box ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, result, Size::DWord));
        }
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            translate_num_expr(
                num_expr,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::Add,
                OpSrc::Var(result + 1),
            ));
        }
        malloc @ ir::PtrExpr::Malloc(_) | malloc @ ir::PtrExpr::WideMalloc(_) => {
            let is_wide = if let ir::PtrExpr::WideMalloc(_) = malloc {
                true
            } else {
                false
            };
            let exprs = if let ir::PtrExpr::Malloc(exprs) = malloc {
                exprs
            } else if let ir::PtrExpr::WideMalloc(exprs) = malloc {
                exprs
            } else {
                unreachable!();
            };

            let (width, width_prefixes) = exprs
                .into_iter()
                .map(|expr| {
                    let mut setting_stat_line = StatLine::new(stat_line.graph());
                    let expr_type = translate_expr(
                        expr,
                        result + 1,
                        &mut setting_stat_line,
                        vars,
                        function_types,
                        options,
                    );
                    (
                        if is_wide {
                            4
                        } else {
                            get_type_width(expr_type).into()
                        },
                        get_type_width(expr_type),
                        setting_stat_line,
                    )
                })
                .fold(
                    (0, Vec::new()),
                    |(total_width, mut width_prefixes),
                     (mem_width, store_width, setting_stat_line)| {
                        width_prefixes.push((total_width, store_width, setting_stat_line));
                        (total_width + mem_width, width_prefixes)
                    },
                );

            if width_prefixes.len() > 0 {
                stat_line.add_stat(StatCode::Assign(result, OpSrc::from(width)));
                stat_line.add_stat(StatCode::Call(result, "malloc".to_string(), vec![result]));
                for (offset, store_width, setting_stat_line) in width_prefixes {
                    stat_line.splice(
                        setting_stat_line
                            .start_node()
                            .expect("No calculation of the expression value"),
                        setting_stat_line
                            .end_node()
                            .expect("No calculation of the expression value"),
                    );
                    stat_line.add_stat(StatCode::AssignOp(
                        result + 2,
                        OpSrc::Var(result),
                        BinOp::Add,
                        OpSrc::from(offset),
                    ));
                    stat_line.add_stat(StatCode::Store(result + 2, result + 1, store_width))
                }
            } else {
                stat_line.add_stat(StatCode::Assign(result, OpSrc::Const(0)))
            }
        }
        ir::PtrExpr::Call(name, args) => {
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
        }
    }
}

/// Translates a single general expression. The result of the expression is placed
/// in the result variable. Returns the type of the expression. It is assumed that
/// no variables after the result variable are used.
pub(super) fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) -> ir::Type {
    match expr {
        ir::Expr::Num(num_expr) => {
            let size =
                translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            ir::Type::Num(size)
        }
        ir::Expr::Bool(bool_expr) => {
            translate_bool_expr(bool_expr, result, stat_line, vars, function_types, options);
            ir::Type::Bool
        }
        ir::Expr::Ptr(ptr_expr) => {
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            ir::Type::Ptr
        }
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    use super::{
        super::{
            super::{Options, PropagationOpt},
            OpSrc, Size, StatCode, StatType,
        },
        translate_expr,
    };
    use crate::{
        backend::three_code::{BinOp, StatLine},
        graph::Graph,
        intermediate::{self as ir, VarRepr},
    };

    fn match_line_stat_vec(
        expr: ir::Expr,
        result: VarRepr,
        stats: Vec<StatCode>,
        vars: &HashMap<VarRepr, ir::Type>,
        function_types: &HashMap<String, ir::Type>,
        expr_type: ir::Type,
    ) {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut stat_line = StatLine::new(graph.clone());
        assert_eq!(
            translate_expr(
                expr,
                result,
                &mut stat_line,
                vars,
                function_types,
                &Options {
                    sethi_ullman_weights: false,
                    dead_code_removal: false,
                    propagation: PropagationOpt::None,
                    inlining: false,
                    tail_call: false,
                    hoisting: false,
                    strength_reduction: false,
                    loop_unrolling: false,
                    common_expressions: false
                }
            ),
            expr_type
        );
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
    fn translate_sizeof_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Bool)),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(1))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::DWord),
        );
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::SizeOfWideAlloc),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(4))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::DWord),
        );
    }

    #[test]
    fn translate_const_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Word, 42)),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(42))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
        )
    }

    #[test]
    fn translate_var_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            &HashMap::from([(1, ir::Type::Num(ir::NumSize::Word))]),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
        )
    }

    #[test]
    fn translate_deref_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Deref(
                ir::NumSize::Word,
                ir::PtrExpr::DataRef(0),
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::DataRef(0, 0)),
                StatCode::Load(0, 0, Size::Word),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
        )
    }

    #[test]
    fn translate_arith_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::ArithOp(
                box ir::NumExpr::Const(ir::NumSize::Byte, 1),
                ir::ArithOp::Add,
                box ir::NumExpr::Const(ir::NumSize::Byte, 2),
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Assign(1, OpSrc::Const(2)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::Add, OpSrc::Var(1)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::Byte),
        )
    }

    #[test]
    fn translate_cast_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Cast(
                ir::NumSize::Byte,
                box ir::NumExpr::Var(1),
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Var(1)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::And, OpSrc::Const(0xff)),
            ],
            &HashMap::from([(1, ir::Type::Num(ir::NumSize::DWord))]),
            &HashMap::new(),
            ir::Type::Num(ir::NumSize::Byte),
        )
    }

    #[test]
    fn translate_call_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Call(
                "f".to_string(),
                vec![
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 1)),
                    ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Byte, 255)),
                ],
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Assign(1, OpSrc::Const(255)),
                StatCode::Call(0, "f".to_string(), vec![0, 1]),
            ],
            &HashMap::new(),
            &HashMap::from([("f".to_string(), ir::Type::Num(ir::NumSize::Word))]),
            ir::Type::Num(ir::NumSize::Word),
        )
    }

    #[test]
    fn translate_const_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Const(true)),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(1))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_var_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            &HashMap::from([(1, ir::Type::Bool)]),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_deref_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Deref(ir::PtrExpr::DataRef(0))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::DataRef(0, 0)),
                StatCode::Load(0, 0, Size::Byte),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::And, OpSrc::Const(0x01)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_test_zero_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::TestZero(ir::NumExpr::Const(
                ir::NumSize::Word,
                0,
            ))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::Eq, OpSrc::Const(0)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_test_positive_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::TestPositive(ir::NumExpr::Const(
                ir::NumSize::Word,
                0,
            ))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::Gt, OpSrc::Const(0)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_ptr_eq_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::PtrEq(ir::PtrExpr::Null, ir::PtrExpr::Null)),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::Eq, OpSrc::Var(1)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_bin_op_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::BoolOp(
                box ir::BoolExpr::Const(true),
                ir::BoolOp::And,
                box ir::BoolExpr::Const(false),
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::And, OpSrc::Var(1)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_call_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Call(
                "f".to_string(),
                vec![
                    ir::Expr::Bool(ir::BoolExpr::Const(true)),
                    ir::Expr::Bool(ir::BoolExpr::Const(false)),
                ],
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(1)),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::Call(0, "f".to_string(), vec![0, 1]),
            ],
            &HashMap::new(),
            &HashMap::from([("f".to_string(), ir::Type::Bool)]),
            ir::Type::Bool,
        )
    }

    #[test]
    fn translate_null_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Null),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(1))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        )
    }

    #[test]
    fn translate_dataref_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::DataRef(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::DataRef(1, 0))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        )
    }

    #[test]
    fn translate_var_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            &HashMap::from([(1, ir::Type::Ptr)]),
            &HashMap::new(),
            ir::Type::Ptr,
        )
    }

    #[test]
    fn translate_deref_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::DataRef(0))),
            0,
            vec![
                StatCode::Assign(0, OpSrc::DataRef(0, 0)),
                StatCode::Load(0, 0, Size::DWord),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        )
    }

    #[test]
    fn translate_offset_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Offset(
                box ir::PtrExpr::Null,
                box ir::NumExpr::Const(ir::NumSize::DWord, 2),
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(0)),
                StatCode::Assign(1, OpSrc::Const(2)),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::Add, OpSrc::Var(1)),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        )
    }

    #[test]
    fn translate_malloc_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Malloc(vec![
                ir::Expr::Bool(ir::BoolExpr::Const(true)),
                ir::Expr::Ptr(ir::PtrExpr::Null),
            ])),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(5)),
                StatCode::Call(0, "malloc".to_string(), vec![0]),
                StatCode::Assign(1, OpSrc::Const(1)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(0)),
                StatCode::Store(2, 1, Size::Byte),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(1)),
                StatCode::Store(2, 1, Size::DWord),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        );
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::WideMalloc(vec![
                ir::Expr::Bool(ir::BoolExpr::Const(true)),
                ir::Expr::Ptr(ir::PtrExpr::Null),
            ])),
            0,
            vec![
                StatCode::Assign(0, OpSrc::Const(8)),
                StatCode::Call(0, "malloc".to_string(), vec![0]),
                StatCode::Assign(1, OpSrc::Const(1)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(0)),
                StatCode::Store(2, 1, Size::Byte),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(4)),
                StatCode::Store(2, 1, Size::DWord),
            ],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        );
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Malloc(vec![])),
            0,
            vec![StatCode::Assign(0, OpSrc::Const(0))],
            &HashMap::new(),
            &HashMap::new(),
            ir::Type::Ptr,
        );
    }

    #[test]
    fn translate_call_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Call(
                "f".to_string(),
                vec![
                    ir::Expr::Ptr(ir::PtrExpr::DataRef(0)),
                    ir::Expr::Ptr(ir::PtrExpr::Null),
                ],
            )),
            0,
            vec![
                StatCode::Assign(0, OpSrc::DataRef(0, 0)),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::Call(0, "f".to_string(), vec![0, 1]),
            ],
            &HashMap::new(),
            &HashMap::from([("f".to_string(), ir::Type::Ptr)]),
            ir::Type::Ptr,
        )
    }
}
