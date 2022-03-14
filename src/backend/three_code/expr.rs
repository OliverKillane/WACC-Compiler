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
    function_types: &HashMap<String, Option<ir::Type>>,
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
            let (_, op_src) =
                translate_expr(expr, arg_result, stat_line, vars, function_types, options);
            assign_op_src(arg_result, op_src, stat_line);
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

pub(super) fn assign_op_src(result: VarRepr, op_src: OpSrc, stat_line: &mut StatLine) {
    if let OpSrc::Var(var) = op_src {
        assert_eq!(var, result);
        return;
    }
    stat_line.add_stat(StatCode::Assign(result, op_src));
}

/// Translates a [numerical expression](ir::NumExpr) into a [series of statements](StatLine). The result of the
/// expression tree is placed in the result field. Returns the size of the resulting
/// numerical expression, i.e. the size of the variable placed in result.
/// It is assumed that no variables after the result variable are used. Returns the [op source](OpSrc) to use for
/// the statement.
pub(super) fn translate_num_expr(
    num_expr: ir::NumExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> (ir::NumSize, OpSrc) {
    match num_expr {
        ir::NumExpr::SizeOf(expr_type) => {
            let width: i32 = get_type_width(expr_type).into();
            (ir::NumSize::DWord, OpSrc::from(width))
        }
        ir::NumExpr::SizeOfWideAlloc => (ir::NumSize::DWord, OpSrc::from(4)),
        ir::NumExpr::Const(size, val) => (size, OpSrc::from(val)),
        ir::NumExpr::Var(var) => {
            let size = match vars.get(&var).expect("Variable not found") {
                ir::Type::Num(size) => *size,
                _ => panic!("Variable of a wrong type"),
            };
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
            (size, OpSrc::Var(result))
        }
        ir::NumExpr::Deref(size, ptr_expr) => {
            let op_src =
                translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, op_src, size.into()));
            (size, OpSrc::Var(result))
        }
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let (size, op_src1) =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(
                result,
                op_src1,
                arith_op.into(),
                op_src2,
            ));
            (size, OpSrc::Var(result))
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let (old_size, op_src) =
                translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            match (size, old_size) {
                (ir::NumSize::Byte, ir::NumSize::DWord | ir::NumSize::Word) => {
                    stat_line.add_stat(StatCode::AssignOp(
                        result,
                        op_src,
                        BinOp::And,
                        OpSrc::from(0xFF),
                    ));
                }
                (ir::NumSize::Word, ir::NumSize::DWord) => {
                    stat_line.add_stat(StatCode::AssignOp(
                        result,
                        op_src,
                        BinOp::And,
                        OpSrc::from(0xFFFF),
                    ));
                }
                _ => {}
            }
            (size, OpSrc::Var(result))
        }
        ir::NumExpr::Call(name, args) => {
            let size = if let Some(ir::Type::Num(size)) =
                function_types.get(&name).expect("Function not found")
            {
                *size
            } else {
                panic!("Function has a wrong return type")
            };
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
            (size, OpSrc::Var(result))
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

/// Translates a [boolean expression](ir::BoolExpr) into a [series of statements](StatLine). The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used. Returns the [op source](OpSrc) to use for
/// the statement.
pub(super) fn translate_bool_expr(
    bool_expr: ir::BoolExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> OpSrc {
    match bool_expr {
        ir::BoolExpr::Const(bool_const) => OpSrc::from(bool_const as i32),
        ir::BoolExpr::Var(var) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
            OpSrc::Var(result)
        }
        ir::BoolExpr::Deref(ptr_expr) => {
            let op_src =
                translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, op_src, Size::Byte));
            stat_line.add_stat(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::And,
                OpSrc::from(0x01),
            ));
            OpSrc::Var(result)
        }
        ir::BoolExpr::NumEq(num_expr1, num_expr2) => {
            let (_, op_src1) =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Eq, op_src2));
            OpSrc::Var(result)
        }
        ir::BoolExpr::NumLt(num_expr1, num_expr2) => {
            let (_, op_src1) =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Lt, op_src2));
            OpSrc::Var(result)
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            let op_src1 =
                translate_ptr_expr(ptr_expr1, result, stat_line, vars, function_types, options);
            let op_src2 = translate_ptr_expr(
                ptr_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Eq, op_src2));
            OpSrc::Var(result)
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            let op_src1 =
                translate_bool_expr(bool_expr1, result, stat_line, vars, function_types, options);
            let op_src2 = translate_bool_expr(
                bool_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, bool_op.into(), op_src2));
            OpSrc::Var(result)
        }
        ir::BoolExpr::Not(box ir::BoolExpr::NumEq(num_expr1, num_expr2)) => {
            let (_, op_src1) =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Ne, op_src2));
            OpSrc::Var(result)
        }
        ir::BoolExpr::Not(box ir::BoolExpr::NumLt(num_expr1, num_expr2)) => {
            let (_, op_src1) =
                translate_num_expr(num_expr1, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr2,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Gte, op_src2));
            OpSrc::Var(result)
        }

        ir::BoolExpr::Not(box bool_expr) => {
            let op_src =
                translate_bool_expr(bool_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::AssignOp(
                result,
                op_src,
                BinOp::Xor,
                OpSrc::from(0x01),
            ));
            OpSrc::Var(result)
        }
        ir::BoolExpr::Call(name, args) => {
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
            OpSrc::Var(result)
        }
    }
}

/// Translates a [pointer expression](ir::PtrExpr) into a [series of statements](StatLine). The result of the
/// expression tree is placed in the result field. It is assumed that no variables
/// after the result variable are used. Returns the [op source](OpSrc) to use for
/// the statement.
pub(super) fn translate_ptr_expr(
    ptr_expr: ir::PtrExpr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> OpSrc {
    match ptr_expr {
        ir::PtrExpr::Null => OpSrc::Const(0),
        ir::PtrExpr::DataRef(data_ref) => OpSrc::DataRef(data_ref, 0),
        ir::PtrExpr::Var(var) => {
            stat_line.add_stat(StatCode::Assign(result, OpSrc::Var(var)));
            OpSrc::Var(result)
        }
        ir::PtrExpr::Deref(box ptr_expr) => {
            let op_src =
                translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            stat_line.add_stat(StatCode::Load(result, op_src, Size::DWord));
            OpSrc::Var(result)
        }
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            let op_src1 =
                translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options);
            let (_, op_src2) = translate_num_expr(
                num_expr,
                result + 1,
                stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::AssignOp(result, op_src1, BinOp::Add, op_src2));
            OpSrc::Var(result)
        }
        malloc @ ir::PtrExpr::Malloc(_) | malloc @ ir::PtrExpr::WideMalloc(_) => {
            let is_wide = matches!(malloc, ir::PtrExpr::WideMalloc(_));
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
                    let (expr_type, op_src) = translate_expr(
                        expr,
                        result + 1,
                        &mut setting_stat_line,
                        vars,
                        function_types,
                        options,
                    );
                    assign_op_src(result + 1, op_src, &mut setting_stat_line);
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
                    (0, vec![]),
                    |(total_width, mut width_prefixes),
                     (mem_width, store_width, setting_stat_line)| {
                        width_prefixes.push((total_width, store_width, setting_stat_line));
                        (total_width + mem_width, width_prefixes)
                    },
                );

            if !width_prefixes.is_empty() {
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
                    stat_line.add_stat(StatCode::Store(
                        OpSrc::Var(result + 2),
                        result + 1,
                        store_width,
                    ))
                }
                OpSrc::Var(result)
            } else {
                OpSrc::Const(0)
            }
        }
        ir::PtrExpr::Call(name, args) => {
            translate_function_call(name, args, result, stat_line, vars, function_types, options);
            OpSrc::Var(result)
        }
    }
}

/// Translates a single [general expression](ir::Expr). The result of the expression is placed
/// in the result variable. Returns the type of the expression. It is assumed that
/// no variables after the result variable are used. Returns the [op source](OpSrc) to use for
/// the statement.
pub(super) fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stat_line: &mut StatLine,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> (ir::Type, OpSrc) {
    match expr {
        ir::Expr::Num(num_expr) => {
            let (size, op_src) =
                translate_num_expr(num_expr, result, stat_line, vars, function_types, options);
            (ir::Type::Num(size), op_src)
        }
        ir::Expr::Bool(bool_expr) => (
            ir::Type::Bool,
            translate_bool_expr(bool_expr, result, stat_line, vars, function_types, options),
        ),
        ir::Expr::Ptr(ptr_expr) => (
            ir::Type::Ptr,
            translate_ptr_expr(ptr_expr, result, stat_line, vars, function_types, options),
        ),
    }
}

#[cfg(test)]
mod tests {
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
    use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

    fn match_line_stat_vec(
        expr: ir::Expr,
        result: VarRepr,
        stats: Vec<StatCode>,
        vars: HashMap<VarRepr, ir::Type>,
        function_types: HashMap<String, Option<ir::Type>>,
        expr_type: ir::Type,
        op_src: OpSrc,
    ) {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut stat_line = StatLine::new(graph.clone());
        assert_eq!(
            translate_expr(
                expr,
                result,
                &mut stat_line,
                &vars,
                &function_types,
                &Options {
                    sethi_ullman_weights: false,
                    dead_code_removal: false,
                    propagation: PropagationOpt::None,
                    inlining: Some(1000),
                    tail_call: false,
                    hoisting: false,
                    strength_reduction: false,
                    loop_unrolling: false,
                    common_expressions: false,
                    show_arm_temp_rep: false
                }
            ),
            (expr_type, op_src)
        );
        if stats.is_empty() {
            return;
        }
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
    fn translate_sizeof_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::SizeOf(ir::Type::Bool)),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::DWord),
            OpSrc::Const(1),
        );
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::SizeOfWideAlloc),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::DWord),
            OpSrc::Const(4),
        );
    }

    #[test]
    fn translate_const_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::Word, 42)),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
            OpSrc::Const(42),
        )
    }

    #[test]
    fn translate_var_num_expr() {
        match_line_stat_vec(
            ir::Expr::Num(ir::NumExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            HashMap::from([(1, ir::Type::Num(ir::NumSize::Word))]),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
            OpSrc::Var(0),
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
            vec![StatCode::Load(0, OpSrc::DataRef(0, 0), Size::Word)],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::Word),
            OpSrc::Var(0),
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
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(1),
                BinOp::Add,
                OpSrc::Const(2),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::Byte),
            OpSrc::Var(0),
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
            HashMap::from([(1, ir::Type::Num(ir::NumSize::DWord))]),
            HashMap::new(),
            ir::Type::Num(ir::NumSize::Byte),
            OpSrc::Var(0),
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
            HashMap::new(),
            HashMap::from([("f".to_string(), Some(ir::Type::Num(ir::NumSize::Word)))]),
            ir::Type::Num(ir::NumSize::Word),
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_const_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Const(true)),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Const(1),
        )
    }

    #[test]
    fn translate_var_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            HashMap::from([(1, ir::Type::Bool)]),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_deref_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Deref(ir::PtrExpr::DataRef(0))),
            0,
            vec![
                StatCode::Load(0, OpSrc::DataRef(0, 0), Size::Byte),
                StatCode::AssignOp(0, OpSrc::Var(0), BinOp::And, OpSrc::Const(0x01)),
            ],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_num_eq_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::NumEq(
                ir::NumExpr::Const(ir::NumSize::Word, 420),
                ir::NumExpr::Const(ir::NumSize::Word, 69),
            )),
            0,
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(420),
                BinOp::Eq,
                OpSrc::Const(69),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_num_lt_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::NumLt(
                ir::NumExpr::Const(ir::NumSize::Word, 420),
                ir::NumExpr::Const(ir::NumSize::Word, 69),
            )),
            0,
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(420),
                BinOp::Lt,
                OpSrc::Const(69),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_not_num_eq_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::NumEq(
                ir::NumExpr::Const(ir::NumSize::Word, 420),
                ir::NumExpr::Const(ir::NumSize::Word, 69),
            ))),
            0,
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(420),
                BinOp::Ne,
                OpSrc::Const(69),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_not_num_lt_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::Not(box ir::BoolExpr::NumLt(
                ir::NumExpr::Const(ir::NumSize::Word, 420),
                ir::NumExpr::Const(ir::NumSize::Word, 69),
            ))),
            0,
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(420),
                BinOp::Gte,
                OpSrc::Const(69),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_ptr_eq_bool_expr() {
        match_line_stat_vec(
            ir::Expr::Bool(ir::BoolExpr::PtrEq(ir::PtrExpr::Null, ir::PtrExpr::Null)),
            0,
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(0),
                BinOp::Eq,
                OpSrc::Const(0),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
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
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(1),
                BinOp::And,
                OpSrc::Const(0),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Bool,
            OpSrc::Var(0),
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
            HashMap::new(),
            HashMap::from([("f".to_string(), Some(ir::Type::Bool))]),
            ir::Type::Bool,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_null_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Null),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Const(0),
        )
    }

    #[test]
    fn translate_dataref_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::DataRef(1)),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::DataRef(1, 0),
        )
    }

    #[test]
    fn translate_var_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Var(1)),
            0,
            vec![StatCode::Assign(0, OpSrc::Var(1))],
            HashMap::from([(1, ir::Type::Ptr)]),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Var(0),
        )
    }

    #[test]
    fn translate_deref_ptr_expr() {
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Deref(box ir::PtrExpr::DataRef(0))),
            0,
            vec![StatCode::Load(0, OpSrc::DataRef(0, 0), Size::DWord)],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Var(0),
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
            vec![StatCode::AssignOp(
                0,
                OpSrc::Const(0),
                BinOp::Add,
                OpSrc::Const(2),
            )],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Var(0),
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
                StatCode::Store(OpSrc::Var(2), 1, Size::Byte),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(1)),
                StatCode::Store(OpSrc::Var(2), 1, Size::DWord),
            ],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Var(0),
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
                StatCode::Store(OpSrc::Var(2), 1, Size::Byte),
                StatCode::Assign(1, OpSrc::Const(0)),
                StatCode::AssignOp(2, OpSrc::Var(0), BinOp::Add, OpSrc::Const(4)),
                StatCode::Store(OpSrc::Var(2), 1, Size::DWord),
            ],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Var(0),
        );
        match_line_stat_vec(
            ir::Expr::Ptr(ir::PtrExpr::Malloc(vec![])),
            0,
            vec![],
            HashMap::new(),
            HashMap::new(),
            ir::Type::Ptr,
            OpSrc::Const(0),
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
            HashMap::new(),
            HashMap::from([("f".to_string(), Some(ir::Type::Ptr))]),
            ir::Type::Ptr,
            OpSrc::Var(0),
        )
    }
}
