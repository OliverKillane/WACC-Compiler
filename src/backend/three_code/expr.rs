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

            let mut setting_stat_line = StatLine::new(stat_line.graph());
            let width: i32 = zip(
                exprs,
                successors(Some(result + 1), |sub_result| Some(sub_result + 1)),
            )
            .into_iter()
            .map(|(expr, sub_result)| {
                if is_wide {
                    4
                } else {
                    get_type_width(translate_expr(
                        expr,
                        sub_result,
                        &mut setting_stat_line,
                        vars,
                        function_types,
                        options,
                    ))
                    .into()
                }
            })
            .sum();

            if let Some(setting_stat_line_start) = setting_stat_line.start_node() {
                stat_line.add_stat(StatCode::Assign(result, OpSrc::from(width)));
                stat_line.add_stat(StatCode::Call(result, "malloc".to_string(), vec![result]));
                stat_line.splice(
                    setting_stat_line_start,
                    setting_stat_line.end_node().unwrap(),
                );
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
