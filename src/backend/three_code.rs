use super::{Options, PropagationOpt};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::{
    collections::HashMap,
    iter::{self, successors, zip},
};

/// Id of a statement in a statement graph
type StatId = usize;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Type of the source operand for an operation
enum OpSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable with a given [id](VarRepr)
    Var(VarRepr),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Size of the load/store operations
enum Size {
    /// 1 byte
    Byte,
    /// 2 bytes
    Word,
    /// 4 bytes
    DWord,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Binary operation code
enum BinOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Signed multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Mod,

    /// Equality (==)
    Eq,
    /// Not equality (!=)
    Ne,
    /// Greater than (>)
    Gt,
    /// Greater than or equal (>=)
    Gte,
    /// Less than (<)
    Lt,
    /// Less than or equal (<=)
    Lte,

    /// Logical And (&&)
    And,
    /// Logical Or (||)
    Or,
    /// Logical Xor (^)
    Xor,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PtrSrc {
    DataRef(DataRef, i32),
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Statement type
enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc),
    /// Load from a reference to static data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadImm(VarRepr, PtrSrc, Size),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadVar(VarRepr, VarRepr, Size),
    /// Store to a static data reference. The number of bytes stored is signified
    /// by the [size](Size) field.
    StoreImm(PtrSrc, VarRepr, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    StoreVar(VarRepr, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Single statement in a dataflow graph - possible incoming statements,
/// statement type, conditional jumps to other statements (evaluated consecutively)
/// and the else branch if all other statements are equal to 0.
struct Stat(Vec<StatId>, StatCode, Vec<(VarRepr, StatId)>, StatId);

/// Graph of statements. The index of a statement signifies the
/// [statement id](StatId) of that statement. The evaluation of the statement
/// graph starts at the first statement.
type StatGraph = Vec<Stat>;

/// Local variables that have to be represented in memory during program execution.
type LocalVars = HashMap<VarRepr, DataRef>;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Function representation. The first vector are the variables to which the
/// arguments will be assigned to and the [statement graph](StatGraph) is the dataflow graph
/// that is evaluated.
struct Function(Vec<VarRepr>, LocalVars, StatGraph);

#[derive(Debug, PartialEq, Eq, Clone)]
/// The entire program in the three-code representation. The first map is a map
/// of all functions defined by the program. The [statement graph](StatGraph) is
/// the main body of the program. The last map is a map of all statically-defined
/// data in the program.
pub(super) struct ThreeCode(
    HashMap<String, Function>,
    LocalVars,
    StatGraph,
    HashMap<DataRef, Vec<u8>>,
);

impl From<i32> for OpSrc {
    fn from(num: i32) -> Self {
        OpSrc::Const(num)
    }
}

fn translate_function_call(
    name: String,
    args: Vec<ir::Expr>,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
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
            translate_expr(expr, arg_result, stats, vars, functions, options);
            arg_result
        })
        .collect(),
    );
    stats.push(stat_code);
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

fn propagate_num_const(result: VarRepr, stats: &mut Vec<StatCode>, val: Option<i32>) {
    if let Some(val) = val {
        stats.push(StatCode::Assign(result, val.into()));
    }
}

fn clip_num_const(num_const: i32, size: &ir::NumSize) -> i32 {
    match size {
        ir::NumSize::DWord => num_const,
        ir::NumSize::Word => num_const as u16 as i32,
        ir::NumSize::Byte => num_const as u8 as i32,
    }
}

fn translate_num_expr(
    num_expr: ir::NumExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) -> (Option<i32>, ir::NumSize) {
    match num_expr {
        ir::NumExpr::SizeOf(size) => (
            Some(match size {
                ir::Type::Num(ir::NumSize::DWord) => 4,
                ir::Type::Num(ir::NumSize::Word) => 2,
                ir::Type::Num(ir::NumSize::Byte) => 1,
                ir::Type::Ptr => 4,
                ir::Type::Bool => 1,
            }),
            ir::NumSize::DWord,
        ),
        ir::NumExpr::SizeOfWideAlloc => (Some(4), ir::NumSize::DWord),
        ir::NumExpr::Const(size, val) => (Some(val), size),
        ir::NumExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            let size = match vars.get(&var).expect("Variable not found") {
                ir::Type::Num(size) => *size,
                _ => panic!("Variable of a wrong type"),
            };
            (None, size.into())
        }
        ir::NumExpr::Deref(size, ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, vars, functions, options);
            if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                stats.push(StatCode::LoadImm(result, ptr_const, size.into()));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, size.into()));
            }
            (None, size.into())
        }
        ir::NumExpr::ArithOp(box num_expr1, arith_op, box num_expr2) => {
            let mut sub_result = result;
            let (num_const, size1) =
                translate_num_expr(num_expr1, sub_result, stats, vars, functions, options);
            let op_src1 = if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                num_const.into()
            } else {
                propagate_num_const(sub_result, stats, num_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let (num_const, size2) =
                translate_num_expr(num_expr2, sub_result, stats, vars, functions, options);
            let op_src2 = if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                num_const.into()
            } else {
                propagate_num_const(sub_result, stats, num_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::Const(num_const1), OpSrc::Const(num_const2)) = (op_src1, op_src2) {
                (
                    Some(clip_num_const(
                        match arith_op {
                            ir::ArithOp::Add => num_const1 + num_const2,
                            ir::ArithOp::Sub => num_const1 - num_const2,
                            ir::ArithOp::Mul => num_const1 * num_const2,
                            ir::ArithOp::Div => num_const1 / num_const2,
                            ir::ArithOp::Mod => num_const1 % num_const2,
                        },
                        &size1,
                    )),
                    size1,
                )
            } else {
                stats.push(StatCode::AssignOp(
                    result,
                    op_src1,
                    arith_op.into(),
                    op_src2,
                ));
                (None, size1)
            }
        }
        ir::NumExpr::Cast(size, box num_expr) => {
            let (num_const, old_size) =
                translate_num_expr(num_expr, result, stats, vars, functions, options);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                (Some(clip_num_const(num_const, &size)), size.into())
            } else {
                propagate_num_const(result, stats, num_const);
                match (size, old_size) {
                    (ir::NumSize::Byte, ir::NumSize::DWord | ir::NumSize::Word) => {
                        stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::And, 0xFF.into()));
                    }
                    (ir::NumSize::Word, ir::NumSize::DWord) => {
                        stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::And, 0xFFFF.into()));
                    }
                    _ => {}
                }
                (None, size.into())
            }
        }
        ir::NumExpr::Call(name, args) => {
            let size = if let ir::Function(ir::Type::Num(size), _, _, _) =
                functions.get(&name).expect("Function not found")
            {
                *size
            } else {
                panic!("Function has a wrong return type")
            };
            translate_function_call(name, args, result, stats, vars, functions, options);
            (None, size.into())
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

fn propagate_bool_const(result: VarRepr, stats: &mut Vec<StatCode>, bool_const: Option<bool>) {
    if let Some(bool_const) = bool_const {
        stats.push(StatCode::Assign(result, (bool_const as i32).into()));
    }
}

fn translate_bool_expr(
    bool_expr: ir::BoolExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) -> Option<bool> {
    match bool_expr {
        ir::BoolExpr::Const(val) => Some(val),
        ir::BoolExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            None
        }
        ir::BoolExpr::Deref(ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, vars, functions, options);
            if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                stats.push(StatCode::LoadImm(result, ptr_const, Size::Byte));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, Size::Byte));
            }
            stats.push(StatCode::AssignOp(
                result,
                OpSrc::Var(result),
                BinOp::And,
                0x01.into(),
            ));
            None
        }
        ir::BoolExpr::TestZero(num_expr) => {
            let (num_const, _) =
                translate_num_expr(num_expr, result, stats, vars, functions, options);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                Some(num_const == 0)
            } else {
                propagate_num_const(result, stats, num_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Eq, 0x00.into()));
                None
            }
        }
        ir::BoolExpr::TestPositive(num_expr) => {
            let (num_const, _) =
                translate_num_expr(num_expr, result, stats, vars, functions, options);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                Some(num_const == 0)
            } else {
                propagate_num_const(result, stats, num_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Gt, 0x00.into()));
                None
            }
        }
        ir::BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
            let mut sub_result = result;
            let ptr_const =
                translate_ptr_expr(ptr_expr1, sub_result, stats, vars, functions, options);
            let op_src1 = if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let ptr_const =
                translate_ptr_expr(ptr_expr2, sub_result, stats, vars, functions, options);
            let op_src2 = if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                OpSrc::Var(sub_result)
            };
            if let (
                OpSrc::DataRef(_, _) | OpSrc::Const(_),
                OpSrc::DataRef(_, _) | OpSrc::Const(_),
            ) = (op_src1, op_src2)
            {
                Some(op_src1 == op_src2)
            } else {
                stats.push(StatCode::AssignOp(result, op_src1, BinOp::Eq, op_src2));
                None
            }
        }
        ir::BoolExpr::BoolOp(box bool_expr1, bool_op, box bool_expr2) => {
            let mut sub_result = result;
            let bool_const =
                translate_bool_expr(bool_expr1, sub_result, stats, vars, functions, options);
            let op_src1 = if let Some(bool_const) = bool_const && options.propagation != PropagationOpt::None {
                (bool_const as i32).into()
            } else {
                propagate_bool_const(sub_result, stats, bool_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let bool_const =
                translate_bool_expr(bool_expr2, sub_result, stats, vars, functions, options);
            let op_src2 = if let Some(bool_const) = bool_const && options.propagation != PropagationOpt::None {
                (bool_const as i32).into()
            } else {
                propagate_bool_const(sub_result, stats, bool_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::Const(bool_const1), OpSrc::Const(bool_const2)) = (op_src1, op_src2) {
                let bool_const1 = bool_const1 != 0;
                let bool_const2 = bool_const2 != 0;
                Some(match bool_op {
                    ir::BoolOp::And => bool_const1 && bool_const2,
                    ir::BoolOp::Or => bool_const1 || bool_const2,
                    ir::BoolOp::Xor => bool_const1 ^ bool_const2,
                })
            } else {
                stats.push(StatCode::AssignOp(result, op_src1, bool_op.into(), op_src2));
                None
            }
        }
        ir::BoolExpr::Not(box bool_expr) => {
            let bool_const =
                translate_bool_expr(bool_expr, result, stats, vars, functions, options);
            if let Some(bool_const) = bool_const && options.propagation != PropagationOpt::None {
                Some(!bool_const)
            } else {
                propagate_bool_const(result, stats, bool_const);
                stats.push(StatCode::AssignOp(result, OpSrc::Var(result), BinOp::Xor, 0x01.into()));
                None
            }
        }
        ir::BoolExpr::Call(name, args) => {
            translate_function_call(name, args, result, stats, vars, functions, options);
            None
        }
    }
}

impl From<PtrSrc> for OpSrc {
    fn from(ptr_const: PtrSrc) -> Self {
        match ptr_const {
            PtrSrc::DataRef(data_ref, offset) => OpSrc::DataRef(data_ref, offset),
            PtrSrc::Null => OpSrc::Const(0),
        }
    }
}

fn propagate_ptr_const(result: VarRepr, stats: &mut Vec<StatCode>, ptr_const: Option<PtrSrc>) {
    if let Some(ptr_const) = ptr_const {
        stats.push(StatCode::Assign(result, ptr_const.into()));
    }
}

fn translate_ptr_expr(
    ptr_expr: ir::PtrExpr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) -> Option<PtrSrc> {
    match ptr_expr {
        ir::PtrExpr::Null => Some(PtrSrc::Null),
        ir::PtrExpr::DataRef(data_ref) => Some(PtrSrc::DataRef(data_ref, 0)),
        ir::PtrExpr::Var(var) => {
            stats.push(StatCode::Assign(result, OpSrc::Var(var)));
            None
        }
        ir::PtrExpr::Deref(box ptr_expr) => {
            let ptr_const = translate_ptr_expr(ptr_expr, result, stats, vars, functions, options);
            if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                stats.push(StatCode::LoadImm(result, ptr_const, Size::DWord));
            } else {
                propagate_ptr_const(result, stats, ptr_const);
                stats.push(StatCode::LoadVar(result, result, Size::DWord));
            }
            None
        }
        ir::PtrExpr::Offset(box ptr_expr, box num_expr) => {
            let mut sub_result = result;
            let ptr_const =
                translate_ptr_expr(ptr_expr, sub_result, stats, vars, functions, options);
            let ptr_op_src = if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                ptr_const.into()
            } else {
                propagate_ptr_const(sub_result, stats, ptr_const);
                let op_src = OpSrc::Var(sub_result);
                sub_result += 1;
                op_src
            };
            let (num_const, _) =
                translate_num_expr(num_expr, sub_result, stats, vars, functions, options);
            let num_op_src = if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                num_const.into()
            } else {
                propagate_num_const(sub_result, stats, num_const);
                OpSrc::Var(sub_result)
            };
            if let (OpSrc::DataRef(data_ref, offset1), OpSrc::Const(offset2)) =
                (ptr_op_src, num_op_src)
            {
                Some(PtrSrc::DataRef(data_ref, offset1 + offset2))
            } else {
                stats.push(StatCode::AssignOp(
                    result,
                    ptr_op_src,
                    BinOp::Add,
                    num_op_src,
                ));
                None
            }
        }

        _ => todo!(),
    }
}

fn translate_expr(
    expr: ir::Expr,
    result: VarRepr,
    stats: &mut Vec<StatCode>,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) {
    match expr {
        _ => todo!(),
    }
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
