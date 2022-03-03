use super::trans_expr::add_string;
use crate::intermediate::{self as ir, DataRef};
use std::collections::HashMap;

pub(super) const ARRAY_INDEX_FNAME: &str = "arr_index";
const ARRAY_INDEX_OOB_NEGATIVE: &str = "ArrayIndexOutOfBoundsError: negative index";
const ARRAY_INDEX_OOB_POSITIVE: &str = "ArrayIndexOutOfBoundsError: index too large";
pub(super) const CHECK_NULL_FNAME: &str = "chk_pair";
const CHECK_NULL_ERR: &str = "NullReferenceError: dereference a null reference";
pub(super) const ZERO_CHECK_FNAME: &str = "chk_zero";
const ZERO_CHECK_ERR: &str = "DivideByZeroError: divide or modulo by zero";
pub(super) const OVERFLOW_HANDLER_FNAME: &str = "overflow_handler";
const OVERFLOW_HANDLER_ERR: &str =
    "OverflowError: the result is to small/large to store in a 4-byte signed-integer";

/// Flags for generation of helper functions.
#[derive(Default)]
pub(super) struct HelperFunctionFlags {
    /// A flag for a helper function for safe array indexing. The function takes
    /// the pointer to the array, the index in the array and the width in bytes
    /// of the array fields.
    pub array_indexing: bool,
    /// A flag for a function that checks if a pointer is null. The function
    /// takes a pass-through pointer and returns it if it is not a null pointer.
    pub check_null: bool,
    /// A flag for a function that checks if an integer is zero. The function
    /// takes a pass-through integer and checks if it is equal to zero.
    pub divide_modulo_check: bool,
}

impl HelperFunctionFlags {
    /// Based on the flags, generates the necessary helper functions.
    pub(super) fn generate_functions(
        self,
        functions_map: &mut HashMap<String, ir::Function>,
        data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
    ) {
        if self.array_indexing {
            functions_map
                .try_insert(
                    ARRAY_INDEX_FNAME.to_string(),
                    array_indexing_function(data_ref_map),
                )
                .expect("Function already exists");
        }
        if self.check_null {
            functions_map
                .try_insert(
                    CHECK_NULL_FNAME.to_string(),
                    null_check_function(data_ref_map),
                )
                .expect("Function already exists");
        }
        if self.divide_modulo_check {
            functions_map
                .try_insert(
                    ZERO_CHECK_FNAME.to_string(),
                    divide_modulo_check(data_ref_map),
                )
                .expect("Function already exists");
        }
        functions_map
            .try_insert(
                OVERFLOW_HANDLER_FNAME.to_string(),
                overflow_handler(data_ref_map),
            )
            .expect("Function already exists");
    }
}

/// Generates a print statement based on a string to be printed.
fn print_static_string(
    string: &str,
    data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>,
) -> ir::Stat {
    ir::Stat::PrintStr(
        ir::PtrExpr::Offset(
            box ir::PtrExpr::DataRef(add_string(data_ref_map, string.to_string())),
            box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
        ),
        ir::NumExpr::Const(ir::NumSize::DWord, string.len() as i32),
    )
}

/// Generates an exit block ending with an exit code of 255.
fn err_exit() -> ir::BlockEnding {
    ir::BlockEnding::Exit(ir::NumExpr::Const(ir::NumSize::DWord, 255))
}

/// Generates an array indexing helper function.
fn array_indexing_function(data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>) -> ir::Function {
    ir::Function(
        ir::Type::Ptr,
        vec![
            (ir::Type::Ptr, 0),
            (ir::Type::Num(ir::NumSize::DWord), 1),
            (ir::Type::Num(ir::NumSize::DWord), 2),
        ],
        HashMap::new(),
        vec![
            ir::Block(
                vec![],
                vec![],
                ir::BlockEnding::CondJumps(
                    vec![
                        (
                            ir::BoolExpr::TestPositive(ir::NumExpr::ArithOp(
                                box ir::NumExpr::Const(ir::NumSize::DWord, 0),
                                ir::ArithOp::Sub,
                                box ir::NumExpr::Var(1),
                            )),
                            1,
                        ),
                        (
                            ir::BoolExpr::Not(box ir::BoolExpr::TestPositive(
                                ir::NumExpr::ArithOp(
                                    box ir::NumExpr::Deref(ir::NumSize::DWord, ir::PtrExpr::Var(0)),
                                    ir::ArithOp::Sub,
                                    box ir::NumExpr::Var(1),
                                ),
                            )),
                            2,
                        ),
                    ],
                    3,
                ),
            ),
            ir::Block(
                vec![0],
                vec![print_static_string(ARRAY_INDEX_OOB_NEGATIVE, data_ref_map)],
                err_exit(),
            ),
            ir::Block(
                vec![0],
                vec![print_static_string(ARRAY_INDEX_OOB_POSITIVE, data_ref_map)],
                err_exit(),
            ),
            ir::Block(
                vec![0],
                vec![],
                ir::BlockEnding::Return(ir::Expr::Ptr(ir::PtrExpr::Offset(
                    box ir::PtrExpr::Var(0),
                    box ir::NumExpr::ArithOp(
                        box ir::NumExpr::ArithOp(
                            box ir::NumExpr::Var(1),
                            ir::ArithOp::Mul,
                            box ir::NumExpr::Var(2),
                        ),
                        ir::ArithOp::Add,
                        box ir::NumExpr::SizeOf(ir::Type::Num(ir::NumSize::DWord)),
                    ),
                ))),
            ),
        ],
    )
}

/// Generates a null pointer checking helper function.
fn null_check_function(data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>) -> ir::Function {
    ir::Function(
        ir::Type::Ptr,
        vec![(ir::Type::Ptr, 0)],
        HashMap::new(),
        vec![
            ir::Block(
                vec![],
                vec![],
                ir::BlockEnding::CondJumps(
                    vec![(
                        ir::BoolExpr::PtrEq(ir::PtrExpr::Var(0), ir::PtrExpr::Null),
                        1,
                    )],
                    2,
                ),
            ),
            ir::Block(
                vec![0],
                vec![print_static_string(CHECK_NULL_ERR, data_ref_map)],
                err_exit(),
            ),
            ir::Block(
                vec![0],
                vec![],
                ir::BlockEnding::Return(ir::Expr::Ptr(ir::PtrExpr::Var(0))),
            ),
        ],
    )
}

/// Generates a division by zero checking helper function.
fn divide_modulo_check(data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>) -> ir::Function {
    ir::Function(
        ir::Type::Num(ir::NumSize::DWord),
        vec![(ir::Type::Num(ir::NumSize::DWord), 0)],
        HashMap::new(),
        vec![
            ir::Block(
                vec![],
                vec![],
                ir::BlockEnding::CondJumps(
                    vec![(ir::BoolExpr::TestZero(ir::NumExpr::Var(0)), 1)],
                    2,
                ),
            ),
            ir::Block(
                vec![0],
                vec![print_static_string(ZERO_CHECK_ERR, data_ref_map)],
                err_exit(),
            ),
            ir::Block(
                vec![0],
                vec![],
                ir::BlockEnding::Return(ir::Expr::Num(ir::NumExpr::Var(0))),
            ),
        ],
    )
}

/// Generates an integer overflow handler.
fn overflow_handler(data_ref_map: &mut HashMap<DataRef, Vec<ir::Expr>>) -> ir::Function {
    ir::Function(
        ir::Type::Num(ir::NumSize::DWord),
        vec![],
        HashMap::new(),
        vec![ir::Block(
            vec![],
            vec![print_static_string(OVERFLOW_HANDLER_ERR, data_ref_map)],
            err_exit(),
        )],
    )
}
