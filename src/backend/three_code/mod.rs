mod expr;
mod stat;

use super::Options;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Type of the source operand for an operation
pub(super) enum OpSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable with a given [id](VarRepr)
    Var(VarRepr),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Size of the load/store operations
pub(super) enum Size {
    /// 1 byte
    Byte,
    /// 2 bytes
    Word,
    /// 4 bytes
    DWord,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Binary operation code
pub(super) enum BinOp {
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
pub(super) enum PtrSrc {
    DataRef(DataRef, i32),
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Statement type
pub(super) enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    Load(VarRepr, VarRepr, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    Store(VarRepr, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
    /// A call to a function that ommits the return value. If the function name
    /// is not in the list of the [program](ThreeCode) functions then it is assumed
    /// to be external and linked to by the linker.
    VoidCall(String, Vec<VarRepr>),
}

/// General type of the statement. Used in the dataflow graph.
#[derive(Debug, Clone)]

pub(super) enum StatType {
    /// Used internally for the purposes of the graph.
    Deleted,
    /// Simple data manipulaiton statement that has a superceding statement.
    Simple(Vec<StatNode>, StatCode, StatNode),
    /// Data manipulation statement that is expected to finish the execution one
    /// way or another.
    Final(Vec<StatNode>, StatCode),
    /// A simple branch instruction that checks if the value in the variable is
    /// a boolean 1 or a 0.
    Branch(Vec<StatNode>, VarRepr, StatNode, StatNode),
}

/// A [statement graph](StatGraph) node.
type StatNode = NodeRef<StatType>;

/// Graph of statements. The evaluation starts at the start node, unless the graph is empty
/// in which case the optional is set to None. Contains
/// [data manipulation and conditional statements](StatType).
#[derive(Debug, Clone)]
pub(super) struct StatGraph {
    start: Option<StatNode>,
    graph: Graph<StatType>,
}

/// Local variables that have to be represented in memory during program execution.
pub(super) type LocalVars = HashMap<VarRepr, DataRef>;

#[derive(Debug, Clone)]
/// Function representation. The first vector are the variables to which the
/// arguments will be assigned to and the [statement graph](StatGraph) is the dataflow graph
/// that is evaluated.
pub(super) struct Function(pub Vec<VarRepr>, pub LocalVars, pub StatGraph);

#[derive(Debug, Clone)]
/// The entire program in the three-code representation. The first map is a map
/// of all functions defined by the program. The [statement graph](StatGraph) is
/// the main body of the program. The last map is a map of all statically-defined
/// data in the program.
pub(super) struct ThreeCode(
    pub HashMap<String, Function>,
    pub LocalVars,
    pub StatGraph,
    pub HashMap<DataRef, Vec<u8>>,
);

impl StatType {
    fn new_final(stat_code: StatCode) -> Self {
        StatType::Final(Vec::new(), stat_code)
    }

    fn new_simple(stat_code: StatCode, next: StatNode) -> Self {
        StatType::Simple(Vec::new(), stat_code, next)
    }

    fn new_branch(cond: VarRepr, if_true: StatNode, if_false: StatNode) -> Self {
        StatType::Branch(Vec::new(), cond, if_true, if_false)
    }

    fn add_incoming(&mut self, node: StatNode) {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Final(incoming, _)
            | Self::Branch(incoming, _, _, _) => incoming.push(node),
            Self::Deleted => panic!("Node has been deleted"),
        }
    }
}

impl Deleted for StatType {
    fn deleted() -> Self {
        StatType::Deleted
    }
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
