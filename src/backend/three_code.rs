use super::Options;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;
use std::mem;

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

#[derive(Debug, PartialEq, Eq, Clone)]
/// Statement type
pub(super) enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable.
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
    /// Returns from the function.
    Return(VarRepr),
}

/// General type of the statement. Used in the dataflow graph.
#[derive(Debug, Clone)]
pub(super) enum StatType {
    /// Used internally for the purposes of creating the graph.
    Dummy(Vec<StatNode>),
    /// Simple data manipulaiton statement that has a superceding statement.
    Simple(Vec<StatNode>, StatCode, StatNode),
    /// Data manipulation statement that is expected to finish the execution one
    /// way or another.
    Final(Vec<StatNode>, StatCode),
    /// A simple branch instruction that checks if the value in the variable is
    /// a boolean 1 or a 0.
    Branch(Vec<StatNode>, VarRepr, StatNode, StatNode),
    /// A self-looping infinite loop, which might occur as a user program.
    Loop(Vec<StatNode>),
}

/// A statement graph node.
type StatNode = NodeRef<StatType>;

#[derive(Debug, Clone)]
/// Function representation.
pub(super) struct Function {
    /// Variables for function argumetns
    pub args: Vec<VarRepr>,
    /// First statement of the program
    pub code: Option<StatNode>,
    /// Reference for usage when calling scanf
    pub read_ref: Option<DataRef>,
}

/// Type of the data reference under a [data reference id](DataRef).
#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum DataRefType {
    Integer(Vec<i32>),
    String(Vec<u8>),
}

#[derive(Debug, Clone)]
/// The entire program in the three-code representation.
pub(super) struct ThreeCode {
    /// All functions in the program
    pub functions: HashMap<String, Function>,
    /// Static data references in the program
    pub data_refs: HashMap<DataRef, DataRefType>,
    /// Graph of all statement nodes in the program
    pub graph: Graph<StatType>,
    /// Reference for usage when calling scanf
    pub read_ref: Option<DataRef>,
    /// First statement of the program
    pub code: Option<StatNode>,
    /// Function to call as an int overflow/underflow handler for checking for
    /// 32-bit overflows.
    pub int_handler: Option<String>,
}

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

    fn new_loop() -> Self {
        StatType::Loop(Vec::new())
    }

    fn add_incoming(&mut self, node: StatNode) {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Final(incoming, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Dummy(incoming) => incoming.push(node),
        }
    }

    fn set_incoming(&mut self, incoming: Vec<StatNode>) {
        match self {
            Self::Simple(old_incoming, _, _)
            | Self::Final(old_incoming, _)
            | Self::Branch(old_incoming, _, _, _)
            | Self::Loop(old_incoming)
            | Self::Dummy(old_incoming) => *old_incoming = incoming,
        }
    }

    fn incoming(&self) -> Vec<StatNode> {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Final(incoming, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Dummy(incoming) => incoming.clone(),
        }
    }

    fn append(&mut self, node: StatNode) {
        let mut tmp_node = Self::deleted();
        mem::swap(self, &mut tmp_node);
        tmp_node = match tmp_node {
            Self::Final(incoming, stat_code) => Self::Simple(incoming, stat_code, node),
            _ => panic!("Node not final"),
        };
        mem::swap(self, &mut tmp_node);
    }
}

impl Deleted for StatType {
    fn deleted() -> Self {
        Self::Dummy(Vec::new())
    }
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
