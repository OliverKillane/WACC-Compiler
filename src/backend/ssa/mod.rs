mod trans_from;
mod trans_into;

use super::three_code as tc;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{DataRef, VarRepr};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Type of the source operand for an operation
pub(super) enum ValSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable declared in the given node
    Var(VarNode),
    /// Function argument
    Arg(VarRepr),
    /// From the phi operator
    Phi(PhiNode),
    /// A reference to a special field on the stack just for reading into variables
    ReadRef,
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
pub(super) struct PhiStat(pub(super) Vec<VarNode>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum VarStat {
    Val(ValSrc),
    BinOp(ValSrc, BinOp, ValSrc),
    Load(VarNode, Size),
    Call(String, Vec<VarNode>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum VoidStat {
    Store(VarNode, VarNode, Size),
    Call(String, Vec<VarNode>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum StatType {
    VarStat(VarNode, StatNodeRef),
    VoidStat(VoidStat, StatNodeRef),
    Branch(VarNode, StatNodeRef, StatNodeRef),
    Phi(Vec<PhiNode>),
    Return(Option<VarNode>),
    Loop,
    Dummy,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) struct StatNode(Vec<StatNode>, StatType);

pub(super) type PhiNode = Rc<RefCell<PhiStat>>;
pub(super) type VarNode = Rc<RefCell<VarStat>>;
pub(super) type StatNodeRef = NodeRef<StatNode>;

pub(super) struct Function {
    pub args: Vec<VarRepr>,
    pub code: StatNodeRef,
    pub read_ref: bool,
}

pub(super) struct SSA {
    /// All functions in the program
    pub functions: HashMap<String, Function>,
    /// Static data references in the program
    pub data_refs: HashMap<DataRef, tc::DataRefType>,
    /// Graph of all statement nodes in the program
    pub graph: Graph<StatNode>,
    /// Whether a [read ref operand source](OpSrc::ReadRef) is used in the main program code
    pub read_ref: bool,
    /// First statement of the program
    pub code: StatNodeRef,
    /// Function to call as an int overflow/underflow handler for checking for
    /// 32-bit overflows.
    pub int_handler: Option<String>,
}

impl Deleted for StatNode {
    fn deleted() -> Self {
        StatNode(vec![], StatType::Dummy)
    }
}
