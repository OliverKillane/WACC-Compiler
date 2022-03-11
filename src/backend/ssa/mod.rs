use crate::graph::NodeRef;
use crate::intermediate::DataRef;

#[derive(Debug, PartialEq, Eq, Clone)]
/// Type of the source operand for an operation
pub(super) enum OpSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable declared in the given node
    Var(VarNode),
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

struct PhiStat {}

enum VarStat {
    Val(OpSrc),
    BinOp(OpSrc, BinOp, OpSrc),
    Load(StatNode, Size),
    Call(String, Vec<StatNode>),
}

enum VoidStat {
    Store(StatNode, StatNode, Size),
    Call(String, Vec<StatNode>),
}

enum StatType {
    VarNode(Vec<StatNode>, VarNode, StatNode),
    VoidStat(Vec<StatNode>, VoidStat, StatNode),
    Branch(Vec<StatNode>, VarNode, StatNode, StatNode),
    Phi(Vec<StatNode>, Vec<PhiNode>),
}

type PhiNode = NodeRef<PhiStat>;
type VarNode = NodeRef<VarStat>;
type StatNode = NodeRef<StatType>;
