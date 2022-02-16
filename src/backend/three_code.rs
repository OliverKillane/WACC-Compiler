use super::Options;
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;

type StatId = usize;

enum OpType {
    DataRef(DataRef),
    Const(u32),
    Var(VarRepr),
}

enum OpSize {
    Byte,
    Word,
    DWord,
}

struct OpSrc(OpType, OpSize);

enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,

    And,
    Or,
    Xor,
}

enum UnOp {
    Not,
    Pos,
    Neg,
    Zero,
}

enum StatCode {
    AssignBinOp(VarRepr, OpSrc, BinOp, OpSrc),
    AssignUnOp(VarRepr, UnOp, OpSrc),
    Load(VarRepr, DataRef),
    Store(DataRef, VarRepr),
    Call(String, Vec<VarRepr>),
}

struct Stat(Vec<StatId>, StatCode, Vec<(VarRepr, StatId)>, StatId);

type StatGraph = Vec<Stat>;

struct Function(Vec<VarRepr>, StatGraph);

pub(super) struct ThreeCode(
    HashMap<String, Function>,
    StatGraph,
    HashMap<DataRef, Vec<u8>>,
);

impl From<(&ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (&ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
