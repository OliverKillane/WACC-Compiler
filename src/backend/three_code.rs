use super::Options;
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;

/// Id of a statement in a statement graph
type StatId = usize;

/// Type of the source operand for an operation
enum OpSrc {
    /// Constant value
    Const(u32),
    /// Variable with a given [id](VarRepr)
    Var(VarRepr),
}

/// Size of the load/store operations
enum Size {
    /// 1 byte
    Byte,
    /// 2 bytes
    Word,
    /// 4 bytes
    DWord,
}

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

/// Statement type
enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc),
    /// Load from a reference to static data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadImm(VarRepr, DataRef, Size),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadVar(VarRepr, VarRepr, Size),
    /// Store to a static data reference. The number of bytes stored is signified
    /// by the [size](Size) field.
    StoreImm(DataRef, VarRepr, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    StoreVar(VarRepr, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
}

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

/// Function representation. The first vector are the variables to which the
/// arguments will be assigned to and the [statement graph](StatGraph) is the dataflow graph
/// that is evaluated.
struct Function(Vec<VarRepr>, LocalVars, StatGraph);

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

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
