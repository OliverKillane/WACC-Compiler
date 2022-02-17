use super::Options;
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::collections::HashMap;

/// Id of a statement in a statement graph
type StatId = usize;

/// Type of the source operand for an operation
#[derive(Debug, PartialEq, Eq)]
enum OpType {
    /// Constant value
    Const(u32),
    /// Data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef),
    /// Variable with a given [id](VarRepr)
    Var(VarRepr),
}

/// Size of the source operand
#[derive(Debug, PartialEq, Eq)]
enum Size {
    /// 1 byte
    Byte,
    /// 2 bytes
    Word,
    /// 4 bytes
    DWord,
}

/// Operand with its size and type
#[derive(Debug, PartialEq, Eq)]
struct OpSrc(OpType, Size);

/// Binary operation code
#[derive(Debug, PartialEq, Eq)]
enum BinOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Multiplication (*)
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
#[derive(Debug, PartialEq, Eq)]
enum StatCode {
    /// Assignment of a binary operation to a variable
    Assign(VarRepr, OpSrc, BinOp, OpSrc),
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
#[derive(Debug, PartialEq, Eq)]
struct Stat(Vec<StatId>, StatCode, Vec<(VarRepr, StatId)>, StatId);

/// Graph of statements. The index of a statement signifies the
/// [statement id](StatId) of that statement. The evaluation of the statement
/// graph starts at the first statement.
type StatGraph = Vec<Stat>;

/// Function representation. The first vector are the variables to which the
/// arguments will be assigned to and the [statement graph](StatGraph) is the dataflow graph
/// that is evaluated.
#[derive(Debug, PartialEq, Eq)]
struct Function(Vec<VarRepr>, StatGraph);

/// The entire program in the three-code representation. The first map is a map
/// of all functions defined by the program. The [statement graph](StatGraph) is
/// the main body of the program. The last map is a map of all statically-defined
/// data in the program.
#[derive(Debug, PartialEq, Eq)]
pub(super) struct ThreeCode(
    HashMap<String, Function>,
    StatGraph,
    HashMap<DataRef, Vec<u8>>,
);

fn convert_function(
    function: ir::Function,
    options: &Options,
    data_map: &mut HashMap<ir::DataRef, Vec<ir::Expr>>,
) -> Function {
    todo!()
}

fn convert_block_graph(
    block_graph: ir::BlockGraph,
    types: &HashMap<usize, ir::Type>,
    options: &Options,
    data_map: &mut HashMap<ir::DataRef, Vec<ir::Expr>>,
) -> StatGraph {
    let mut var_repr_counter = types
        .keys()
        .max()
        .map(ToOwned::to_owned)
        .unwrap_or_default()
        + 1;

    todo!()
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        let ir::Program(functions, types, block_graph, static_data) = program;

        let mut data_map = HashMap::new();

        let new_functions: HashMap<String, Function> = functions
            .into_iter()
            .map(|(s, f)| (s, convert_function(f, options, &mut data_map)))
            .collect();

        // let stat_graph: StatGraph = block_graph
        //     .into_iter()
        //     .map(|b| (s, convert_function(f, options, &mut data_map)))
        //     .collect();

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    // use crate::intermediate::*;
    #[test]
    fn from_one_statement() {
        let ir = ir::Program(
            HashMap::from([]),
            HashMap::from([]),
            vec![ir::Block(
                vec![],
                vec![ir::Stat::AssignVar(
                    0,
                    ir::Expr::Bool(ir::BoolExpr::Const(true)),
                )],
                ir::BlockEnding::Return(ir::Expr::Num(ir::NumExpr::Const(ir::NumSize::DWord, 0))),
            )],
            HashMap::from([]),
        );
    }
}
