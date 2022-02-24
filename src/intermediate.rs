//! An intermediate representation between the frontend and the backend.
//!
//! # Backend behavior guarantees
//!
//! ## Statement Evaluation
//!
//! The statements are guaranteed to produce the same behavior as if evaluated
//! in the order they are given in.
//!
//! ## Expression Evaluation
//!
//! For evaluation of expression trees (consisting of [generic expressions](Expr),
//! [numeric expressions](NumExpr), [pointer expressions](PtrExpr) and
//! [boolean expressions](BoolExpr)):
//!  - If the sub-expression is a part of an arithmetic operation on 2 numeric
//!    expressions or a boolean operation on 2 boolean expressions,
//!    the only guarantee is that the result of the arithmetic operation will be
//!    the same as if executed without short-circuiting. This means that, if
//!    a function call is a sub-expression in an arithmetic or a boolean operation
//!    then there is no guarantee if it will be actually executed, only that the
//!    result of the operation will be preserved.
//!  - In all other cases all direct sub-expressions are executed.

use std::{
    collections::{HashMap, HashSet, LinkedList},
    iter::zip,
};

/// The representation of an identifier of a variable.
pub type VarRepr = usize;

/// The id of a block in a [block graph](BlockGraph).
pub type BlockId = usize;

/// A reference to a piece of data in the data section.
pub type DataRef = u64;

/// A common container for all types of expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    /// A [number expression](NumExpr)
    Num(NumExpr),
    /// A [boolean expression](BoolExpr)
    Bool(BoolExpr),
    /// A [pointer expression](PtrExpr)
    Ptr(PtrExpr),
}

/// Types of arithmetic operations on [numeric expressions](NumExpr).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ArithOp {
    /// Addition (+).
    Add,
    /// Subtraction (-).
    Sub,
    /// Signed multiplication (*).
    Mul,
    /// Signed division (/).
    Div,
    /// Signed modulo (%).
    Mod,
}

/// Size of the [numeric expression](NumExpr).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NumSize {
    /// Signifies a signed 4-byte expression, for example an int
    DWord,
    /// Signifies a unsigned 2-byte expression
    Word,
    /// Signifies a unsigned 1-byte expression, for example a char
    Byte,
}

/// A numeric expression. Can represent for example character or integer expressions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum NumExpr {
    /// Size of the given type in the memory. The type of this expression is
    /// always a DWord.
    SizeOf(Type),
    /// Size of a wide allocation field.
    SizeOfWideAlloc,
    /// A number constant. The value must fit within the given expression size.
    Const(NumSize, i32),
    /// A reference to a variable. The variable must have a numeric type in
    /// the symbol table attached to each [function](Function) or to [program](Program).
    /// The size of the expression is determined based on the entry in the symbol table.
    Var(VarRepr),
    /// Dereference of a number under a given pointer.
    Deref(NumSize, PtrExpr),

    /// An arithmetic operation expression. The sizes of the two sub-expressions
    /// must be the same. The size of the expression is equal to the size of the
    /// two sub-expressions.
    ArithOp(Box<NumExpr>, ArithOp, Box<NumExpr>),
    /// A cast from one expression size to another.
    Cast(NumSize, Box<NumExpr>),

    /// A call to a [function](Function). The function must have a numeric output type.
    /// The size of the expression is determined based on the type of the function.
    /// The number and types of the argument expressions must match the ones of
    /// the function parameters.
    Call(String, Vec<Expr>),
}

/// Types of boolean operations on [boolean expressions](BoolExpr).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BoolOp {
    /// Conjunction (&&).
    And,
    /// Disjunction (||).
    Or,
    /// Exclusive or (^).
    Xor,
}

/// A boolean expression. Can have a value of either true or false.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BoolExpr {
    /// A boolean constant. Can be either true or false.
    Const(bool),
    /// A reference to a variable. The variable must have a boolean type in
    /// the symbol table attached to each [function](Function) or to [program](Program).
    Var(VarRepr),
    /// Dereference of a boolean under a given pointer.
    Deref(PtrExpr),

    /// Tests whether the numeric expression evaluates to zero.
    TestZero(NumExpr),
    /// Tests whether the numeric expression evaluates to a positive signed value.
    TestPositive(NumExpr),
    /// Check if two pointers point to the same address.
    PtrEq(PtrExpr, PtrExpr),

    /// A boolean operation expression.
    BoolOp(Box<BoolExpr>, BoolOp, Box<BoolExpr>),
    /// A boolean negation expression.
    Not(Box<BoolExpr>),
    /// A call to a [function](Function). The function must have a boolean output type.
    /// The number and types of the argument expressions must match the ones of
    /// the function parameters.
    Call(String, Vec<Expr>),
}

/// A pointer manipulation expression.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PtrExpr {
    /// A null-pointer expression. Dereferencing it will cause a segmentation fault.
    Null,
    /// A reference to a piece of data in the data section. Can be used to for
    /// example store strings. The data reference must exist in the main map
    /// of the data section structs.
    DataRef(DataRef),
    /// A reference to a variable. The variable must have a pointer type in
    /// the symbol table attached to each [function](Function) or to [program](Program).
    Var(VarRepr),
    /// Dereference of a pointer under a given pointer.
    Deref(Box<PtrExpr>),
    /// An offset of a pointer. Can be used for example to index an array or
    /// get an element out of a pair.
    Offset(Box<PtrExpr>, Box<NumExpr>),
    /// Allocates a container on a heap with given items inside it. An array
    /// for example can be represented as the first element being a dword with
    /// the size of the array, and the rest of the expressions being the items
    /// in that array. The width of the allocation is the sum of the sizes
    /// of all of the expressions
    Malloc(Vec<Expr>),
    /// Allocates a container on a heap with given items inside it. A pair for
    /// example can be represented as an allocation of exactly 2 expressions.
    /// The width of the allocation is the number of the fields in the
    /// allocation times the size of a single wide malloc field.
    WideMalloc(Vec<Expr>),
    /// A call to a [function](Function). The function must have a pointer output type.
    /// The number and types of the argument expressions must match the ones of
    /// the function parameters.
    Call(String, Vec<Expr>),
}

/// An execution statement.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stat {
    /// Assigns the value of an expression to a variable. The variable must have
    /// a matching type in the symbol table attached to each
    /// [function](Function) or to [program](Program).
    AssignVar(VarRepr, Expr),
    /// Assigns the value of an expression to an address to which the pointer
    /// expression points to.
    AssignPtr(PtrExpr, Expr),

    /// Reads an integer into a variable. The variable must have a numeric dword type.
    ReadIntVar(VarRepr),
    /// Reads an ascii value of a character into a variable. The variable must
    /// have a numeric byte type.
    ReadCharVar(VarRepr),
    /// Reads an integer into an address under the pointer. The pointer is assumed
    /// to point to a dword.
    ReadIntPtr(PtrExpr),
    /// Reads an ascii value of a character into an address under the pointer.
    /// The pointer is assumed to point to a byte.
    ReadCharPtr(PtrExpr),

    /// Frees a malloced structure with a size given by the number expression.
    Free(PtrExpr, NumExpr),

    /// Prints a raw value of an expression according to that expression's string format.
    PrintExpr(Expr),
    /// Prints the value of a number expression as a character. The expression must have
    /// a byte size.
    PrintChar(NumExpr),
    /// Prints the value of a consecutive sequence of characters to which the pointer
    /// points to. The number of characters is given by the numeric expression.
    PrintStr(PtrExpr, NumExpr),
    /// Prints an end-of-line character/sequence of characters, according to the platform
    /// specifications.
    PrintEol(),
}

/// An action to be performed after a block of statements is executed.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BlockEnding {
    /// Represents a conditional jump. The boolean expressions in the vector are
    /// evaluated consecutively. If a boolean expression is true, then the
    /// execution moves tho the block with a given [block id](BlockId). If all checks
    /// fail, then the execution jumps to the other block id.
    CondJumps(Vec<(BoolExpr, BlockId)>, BlockId),
    /// Represents an exit statement. Causes the program to exit the execution with
    /// an exit code given in the numeric expression. The expression must have a byte size.
    Exit(NumExpr),
    /// Returns a value of an expression. Cannot be used in a block in the
    /// main program. If used in a function, must have the type and size of the expression
    /// matching the output type of the function.
    Return(Expr),
}

/// A block of statements. Contains the blocks that have conditional jumps to it.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block(pub Vec<BlockId>, pub Vec<Stat>, pub BlockEnding);
/// A graph of blocks. The index of the block in the block graph signifies the
/// [block id](BlockId) of that block. The block graph must have at least one
/// block in it.
pub type BlockGraph = Vec<Block>;

/// Type of an expression.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    /// A numeric expression type, with the given expression size.
    Num(NumSize),
    /// A boolean expression type.
    Bool,
    /// A pointer expression type.
    Ptr,
}

/// A function. Contains the return type of the function, the types and identifiers
/// for the arguments, the table of types for local variables used and the
/// [block graph](BlockGraph) for its body. The execution starts off from the
/// first block in the block graph.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function(
    pub Type,
    pub Vec<(Type, VarRepr)>,
    pub HashMap<VarRepr, Type>,
    pub BlockGraph,
);

/// An entire program. Contains the definitions of functions, the table of types
/// for local variables used, the [block graph](BlockGraph) for its body, the
/// map of structs in the data section for the whole program and the optional function
/// name to be called in case of an integer overflow or underlow(this function should
/// take no arguments). The execution starts off from the first block in the block
/// graph. The expressions in the data section structs must be constant expressions,
/// i.e. they must be capable of static evaluation.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program(
    pub HashMap<String, Function>,
    pub HashMap<VarRepr, Type>,
    pub BlockGraph,
    pub HashMap<DataRef, Vec<Expr>>,
    pub Option<String>,
);

/// Returns an Ok or an Err based on the value of the boolean
fn cond_result(b: bool) -> Result<(), ()> {
    if b {
        Ok(())
    } else {
        Err(())
    }
}

impl BoolExpr {
    /// Validates a boolean expression: variable existence and type, function
    /// call type and validity of sub-expressions.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
        const_eval: bool,
    ) -> Result<(), ()> {
        match self {
            BoolExpr::Const(_) => Ok(()),
            BoolExpr::Var(var) => {
                cond_result(Type::Bool == *vars.get(var).ok_or(())? && !const_eval)
            }
            BoolExpr::Deref(ptr_expr) => {
                ptr_expr.validate(functions, vars, data_refs, const_eval)?;
                cond_result(!const_eval)
            }
            BoolExpr::TestZero(num_expr) | BoolExpr::TestPositive(num_expr) => num_expr
                .validate(functions, vars, data_refs, const_eval)
                .map(|_| ()),
            BoolExpr::PtrEq(ptr_expr1, ptr_expr2) => {
                ptr_expr1.validate(functions, vars, data_refs, const_eval)?;
                ptr_expr2.validate(functions, vars, data_refs, const_eval)
            }
            BoolExpr::BoolOp(box bool_expr1, _, box bool_expr2) => {
                bool_expr1.validate(functions, vars, data_refs, const_eval)?;
                bool_expr2.validate(functions, vars, data_refs, const_eval)
            }
            BoolExpr::Not(box bool_expr) => {
                bool_expr.validate(functions, vars, data_refs, const_eval)
            }
            BoolExpr::Call(name, exprs) => {
                let function = functions.get(name).ok_or(())?;
                cond_result(
                    Type::Bool == function.validate_call(exprs, functions, vars, data_refs)?
                        && !const_eval,
                )
            }
        }
    }
}

impl PtrExpr {
    /// Validates a pointer expression: data reference existence, variable existence
    /// and type, pointer offset size, function call type and validity of sub-expressions.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
        const_eval: bool,
    ) -> Result<(), ()> {
        match self {
            PtrExpr::Null => Ok(()),
            PtrExpr::DataRef(data_ref) => {
                data_refs.get(data_ref).ok_or(())?;
                cond_result(!const_eval)
            }
            PtrExpr::Var(var) => cond_result(Type::Ptr == *vars.get(var).ok_or(())? && !const_eval),
            PtrExpr::Deref(box ptr_expr) => {
                ptr_expr.validate(functions, vars, data_refs, const_eval)?;
                cond_result(!const_eval)
            }
            PtrExpr::Offset(box ptr_expr, box num_expr) => {
                if let NumSize::DWord = num_expr.validate(functions, vars, data_refs, const_eval)? {
                    ptr_expr.validate(functions, vars, data_refs, const_eval)
                } else {
                    Err(())
                }
            }
            PtrExpr::Malloc(exprs) | PtrExpr::WideMalloc(exprs) => {
                for expr in exprs {
                    expr.validate(functions, vars, data_refs, const_eval)?;
                }
                cond_result(!const_eval)
            }
            PtrExpr::Call(name, exprs) => {
                let function = functions.get(name).ok_or(())?;
                cond_result(
                    Type::Ptr == function.validate_call(exprs, functions, vars, data_refs)?
                        && !const_eval,
                )
            }
        }
    }
}

impl NumExpr {
    /// Checks the size of a numeric expression and validates it: bounds on a
    /// constant, variable existence and type, equal-sizeness of arithmetic
    /// operation arms, function call type and validity of sub-expressions.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
        const_eval: bool,
    ) -> Result<NumSize, ()> {
        Ok(match self {
            NumExpr::SizeOf(_) | NumExpr::SizeOfWideAlloc => NumSize::DWord,
            NumExpr::Const(size, val) => {
                if *val
                    == match size {
                        NumSize::DWord => *val,
                        NumSize::Word => *val as i16 as i32,
                        NumSize::Byte => *val as i8 as i32,
                    }
                {
                    *size
                } else {
                    return Err(());
                }
            }
            NumExpr::Var(var) => {
                if let Type::Num(size) = vars.get(var).ok_or(())? && !const_eval {
                    *size
                } else {
                    return Err(());
                }
            }
            NumExpr::Deref(size, ptr_expr) => {
                ptr_expr.validate(functions, vars, data_refs, const_eval)?;
                if const_eval {
                    return Err(());
                }
                *size
            }
            NumExpr::ArithOp(box num_expr1, _, box num_expr2) => {
                let size1 = num_expr1.validate(functions, vars, data_refs, const_eval)?;
                let size2 = num_expr2.validate(functions, vars, data_refs, const_eval)?;
                if size1 != size2 {
                    return Err(());
                }
                size1
            }
            NumExpr::Cast(size, box num_expr) => {
                num_expr.validate(functions, vars, data_refs, const_eval)?;
                *size
            },
            NumExpr::Call(name, exprs) => {
                let function = functions.get(name).ok_or(())?;
                if let Type::Num(size) =
                    function.validate_call(exprs, functions, vars, data_refs)? && !const_eval
                {
                    size
                } else {
                    return Err(());
                }
            }
        })
    }
}

impl Expr {
    /// Checks the type of an expression and validates the underlying typed expression.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
        const_eval: bool,
    ) -> Result<Type, ()> {
        Ok(match self {
            Expr::Num(num_expr) => {
                Type::Num(num_expr.validate(functions, vars, data_refs, const_eval)?)
            }
            Expr::Bool(bool_expr) => {
                bool_expr.validate(functions, vars, data_refs, const_eval)?;
                Type::Bool
            }
            Expr::Ptr(ptr_expr) => {
                ptr_expr.validate(functions, vars, data_refs, const_eval)?;
                Type::Ptr
            }
        })
    }
}

impl Stat {
    /// Validates a statement: variable assignment sizeness, sizeness of reads,
    /// print char sizeness and the validity of sub-expressions.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
    ) -> Result<(), ()> {
        match self {
            Stat::AssignVar(var, expr) => cond_result(
                *vars.get(var).ok_or(())? == expr.validate(functions, vars, data_refs, false)?,
            ),
            Stat::AssignPtr(ptr_expr, expr) => {
                ptr_expr.validate(functions, vars, data_refs, false)?;
                expr.validate(functions, vars, data_refs, false).map(|_| ())
            }
            Stat::ReadIntVar(var) => {
                cond_result(Type::Num(NumSize::DWord) == *vars.get(var).ok_or(())?)
            }
            Stat::ReadCharVar(var) => {
                cond_result(Type::Num(NumSize::Byte) == *vars.get(var).ok_or(())?)
            }
            Stat::ReadIntPtr(ptr_expr) | Stat::ReadCharPtr(ptr_expr) => {
                ptr_expr.validate(functions, vars, data_refs, false)
            }
            Stat::Free(ptr_expr, num_expr) | Stat::PrintStr(ptr_expr, num_expr) => {
                num_expr.validate(functions, vars, data_refs, false)?;
                ptr_expr.validate(functions, vars, data_refs, false)
            }
            Stat::PrintExpr(expr) => expr.validate(functions, vars, data_refs, false).map(|_| ()),
            Stat::PrintChar(num_expr) => {
                cond_result(NumSize::Byte == num_expr.validate(functions, vars, data_refs, false)?)
            }
            Stat::PrintEol() => Ok(()),
        }
    }
}

/// Checks the return type of a block graph and validates it:
/// non-emptyness, equal return types, exit expression type and size and validity
/// of sub-expressions.
fn validate_block_graph(
    graph: &BlockGraph,
    functions: &HashMap<String, Function>,
    vars: &HashMap<VarRepr, Type>,
    data_refs: &HashMap<DataRef, Vec<Expr>>,
) -> Result<Option<Type>, ()> {
    let size = graph.len();
    if size == 0 {
        return Err(());
    }

    let mut incoming = HashMap::<BlockId, LinkedList<BlockId>>::new();
    let mut return_type = None;
    for (block_idx, Block(_, stats, ending)) in graph.iter().enumerate() {
        for stat in stats {
            stat.validate(functions, vars, data_refs)?;
        }
        match ending {
            BlockEnding::CondJumps(cond_jumps, jump) => {
                for (bool_expr, jump) in cond_jumps {
                    bool_expr.validate(functions, vars, data_refs, false)?;
                    if *jump >= size {
                        return Err(());
                    } else if let Some(incoming_list) = incoming.get_mut(jump) {
                        incoming_list.push_back(block_idx);
                    } else {
                        let mut incoming_list = LinkedList::new();
                        incoming_list.push_back(block_idx);
                        incoming.insert(*jump, incoming_list);
                    }
                }
                if *jump >= size {
                    return Err(());
                } else if let Some(incoming_list) = incoming.get_mut(jump) {
                    incoming_list.push_back(block_idx);
                } else {
                    let mut incoming_list = LinkedList::new();
                    incoming_list.push_back(block_idx);
                    incoming.insert(*jump, incoming_list);
                }
            }
            BlockEnding::Exit(num_expr) => {
                if NumSize::DWord != num_expr.validate(functions, vars, data_refs, false)? {
                    return Err(());
                }
            }
            BlockEnding::Return(expr) => {
                let expr_type = expr.validate(functions, vars, data_refs, false)?;
                if return_type.is_none() {
                    return_type = Some(expr_type);
                } else if return_type != Some(expr_type) {
                    return Err(());
                }
            }
        }
    }
    for (block_idx, Block(incoming_vec, _, _)) in graph.iter().enumerate() {
        if incoming_vec.iter().collect::<HashSet<_>>()
            != incoming
                .get(&block_idx)
                .unwrap_or(&LinkedList::new())
                .iter()
                .collect::<HashSet<_>>()
        {
            return Err(());
        }
    }
    Ok(return_type)
}

impl Function {
    /// Validates a function definition: non-collision between local variables and arguments
    /// and the validity of the underlying block graph.
    fn validate(
        &self,
        functions: &HashMap<String, Function>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
    ) -> Result<(), ()> {
        let Function(ret_type, args, vars, graph) = self;
        let mut vars = vars.clone();
        for &(arg_type, var) in args {
            if vars.insert(var, arg_type).is_some() {
                return Err(());
            }
        }
        cond_result(
            validate_block_graph(graph, functions, &vars, data_refs)?
                .map_or(true, |block_type| block_type == *ret_type),
        )
    }

    /// Validates a function call: validity of argument expressions and type and
    /// length matching on arguments.
    fn validate_call(
        &self,
        arg_values: &[Expr],
        functions: &HashMap<String, Function>,
        vars: &HashMap<VarRepr, Type>,
        data_refs: &HashMap<DataRef, Vec<Expr>>,
    ) -> Result<Type, ()> {
        let Function(ret_type, args, _, _) = self;
        if arg_values.len() != args.len() {
            return Err(());
        }
        for ((arg_type, _), arg_value) in zip(args, arg_values) {
            if *arg_type != arg_value.validate(functions, vars, data_refs, false)? {
                return Err(());
            }
        }
        Ok(*ret_type)
    }
}

impl Program {
    /// Validates a program: validity of functions, main program body, no returns in the main function
    /// and the validity of static data references (valid expressions + expressions are constant).
    pub fn validate(&self) -> Result<(), ()> {
        let Program(functions, vars, graph, data_refs, int_handler) = self;
        for exprs in data_refs.values() {
            for expr in exprs {
                expr.validate(&HashMap::new(), &HashMap::new(), &HashMap::new(), true)?;
            }
        }
        for function in functions.values() {
            function.validate(functions, data_refs)?;
        }
        if let Some(int_handler) = int_handler {
            if let Function(_, args, _, _) = functions.get(int_handler).ok_or(())? && !args.is_empty() {
                return Err(());
            }
        }
        cond_result(None == validate_block_graph(graph, functions, vars, data_refs)?)
    }
}

#[cfg(test)]
mod test {
    use super::{
        ArithOp, Block, BlockEnding, BoolExpr, DataRef, Expr, Function, NumExpr, NumSize, Program,
        PtrExpr, Stat, Type, VarRepr,
    };
    use std::collections::HashMap;

    fn validate_num_expr(
        num_expr: NumExpr,
        vars: HashMap<VarRepr, Type>,
        data_refs: HashMap<DataRef, Vec<Expr>>,
    ) -> Result<(), ()> {
        validate_expr(Expr::Num(num_expr), vars, data_refs)
    }

    fn validate_expr(
        expr: Expr,
        vars: HashMap<VarRepr, Type>,
        data_refs: HashMap<DataRef, Vec<Expr>>,
    ) -> Result<(), ()> {
        Program(
            HashMap::new(),
            vars,
            vec![Block(
                vec![],
                vec![Stat::PrintExpr(expr)],
                BlockEnding::Exit(NumExpr::Const(NumSize::DWord, 0)),
            )],
            data_refs,
            None,
        )
        .validate()
    }

    fn validate_call(
        expr: Expr,
        fname: String,
        args: Vec<(Type, VarRepr)>,
        ret_type: Type,
    ) -> Result<(), ()> {
        Program(
            HashMap::from([(
                fname,
                Function(
                    ret_type,
                    args,
                    HashMap::from([(0, ret_type)]),
                    vec![Block(
                        vec![],
                        vec![Stat::PrintEol()],
                        BlockEnding::Return(match ret_type {
                            Type::Ptr => Expr::Ptr(PtrExpr::Var(0)),
                            Type::Bool => Expr::Bool(BoolExpr::Var(0)),
                            Type::Num(_) => Expr::Num(NumExpr::Var(0)),
                        }),
                    )],
                ),
            )]),
            HashMap::new(),
            vec![Block(
                vec![],
                vec![Stat::PrintExpr(expr)],
                BlockEnding::Exit(NumExpr::Const(NumSize::DWord, 0)),
            )],
            HashMap::new(),
            None,
        )
        .validate()
    }

    fn validate_block_graph(graph: Vec<Block>) -> Result<(), ()> {
        Program(HashMap::new(), HashMap::new(), graph, HashMap::new(), None).validate()
    }

    #[test]
    fn check_const_bounds_ok() {
        assert_eq!(
            validate_num_expr(
                NumExpr::Const(NumSize::Byte, -128),
                HashMap::new(),
                HashMap::new()
            ),
            Ok(())
        );
    }

    #[test]
    fn check_const_bounds_err() {
        assert_eq!(
            validate_num_expr(
                NumExpr::Const(NumSize::Byte, -129),
                HashMap::new(),
                HashMap::new()
            ),
            Err(())
        );
    }

    #[test]
    fn check_var_exists_ok() {
        assert_eq!(
            validate_num_expr(
                NumExpr::Var(0),
                HashMap::from([(0, Type::Num(NumSize::DWord))]),
                HashMap::new()
            ),
            Ok(())
        )
    }

    #[test]
    fn check_var_exists_err() {
        assert_eq!(
            validate_num_expr(NumExpr::Var(0), HashMap::new(), HashMap::new()),
            Err(())
        )
    }

    #[test]
    fn check_var_type_ok() {
        for (var_expr, var_type) in [
            (Expr::Num(NumExpr::Var(0)), Type::Num(NumSize::DWord)),
            (Expr::Bool(BoolExpr::Var(0)), Type::Bool),
            (Expr::Ptr(PtrExpr::Var(0)), Type::Ptr),
        ] {
            assert_eq!(
                validate_expr(var_expr, HashMap::from([(0, var_type)]), HashMap::new()),
                Ok(())
            );
        }
    }

    #[test]
    fn check_var_type_err() {
        for (var_expr, var_type) in [
            (Expr::Num(NumExpr::Var(0)), Type::Ptr),
            (Expr::Bool(BoolExpr::Var(0)), Type::Num(NumSize::Byte)),
            (Expr::Ptr(PtrExpr::Var(0)), Type::Bool),
        ] {
            assert_eq!(
                validate_expr(var_expr, HashMap::from([(0, var_type)]), HashMap::new()),
                Err(())
            );
        }
    }

    #[test]
    fn check_arith_op_ok() {
        assert_eq!(
            validate_num_expr(
                NumExpr::ArithOp(
                    box NumExpr::Const(NumSize::Word, 0),
                    ArithOp::Add,
                    box NumExpr::Const(NumSize::Word, 0)
                ),
                HashMap::new(),
                HashMap::new()
            ),
            Ok(())
        )
    }

    #[test]
    fn check_arith_op_err() {
        assert_eq!(
            validate_num_expr(
                NumExpr::ArithOp(
                    box NumExpr::Const(NumSize::Byte, 0),
                    ArithOp::Add,
                    box NumExpr::Const(NumSize::DWord, 0)
                ),
                HashMap::new(),
                HashMap::new()
            ),
            Err(())
        )
    }

    #[test]
    fn check_call_ok() {
        for (call_expr, ret_type) in [
            (
                Expr::Num(NumExpr::Call("f".to_string(), vec![])),
                Type::Num(NumSize::Byte),
            ),
            (
                Expr::Bool(BoolExpr::Call("f".to_string(), vec![])),
                Type::Bool,
            ),
            (Expr::Ptr(PtrExpr::Call("f".to_string(), vec![])), Type::Ptr),
        ] {
            assert_eq!(
                validate_call(call_expr, "f".to_string(), vec![], ret_type),
                Ok(())
            )
        }
    }

    #[test]
    fn check_call_exists_err() {
        assert_eq!(
            validate_call(
                Expr::Bool(BoolExpr::Call("f".to_string(), vec![])),
                "g".to_string(),
                vec![],
                Type::Bool
            ),
            Err(())
        )
    }

    #[test]
    fn check_call_args_num_err() {
        assert_eq!(
            validate_call(
                Expr::Bool(BoolExpr::Call(
                    "f".to_string(),
                    vec![Expr::Bool(BoolExpr::Const(true))]
                )),
                "g".to_string(),
                vec![],
                Type::Bool
            ),
            Err(())
        )
    }

    #[test]
    fn check_call_args_types_err() {
        assert_eq!(
            validate_call(
                Expr::Bool(BoolExpr::Call(
                    "f".to_string(),
                    vec![Expr::Bool(BoolExpr::Const(true))]
                )),
                "f".to_string(),
                vec![(Type::Ptr, 1)],
                Type::Bool
            ),
            Err(())
        )
    }

    #[test]
    fn check_call_args_collision_err() {
        assert_eq!(
            validate_call(
                Expr::Bool(BoolExpr::Call(
                    "f".to_string(),
                    vec![
                        Expr::Bool(BoolExpr::Const(true)),
                        Expr::Bool(BoolExpr::Const(true))
                    ]
                )),
                "f".to_string(),
                vec![(Type::Bool, 1), (Type::Bool, 1)],
                Type::Bool
            ),
            Err(())
        )
    }

    #[test]
    fn check_call_ret_type_err() {
        for (call_expr, ret_type) in [
            (
                Expr::Num(NumExpr::ArithOp(
                    box NumExpr::Call("f".to_string(), vec![]),
                    ArithOp::Add,
                    box NumExpr::Const(NumSize::DWord, 0),
                )),
                Type::Num(NumSize::Byte),
            ),
            (
                Expr::Bool(BoolExpr::Call("f".to_string(), vec![])),
                Type::Ptr,
            ),
            (
                Expr::Ptr(PtrExpr::Call("f".to_string(), vec![])),
                Type::Bool,
            ),
        ] {
            assert_eq!(
                validate_call(call_expr, "f".to_string(), vec![], ret_type),
                Err(())
            )
        }
    }

    #[test]
    fn check_dataref_exists_ok() {
        assert_eq!(
            validate_expr(
                Expr::Ptr(PtrExpr::DataRef(0)),
                HashMap::new(),
                HashMap::from([(0, vec![Expr::Bool(BoolExpr::Const(true))])])
            ),
            Ok(())
        )
    }

    #[test]
    fn check_dataref_exists_err() {
        assert_eq!(
            validate_expr(
                Expr::Ptr(PtrExpr::DataRef(0)),
                HashMap::new(),
                HashMap::new()
            ),
            Err(())
        )
    }

    #[test]
    fn check_incoming_properly_defined() {
        assert_eq!(
            validate_block_graph(vec![Block(
                vec![],
                vec![Stat::PrintEol()],
                BlockEnding::CondJumps(vec![], 0)
            )]),
            Err(())
        )
    }

    #[test]
    fn check_main_no_return() {
        assert_eq!(
            validate_block_graph(vec![Block(
                vec![],
                vec![],
                BlockEnding::Return(Expr::Num(NumExpr::Const(NumSize::DWord, 0)))
            )]),
            Err(())
        )
    }

    #[test]
    fn check_different_typed_returns() {
        assert_eq!(
            Program(
                HashMap::from([(
                    "f".to_string(),
                    Function(
                        Type::Num(NumSize::Byte),
                        vec![],
                        HashMap::new(),
                        vec![Block(
                            vec![],
                            vec![],
                            BlockEnding::Return(Expr::Num(NumExpr::Const(NumSize::Word, 0)))
                        )],
                    )
                )]),
                HashMap::new(),
                vec![Block(
                    vec![],
                    vec![],
                    BlockEnding::Exit(NumExpr::Const(NumSize::DWord, 0))
                )],
                HashMap::new(),
                None,
            )
            .validate(),
            Err(())
        )
    }

    #[test]
    fn check_exit_dword() {
        assert_eq!(
            Program(
                HashMap::new(),
                HashMap::new(),
                vec![Block(
                    vec![],
                    vec![],
                    BlockEnding::Exit(NumExpr::Const(NumSize::Word, 0))
                )],
                HashMap::new(),
                None,
            )
            .validate(),
            Err(())
        )
    }

    #[test]
    fn check_static_expressions_const() {
        assert_eq!(
            Program(
                HashMap::from([(
                    "f".to_string(),
                    Function(
                        Type::Num(NumSize::DWord),
                        vec![],
                        HashMap::new(),
                        vec![Block(
                            vec![],
                            vec![],
                            BlockEnding::Return(Expr::Num(NumExpr::Const(NumSize::DWord, 0)))
                        )]
                    )
                )]),
                HashMap::new(),
                vec![Block(
                    vec![],
                    vec![],
                    BlockEnding::Exit(NumExpr::Const(NumSize::DWord, 0))
                )],
                HashMap::from([(0, vec![Expr::Num(NumExpr::Call("f".to_string(), vec![]))],)]),
                None,
            )
            .validate(),
            Err(())
        )
    }

    #[test]
    fn check_int_handler_args() {
        assert_eq!(
            Program(
                HashMap::from([(
                    "f".to_string(),
                    Function(
                        Type::Num(NumSize::DWord),
                        vec![(Type::Ptr, 0)],
                        HashMap::new(),
                        vec![Block(
                            vec![],
                            vec![],
                            BlockEnding::Return(Expr::Num(NumExpr::Const(NumSize::DWord, 0))),
                        )],
                    ),
                )]),
                HashMap::new(),
                vec![Block(
                    vec![],
                    vec![],
                    BlockEnding::Exit(NumExpr::Const(NumSize::DWord, 0)),
                )],
                HashMap::new(),
                Some("f".to_string()),
            )
            .validate(),
            Err(())
        )
    }
}
