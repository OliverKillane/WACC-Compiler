use std::collections::HashMap;

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
    /// Signifies a 4-byte expression, for example an int
    DWord,
    /// Signifies a 2-byte expression
    Word,
    /// Signifies a 1-byte expression, for example a char
    Byte,
}

/// A numeric expression. Can represent for example
/// character or integer expressions.
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

/// A boolean expression.
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

/// A pointer manipulation expressiion.
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

/// A block of statements. Contains the blocks that have conditional jumps to it. The
/// list of statements must not be empty.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block(pub Vec<BlockId>, pub Vec<Stat>, pub BlockEnding);
/// A graph of blocks. The index of the block in the block graph signifies the
/// [block id](BlockId) of that block.
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
/// for local variables used, the [block graph](BlockGraph) for its body and the
/// map of structs in the data section for the whole program. The execution starts
/// off from the first block in the block graph. The expressions in the data
/// section structs may not use any variable references.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program(
    pub HashMap<String, Function>,
    pub HashMap<VarRepr, Type>,
    pub BlockGraph,
    pub HashMap<DataRef, Vec<Expr>>,
);
