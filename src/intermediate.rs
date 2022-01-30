use std::collections::HashMap;

type VarRepr = u32;
type BlockId = usize;

enum Expr {
    DWordExpr(DWordExprBox), //
    ByteExpr(ByteExpr),
}

// Encodes integers, pairs, arrays, strings and pointers
enum DWordExpr {
    // null pair/integer constant
    Const(i32),

    // Value inside the variable
    Var(VarRepr),

    // Ascii index of a character
    Ord(ByteExprBox),

    // Arithmetic operations on integers
    Add(DWordExprBox, DWordExprBox),
    Sub(DWordExprBox, DWordExprBox),
    Mul(DWordExprBox, DWordExprBox),
    Div(DWordExprBox, DWordExprBox),
    Mod(DWordExprBox, DWordExprBox),

    // Call to a function that returns an integer/pair/array
    Call(String, Vec<Expr>),

    // Allocation of an array containing integers/pairs/arrays
    MallocDWordArray(Vec<DWordExpr>),
    // Pointer to an element of an array containing integers/pairs/arrays
    IndexDWordArray(DWordExprBox, DWordExprBox),
    // Length of an array containing integers/pairs/arrays
    DWordArrayLen(DWordExprBox),

    // Allocation of an array containing characters/booleans (or a string)
    MallocByteArray(Vec<ByteExpr>),
    // Pointer to an element of an array containing characters/booleans
    IndexByteArray(DWordExprBox, DWordExprBox),
    // Length of an array containing characters/booleans (or a string)
    ByteArrayLen(DWordExprBox),

    // Allocation of a pair
    MallocPair(Expr, Expr),
    // First element of a pair
    PairFirst(DWordExprBox),
    // Second element of a pair
    PairSecond(DWordExprBox),

    // Dereference of a pointer to an integer/pair/array
    Deref(DWordExprBox),
}

type DWordExprBox = Box<DWordExpr>;

enum ByteExpr {
    // Character/boolean constant
    Const(u8),

    // Value inside the variable
    Var(VarRepr),

    // Dereference of a pointer to an integer/pair/array
    Deref(DWordExprBox),

    // Character with a given ascii number
    Chr(DWordExprBox),

    // Call to a function returning a character/boolean
    Call(String, Vec<Expr>),

    // Less than operation between integers
    DWordLt(DWordExprBox, DWordExprBox),
    // Less than operation between characters
    ByteLt(ByteExprBox, ByteExprBox),
    // Equality between integers/arrays/pairs
    DWordEq(DWordExprBox, DWordExprBox),
    // Equality between characters/booleans
    ByteEq(ByteExprBox, ByteExprBox),

    // Logical and
    And(ByteExprBox, ByteExprBox),
    // Logical or
    Or(ByteExprBox, ByteExprBox),
    // Logical negation
    Neg(ByteExprBox),
}

type ByteExprBox = Box<ByteExpr>;

enum Stat {
    // Assignment of a DWord Expression to a varialbe
    AssignDWordVar(VarRepr, DWordExpr),
    // Assignment of a Byte Expression to a varialbe
    AssignByteVar(VarRepr, ByteExpr),
    // Assignment of a DWord Expression to a DWord under a pointer
    AssignDWordPtr(DWordExpr, DWordExpr),
    // Assignment of a Byte Expression to a Byte under a pointer
    AssignBytePtr(DWordExpr, ByteExpr),

    // Read an integer into a variable
    ReadDWordVar(VarRepr),
    // Read a character into a variable
    ReadByteVar(VarRepr),
    // Read an integer into a DWord pointer
    ReadDWordPtr(DWordExpr),
    // Read a character into a Byte pointer
    ReadBytePtr(DWordExpr),

    // Free an array of integers/pairs/arrays
    FreeDWordArray(DWordExpr),
    // Free an array of characters/booleans
    FreeByteArray(DWordExpr),
    // Free a pair
    FreePair(DWordExpr),

    // Print an integer
    PrintInt(DWordExpr),
    // Print a character
    PrintChar(ByteExpr),
    // Print a boolean
    PrintBool(ByteExpr),
    // Print a string/character array
    PrintStr(DWordExpr),
    // Print a pointer (regular array/pair)
    PrintPtr(DWordExpr),
    // Print a newline character
    PrintEol(),
}

enum BlockEnding {
    // Conditional jump - if the expression is non-zero then do the first jump,
    // otherwise do the second jump
    CondJump(ByteExpr, BlockId, BlockId),
    // Unconditional jump - jump to the given block
    Jump(BlockId),
    // Return an integer/array/pair
    DWordReturn(DWordExpr),
    // Return a character/boolean
    ByteReturn(ByteExpr),
    // Exit with a given value
    Exit(DWordExpr),
}

struct Block(Vec<Stat>, BlockEnding);

type BlockGraph = Vec<Block>;

enum Param {
    DWord(VarRepr),
    Byte(VarRepr),
}

struct Function(Vec<Param>, BlockGraph);

struct Program(HashMap<String, Function>, BlockGraph);
