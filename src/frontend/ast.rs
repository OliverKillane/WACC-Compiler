//! This module defines the types for the AST structure that is produced by the
//! parser and consumed by the semantic analyser.
//!
//! # Examples
//! ## Empty code with skip statement
//! WACC code:
//! ```text
//! begin  
//!   int a = 9 * (3 + 7)
//! end
//! ```
//! AST definition:
//! ```
//! let program: Program = Program(vec![], vec![
//!         Stat::Assign(
//!             AssignLhs::Var(A),
//!             AssignRhs::Expr(
//!                 ExprKind::BinOp(
//!                     ExprKind::Int(9),
//!                     BinOp::Mul,
//!                     ExprKind::BinOp(
//!                         Int 3,
//!                         BinOp::Add,
//!                         Int 7
//!                     )
//!                 )
//!             )
//!         )
//!        ]
//!    );
//! ```
//!
//! ## Example of a function call
//! WACC code:
//! ```text
//! begin
//!   int function_name(int a) is
//!     return a + 9
//!   end
//!   int variable = call function_name(6)
//! end
//! ```
//! AST definition:
//! ```
//! let aa = Program(vec![
//! Function(Type::Int, String::from("function_name"),
//!     vec![ Param(Type::Int, String::from("a"))],
//!     vec![
//!         Stat::Return(
//!             Expr::BinOp(
//!                 Expr::Var(String::from("a")),
//!                 BinOp::Add, Expr::Int(9)
//!             )
//!         )
//!     ]
//! )
//! ], vec![
//!     Stat::Assign(
//!         AssignLhs::Var(String::from("variable")),
//!         AssignRhs::Call(String::from("function_name"), vec![Expr::Int(6)]))
//! ]);
//! ```
//!

/// Type specification in WACC.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    /// Integer primitive type.
    Int,
    /// Boolean primitive type.
    Bool,
    /// Character primitive type.
    Char,
    /// String primitive type.
    String,
    /// Typeless pair type. Can be coerced from or to any regular pair type.
    GenericPair,
    /// Typed pair type. The arguments to the nameless tuple are the types of
    /// the first and second field of the pair respectively.
    Pair(Box<Type>, Box<Type>),
    /// Multidimentional array. The first argument is the type of elements in
    /// the arrays. The second argument is the dimensionality of the array.
    ///
    /// **Note**: The type of the array elements should not be Array.
    Array(Box<Type>, usize),
}

/// An index into an array variable in WACC. The first argument specifies the
/// name of the variable, and the second the indicies to the array dimensions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayElem(String, Vec<ExprKind>);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Minus,
    Len,
    Ord,
    Chr,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Ne,
    And,
    Or,
}

/// Expression Data Type
/// ```
///
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExprKind {
    Int(u32),
    Bool(bool),
    Char(char),
    String(String),
    Null,
    Var(String),
    ArrayElem(ArrayElem),
    UnOp(UnOp, Box<ExprKind>),
    BinOp(Box<ExprKind>, BinOp, Box<ExprKind>),
}

pub enum AssignLhs {
    Var(String),
    ArrayElem(ArrayElem),
    PairFst(ExprKind),
    PairSnd(ExprKind),
}

pub enum AssignRhs {
    Expr(ExprKind),
    Array(Vec<ExprKind>),
    NewPair(ExprKind, ExprKind),
    PairFst(ExprKind),
    PairSnd(ExprKind),
    Call(String, Vec<ExprKind>),
}

pub enum Stat {
    Skip,
    Def(Type, String, AssignRhs),
    Assign(AssignLhs, AssignRhs),
    Read(AssignLhs),
    Free(ExprKind),
    Return(ExprKind),
    Exit(ExprKind),
    Print(ExprKind),
    PrintLn(ExprKind),
    If(ExprKind, Vec<Stat>, Vec<Stat>),
    While(ExprKind, Vec<Stat>),
    Scope(Vec<Stat>),
}

pub struct Param(Type, String);

pub struct Function(Type, String, Vec<Param>, Vec<Stat>);

pub struct Program(Vec<Function>, Vec<Stat>);
