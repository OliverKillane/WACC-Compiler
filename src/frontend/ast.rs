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
//! let program = Program(vec![], vec![
//!         Stat::Assign(
//!             AssignLhs::Var(A),
//!             AssignRhs::Expr(
//!                 Expr::BinOp(
//!                     Box::new(Expr::Int(9)),
//!                     BinOp::Mul,
//!                     Box::new(
//!                         Expr::BinOp(
//!                         Int 3,
//!                         BinOp::Add,
//!                         Int 7)
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
//! Program(vec![
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
pub struct ArrayElem(String, Vec<Expr>);

/// All unary operators supported by WACC and used in [expressions](Expr).
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum UnOp {
    /// Logical negation
    /// ```text
    /// bool a = ! true ;
    /// bool b = ! (a && true) ;
    /// ```
    Neg,

    /// Integer negation
    /// ```text
    /// int a = -3 ;
    /// int b = -(a * 4) ;
    /// ```
    Minus,

    /// Gets the length of an array as an integer.
    /// ```text
    /// int[] arr = [1,2,3,4] ;
    /// int arr_length = len arr ;
    /// ```
    Len,

    /// Gets the ascii value associated with a character as an integer.
    /// ```text
    /// int a = ord 'a' ;
    /// ```
    Ord,

    /// Gets the character associated with a given integer ascii value.
    /// ```text
    /// char a = chr 97 ;
    /// ```
    Chr,
}

/// Binary Operators supported by WACC available for use in [expressions](Expr).
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
///
/// # Examples:
/// ## Example of nested binary operators
/// ```text
/// (4 - 3) + (9 * 7)
/// ```
/// ```
/// Expr::BinOp(
///     Box::new(
///         Expr::BinOp(
///             Box::new(Expr::Int(4)),
///             BinOp::Sub,
///             Box::new(Expr::Int(3))
///         ),
///         BinOp::Add,
///         Expr::BinOp(
///             Box::new(Expr::Int(9)),
///             BinOp::Mul,
///             Box::new(Expr::Int(7))
///         )
///     )
/// )
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    Null,
    Int(u32),
    Bool(bool),
    Char(char),
    String(String),
    Var(String),
    ArrayElem(ArrayElem),

    /// Unary operator application determined by the [UnOp enum](Unop).
    UnOp(UnOp, Box<Expr>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    BinOp(Box<Expr>, BinOp, Box<Expr>),
}

/// Lefthand side of assignments.
/// ```text
/// AssignLHS = AssignRHS ;
/// ```
pub enum AssignLhs {
    Var(String),
    ArrayElem(ArrayElem),
    PairFst(Expr),
    PairSnd(Expr),
}

/// Righthand side of assignments.
/// ```text
/// AssignLHS = AssignRHS ;
/// ```
pub enum AssignRhs {
    /// Assigns an [expression](Expr)
    /// ```
    /// AssignLHS = AssignRHS ;
    /// ```
    Expr(Expr),

    Array(Vec<Expr>),
    NewPair(Expr, Expr),
    PairFst(Expr),
    PairSnd(Expr),
    Call(String, Vec<Expr>),
}

pub enum Stat {
    Skip,
    Def(Type, String, AssignRhs),
    Assign(AssignLhs, AssignRhs),
    Read(AssignLhs),
    Free(Expr),
    Return(Expr),
    Exit(Expr),
    Print(Expr),
    PrintLn(Expr),
    If(Expr, Body, Body),
    While(Expr, Body),
    Scope(Body),
}

pub struct Body(pub Vec<Stat>); // Will also contain a symbol table

pub struct Param(pub Type, pub String);

pub struct Function(pub Type, pub String, pub Vec<Param>, pub Body);

pub struct Program(pub Vec<Function>, pub Body);

use std::collections::HashMap;
use std::rc::{Rc, Weak};
struct SymbolTableContents<T>(HashMap<String, T>, Option<Weak<SymbolTable<T>>>);

pub struct SymbolTable<T>(Rc<SymbolTableContents<T>>);

impl<T> SymbolTable<T> {
    /// Search symbol table for an identifier, including all parent symbol tables/scopes.
    fn find(&self, ident: &str) -> Option<&T> {
        unimplemented!();
        let SymbolTableContents(ref hash_map, ref parent) = *(self.0);
        if let Some(val) = hash_map.get(ident) {
            return Some(val);
        }
        if let Some(ref parent) = parent {
            parent.upgrade().unwrap().clone().find(ident)
        } else {
            None
        }
    }
}
