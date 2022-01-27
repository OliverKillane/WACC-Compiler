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
/// name of the variable, and the second the indices to the array dimensions.
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
    // Integer addition (+).
    Add,

    // Integer subtraction(-).
    Sub,

    // Integer multipication (*).
    Mul,

    // Integer division (/).
    Div,

    // Integer modulus.
    Mod,

    // Comparison greater-than (>).
    Gt,

    // Comparison greater-than or equal (>=).
    Gte,

    // Comparison less-than (<).
    Lt,

    // Comparison less-than or equal (<=).
    Lte,

    // Comparison equality (==).
    Eq,

    // Comparison not equal (!=).
    Ne,

    // Logical conjunction/and (&&).
    And,

    // Logical disjunction/or (||).
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

    /// Unary operator application determined by the [UnOp enum](UnOp).
    UnOp(UnOp, Box<Expr>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    BinOp(Box<Expr>, BinOp, Box<Expr>),
}

/// Lefthand side of assignments.
/// ```text
/// AssignLHS = AssignRHS ;
/// ```
pub enum AssignLhs {
    /// Variable assignment.
    /// ```text
    /// int a = 3 ;
    /// ```
    Var(String),

    /// Array element assignment.
    /// ```text
    /// int[] a = [1,2,3,4] ;
    /// a[0] = 9 ;
    /// ```
    ArrayElem(ArrayElem),

    /// Assign to the first element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// fst a = 3 ;
    /// ```
    PairFst(Expr),

    /// Assign to the second element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// snd a = false ;
    /// ```
    PairSnd(Expr),
}

/// Righthand side of assignments.
/// ```text
/// AssignLHS = AssignRHS ;
/// ```
pub enum AssignRhs {
    /// Assigns an expression.
    /// ```text
    /// int a = 3 + (4 * 5)
    /// ```
    Expr(Expr),

    /// Array assignment by expressions of the same type.
    /// ```text
    /// int[] int_arr = [2, 3 + 3, 4 * 7, 0] ;
    /// ```
    Array(Vec<Expr>),

    /// Assigns to a new pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(3 + 3, true && false) ;
    /// ```
    NewPair(Expr, Expr),

    /// Assign the first element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// int a_fst = fst a_pair ;
    /// ```
    PairFst(Expr),

    /// Assign the second element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// bool a_snd = snd a_pair ;
    /// ```
    PairSnd(Expr),

    /// Assign the return value of a function, with arguments.
    /// ```text
    /// int add(int a, int b) is
    ///     return a + b
    /// end
    ///
    /// int a = add(3,4) ;
    /// ```
    Call(String, Vec<Expr>),
}

/// Statements for assignment, control flow and definitions.
pub enum Stat {
    /// A _no-operation_ instruction.
    Skip,

    /// Function Definition
    /// ```text
    /// int function(int a, int b) is
    ///     skip ;
    ///     return a * 2 + b
    /// end
    /// ```
    Def(Type, String, AssignRhs),

    /// Assignment (to variable, parts of pairs or arrays).
    /// ```text
    /// int a = 9 ;
    /// ```
    /// ```text
    /// int[] arr = [1,2,3,4] ;
    /// a[1] = call function(3, 4) ;
    /// ```
    Assign(AssignLhs, AssignRhs),

    /// Read from standard input.
    /// ```text
    /// int a = 9 ;
    /// read a;
    /// ```
    Read(AssignLhs),

    /// Free dynamically allocated data structures.
    /// ```text
    /// pair(int, int) a_pair = newpair(3,3) ;
    /// free a_pair ;
    /// ```
    Free(Expr),

    /// Return value from a subroutine/function.
    /// ```text
    /// int double(int b) is
    ///     return 2 * b
    /// end
    /// ```
    Return(Expr),

    /// End the program and return the exit code.
    /// ```text
    /// exit 0 ;
    /// ```
    Exit(Expr),

    /// Print text to the console.
    /// ```text
    /// int a = 'a' ;
    /// print a ;
    /// ```
    Print(Expr),

    /// Print text to the console and start a new line.
    /// ```text
    /// int a = 'a' ;
    /// print a + 9 ;
    /// ```
    PrintLn(Expr),

    /// If-Else statement to alter control flow.
    /// ```text
    ///
    /// if 3 = 4 - 1
    /// then
    ///     print 'a'
    /// else
    ///     print 'b'
    /// fi ;
    /// ```
    If(Expr, Body, Body),

    /// While Loop control flow structure.
    /// ```text
    /// int n = 0 ;
    /// while n < 10 do
    ///     n = n + 1
    /// done ;
    /// ```
    While(Expr, Body),

    /// Scope statement for begin-end delimited blocks of statements.
    Scope(Body),
}

/// Body stores a symbol table associated and an block of code.
pub struct Body(pub Vec<Stat>); // Will also contain a symbol table

/// Formal parameter used in functions.
pub struct Param(pub Type, pub String);

/// Function definition with return type, name, parameters and the code body.
/// ```text
/// int sum(int, a, int, b, int c) is
///     return a + b + c
/// end
/// ```
pub struct Function(pub Type, pub String, pub Vec<Param>, pub Body);

/// Program is the root of the abstract syntax tree, containing all function
/// definitions and the main program body.
/// ```text
/// begin
///     int fun(char a) is
///         int x = ord a ;
///         return x
///     end
///
///     int i = fun('a') ;
///      exit 0
/// end
/// ```
pub struct Program(pub Vec<Function>, pub Body);
