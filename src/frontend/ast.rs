//! This module defines the types for the AST structure that is produced by the
//! parser and consumed by the semantic analyser.
//! 
//! References to the source code represented in the tree are kept to allow for 
//! beautiful error messages from the semantic analyser.
//! 
//! The AST is generic for variable identifiers, this allows for renaming to 
//! occur (as is required for our symbol table generation).
//! 
//! Function identifiers are contained withing Strings, allowing them to live 
//! when the source code string is dropped and to be used in assembly 
//! generation.
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
//! Program(
//!     vec![],
//!     vec![
//!         Stat("int a = 9 * (3 + 7)", StatCode::Def(
//!             Type::Int,
//!             "a",
//!             AssignRhs::Expr(
//!                 Expr(
//!                 "9 * (3 + 7)",
//!                 ExprCode::BinOp(
//!                     Box::from(Expr("9", ExprCode::Int(9))),
//!                     BinOp::Mul,
//!                     Box::from(Expr("(3 + 7)", ExprCode::BinOp(
//!                         Box::from(Expr("3", ExprCode::Int(3))),
//!                         BinOp::Add,
//!                         Box::from(Expr("7", ExprCode::Int(7)))
//!                     )))
//!                 ))
//!             )
//!         ))
//!     ]
//!     );
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
//! Program(
//!     vec![Function(
//!         "function_name(int a)",
//!         Type::Int,
//!         String::from("function_name"),
//!         vec![Param("int a", Type::Int, "a")],
//!         vec![Stat(
//!             "return a + 9",
//!             StatCode::Return(Expr(
//!                 "a + 9",
//!                 ExprCode::BinOp(
//!                     Box::from(Expr("a", ExprCode::Var("a"))),
//!                     BinOp::Add,
//!                     Box::from(Expr("9", ExprCode::Int(9))),
//!                 ),
//!             )),
//!         )],
//!     )],
//!     vec![Stat(
//!         "int variable = call function_name(6)",
//!         StatCode::Def(
//!             Type::Int,
//!             "variable",
//!             AssignRhs::Call(
//!                 String::from("function_name"),
//!                 vec![Expr("6", ExprCode::Int(6))],
//!             ),
//!         ),
//!     )],
//! );
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
/// Expr("(4 - 3) + (9 * 7)", ExprCode::BinOp(
///     Box::from(Expr("4 - 3", ExprCode::BinOp(
///         Box::from(Expr("4", ExprCode::Int(4))),
///         BinOp::Sub,
///         Box::from(Expr("3", ExprCode::Int(3)))
///     ))),
///     BinOp::Add,
///     Box::from(Expr("9 * 7", ExprCode::BinOp(
///         Box::from(Expr("9", ExprCode::Int(9))),
///         BinOp::Mul,
///         Box::from(Expr("7", ExprCode::Int(7)))
///     ))),
/// ))
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExprCode<'a, IdRepr> {
    Null,
    Int(u32),
    Bool(bool),
    Char(char),
    String(String),
    Var(IdRepr),
    ArrayElem(IdRepr, Vec<Expr<'a, IdRepr>>),

    /// Unary operator application determined by the [UnOp enum](UnOp).
    UnOp(UnOp, Box<Expr<'a, IdRepr>>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    BinOp(Box<Expr<'a, IdRepr>>, BinOp, Box<Expr<'a, IdRepr>>),
}

/// Lefthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
pub enum AssignLhs<'a, IdRepr> {
    /// Variable assignment.
    /// ```text
    /// int a = 3 ;
    /// ```
    Var(IdRepr),

    /// Array element assignment.
    /// ```text
    /// int[] a = [1,2,3,4] ;
    /// a[0] = 9 ;
    /// ```
    ArrayElem(Expr<'a, IdRepr>),

    /// Assign to the first element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// fst a = 3 ;
    /// ```
    PairFst(Expr<'a, IdRepr>),

    /// Assign to the second element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// snd a = false ;
    /// ```
    PairSnd(Expr<'a, IdRepr>),
}

/// Righthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
pub enum AssignRhs<'a, IdRepr> {
    /// Assigns an expression.
    /// ```text
    /// int a = 3 + (4 * 5) ;
    /// ```
    Expr(Expr<'a, IdRepr>),

    /// Array assignment by expressions of the same type.
    /// ```text
    /// int[] int_arr = [2, 3 + 3, 4 * 7, 0] ;
    /// ```
    Array(Vec<Expr<'a, IdRepr>>),

    /// Assigns to a new pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(3 + 3, true && false) ;
    /// ```
    NewPair(Expr<'a, IdRepr>, Expr<'a, IdRepr>),

    /// Assign the first element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// int a_fst = fst a_pair ;
    /// ```
    PairFst(Expr<'a, IdRepr>),

    /// Assign the second element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// bool a_snd = snd a_pair ;
    /// ```
    PairSnd(Expr<'a, IdRepr>),

    /// Assign the return value of a function, with arguments.
    /// ```text
    /// int add(int a, int b) is
    ///     return a + b
    /// end
    ///
    /// int a = add(3,4) ;
    /// ```
    Call(String, Vec<Expr<'a, IdRepr>>),
}

/// Statements for assignment, control flow and definitions.
pub enum StatCode<'a, IdRepr> {
    /// A _no-operation_ instruction.
    Skip,

    /// Variable Definition
    /// ```text
    /// int a = 99;
    /// ```
    Def(Type, IdRepr, AssignRhs<'a, IdRepr>),

    /// Assignment (to variable, parts of pairs or arrays).
    /// ```text
    /// int a = 9 ;
    /// ```
    /// ```text
    /// int[] arr = [1,2,3,4] ;
    /// a[1] = call function(3, 4) ;
    /// ```
    Assign(AssignLhs<'a, IdRepr>, AssignRhs<'a, IdRepr>),

    /// Read from standard input.
    /// ```text
    /// int a = 9 ;
    /// read a;
    /// ```
    Read(AssignLhs<'a, IdRepr>),

    /// Free dynamically allocated data structures.
    /// ```text
    /// pair(int, int) a_pair = newpair(3,3) ;
    /// free a_pair ;
    /// ```
    Free(Expr<'a, IdRepr>),

    /// Return value from a subroutine/function.
    /// ```text
    /// int double(int b) is
    ///     return 2 * b
    /// end
    /// ```
    Return(Expr<'a, IdRepr>),

    /// End the program and return the exit code.
    /// ```text
    /// exit 0 ;
    /// ```
    Exit(Expr<'a, IdRepr>),

    /// Print text to the console.
    /// ```text
    /// int a = 'a' ;
    /// print a ;
    /// ```
    Print(Expr<'a, IdRepr>),

    /// Print text to the console and start a new line.
    /// ```text
    /// int a = 'a' ;
    /// print a + 9 ;
    /// ```
    PrintLn(Expr<'a, IdRepr>),

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
    If(
        Expr<'a, IdRepr>,
        Vec<Stat<'a, IdRepr>>,
        Vec<Stat<'a, IdRepr>>,
    ),

    /// While Loop control flow structure.
    /// ```text
    /// int n = 0 ;
    /// while n < 10 do
    ///     n = n + 1
    /// done ;
    /// ```
    While(Expr<'a, IdRepr>, Vec<Stat<'a, IdRepr>>),
}

/// Statement containing the span of the statement (for error messages), as
/// well as the statement code itself.
pub struct Stat<'a, IdRepr>(pub &'a str, pub StatCode<'a, IdRepr>);

/// Expression containing a span of the expression as well as the expression
/// code itself.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expr<'a, IdRepr>(pub &'a str, pub ExprCode<'a, IdRepr>);

/// Formal parameter used in functions.
/// (span of parameter definition, type, parameter identifier)
pub struct Param<'a, IdRepr>(&'a str, pub Type, IdRepr);

/// Function definition with return type, name, parameters, the span of the
/// definition and the code body.
/// ```text
/// int sum(int, a, int, b, int c) is
///     return a + b + c
/// end
/// ```
pub struct Function<'a, IdRepr>(
    pub &'a str,
    pub Type,
    pub String,
    pub Vec<Param<'a, IdRepr>>,
    pub Vec<Stat<'a, IdRepr>>,
);

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
///     exit 0
/// end
/// ```
pub struct Program<'a, IdRepr>(pub Vec<Function<'a, IdRepr>>, pub Vec<Stat<'a, IdRepr>>);
