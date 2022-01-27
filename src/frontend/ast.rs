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
//!     int a = 9 * (3 + 7)
//! end
//! ```
//! AST definition:
//! ```
//! Program(
//!     vec![],
//!     vec![
//!         WrapSpan("int a = 9 * (3 + 7)", Stat::Def(
//!             Type::Int,
//!             "a",
//!             AssignRhs::Expr(
//!                 WrapSpan(
//!                 "9 * (3 + 7)",
//!                 Expr::BinOp(
//!                     Box::from(WrapSpan("9", Expr::Int(9))),
//!                     BinOp::Mul,
//!                     Box::from(WrapSpan("(3 + 7)", Expr::BinOp(
//!                         Box::from(WrapSpan("3", Expr::Int(3))),
//!                         BinOp::Add,
//!                         Box::from(WrapSpan("7", Expr::Int(7)))
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
//!     int function_name(int a) is
//!         return a + 9
//!     end
//!
//!     int variable = call function_name(6)
//! end
//! ```
//! AST definition:
//! ```
//! Program(
//!     vec![WrapSpan(
//!         "function_name(int a)",
//!         Function(
//!             Type::Int,
//!             String::from("function_name"),
//!             vec![Param("int a", Type::Int, "a")],
//!             vec![Stat(
//!                 "return a + 9",
//!                 StatCode::Return(Expr(
//!                     "a + 9",
//!                     ExprCode::BinOp(
//!                         Box::from(Expr("a", ExprCode::Var("a"))),
//!                         BinOp::Add,
//!                         Box::from(Expr("9", ExprCode::Int(9))),
//!                     ),
//!                 )),
//!             )],
//!         )
//!     )],
//!     vec![WrapSpan(
//!         "int variable = call function_name(6)",
//!         Stat::Def(
//!             Type::Int,
//!             "variable",
//!             AssignRhs::Call(
//!                 String::from("function_name"),
//!                 vec![WrapSpan("6", Expr::Int(6))],
//!             ),
//!         ),
//!     )],
//! );
//! ```

/// A wrapper for AST nodes containing the span of the original code for which
/// the wrapped structure represents.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct WrapSpan<'a, T>(pub &'a str, pub T);

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
    Neg,

    /// Integer negation
    Minus,

    /// Gets the length of an array as an integer.
    Len,

    /// Gets the ascii value associated with a character as an integer.
    Ord,

    /// Gets the character associated with a given integer ascii value.
    Chr,
}

/// Binary Operators supported by WACC available for use in [expressions](Expr).
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BinOp {
    /// Integer addition (+).
    Add,

    /// Integer subtraction(-).
    Sub,

    /// Integer multiplication (*).
    Mul,

    /// Integer division (/).
    Div,

    /// Integer modulus.
    Mod,

    /// Comparison greater-than (>).
    Gt,

    /// Comparison greater-than or equal (>=).
    Gte,

    /// Comparison less-than (<).
    Lt,

    /// Comparison less-than or equal (<=).
    Lte,

    /// Comparison equality (==).
    Eq,

    /// Comparison not equal (!=).
    Ne,

    /// Logical conjunction/and (&&).
    And,

    /// Logical disjunction/or (||).
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
/// WrapSpan("(4 - 3) + (9 * 7)", Expr::BinOp(
///     Box::from(WrapSpan("4 - 3", Expr=::BinOp(
///         Box::from(WrapSpan("4", Expr::Int(4))),
///         BinOp::Sub,
///         Box::from(WrapSpan("3", Expr::Int(3)))
///     ))),
///     BinOp::Add,
///     Box::from(WrapSpan("9 * 7", Expr::BinOp(
///         Box::from(WrapSpan("9", Expr::Int(9))),
///         BinOp::Mul,
///         Box::from(WrapSpan("7", Expr::Int(7)))
///     ))),
/// ))
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr<'a, IdRepr> {
    /// null value that can be used for pair values.
    /// ```text
    /// pair(int, int) example = null
    /// ```
    /// ```
    /// WrapSpan(
    ///     "pair(int, int) example = null",
    ///     Stat::Def(
    ///         Type::Pair(Box::from(Type::Int), Box::from(Type::Int)),
    ///         "example",
    ///         AssignRhs::Expr(WrapSpan("null", Expr::Null)),
    ///     ),
    /// )
    /// ```
    Null,

    /// Integer constants
    /// ```text
    /// int a = 9
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = 9",
    ///     Stat::Def(
    ///         Type::Bool,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("9", Expr::Int(9))),
    ///     ),
    /// );
    /// ```
    Int(u32),

    /// Boolean constants
    /// ```text
    /// bool a = true
    /// ```
    /// ```
    /// WrapSpan(
    ///     "bool a = true",
    ///     Stat::Def(
    ///         Type::Bool,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("true", Expr::Bool(true))),
    ///     ),
    /// );
    /// ```
    Bool(bool),

    /// Character constants
    /// ```text
    /// char a = 'a'
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = 'a'",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("'a'", Expr::Char('a'))),
    ///     ),
    /// );
    /// ```
    Char(char),

    /// String Constants
    /// ```text
    /// string a = "hello world"
    /// ```
    /// ```
    /// WrapSpan(
    ///     "string a = \"hello world\"",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("\"hello world\"", Expr::String(String::from("hello world")))),
    ///     ),
    /// );
    /// ```
    String(String),

    /// Variable reference
    /// ```text
    /// int a = 7 ;
    /// int b = a;
    /// ```
    /// ```
    /// vec![
    ///     WrapSpan(
    ///         "int a = 7",
    ///         Stat::Def(Type::Int, "a", AssignRhs::Expr(WrapSpan("7", Expr::Int(7)))),
    ///     ),
    ///     WrapSpan(
    ///         "int b = a",
    ///         Stat::Def(
    ///             Type::Int,
    ///             "b",
    ///             AssignRhs::Expr(WrapSpan("a", Expr::Var("a"))),
    ///         ),
    ///     ),
    /// ];
    /// ```
    Var(IdRepr),

    /// Array indexing
    /// ```text
    /// # Given some int[][] b
    /// int a = b[3][5] ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = b[3][5]",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "b[3][5]",
    ///             Expr::ArrayElem(
    ///                 "b",
    ///                 vec![WrapSpan("3", Expr::Int(3)), WrapSpan("5", Expr::Int(5))],
    ///             ),
    ///         )),
    ///     ),
    /// )
    /// ```
    ArrayElem(IdRepr, Vec<WrapSpan<'a, Expr<'a, IdRepr>>>),

    /// Unary operator application determined by the [UnOp enum](UnOp).
    UnOp(UnOp, Box<WrapSpan<'a, Expr<'a, IdRepr>>>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    BinOp(
        Box<WrapSpan<'a, Expr<'a, IdRepr>>>,
        BinOp,
        Box<WrapSpan<'a, Expr<'a, IdRepr>>>,
    ),
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
    ArrayElem(IdRepr, WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Assign to the first element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// fst a = 3 ;
    /// ```
    PairFst(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Assign to the second element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// snd a = false ;
    /// ```
    PairSnd(WrapSpan<'a, Expr<'a, IdRepr>>),
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
    Expr(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Array assignment by expressions of the same type.
    /// ```text
    /// int[] int_arr = [2, 3 + 3, 4 * 7, 0] ;
    /// ```
    Array(Vec<WrapSpan<'a, Expr<'a, IdRepr>>>),

    /// Assigns to a new pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(3 + 3, true && false) ;
    /// ```
    NewPair(
        WrapSpan<'a, Expr<'a, IdRepr>>,
        WrapSpan<'a, Expr<'a, IdRepr>>,
    ),

    /// Assign the first element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// int a_fst = fst a_pair ;
    /// ```
    PairFst(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Assign the second element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// bool a_snd = snd a_pair ;
    /// ```
    PairSnd(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Assign the return value of a function, with arguments.
    /// ```text
    /// int add(int a, int b) is
    ///     return a + b
    /// end
    ///
    /// int a = add(3,4) ;
    /// ```
    Call(String, Vec<WrapSpan<'a, Expr<'a, IdRepr>>>),
}

/// Statements for assignment, control flow and definitions.
pub enum Stat<'a, IdRepr> {
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
    Free(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Return value from a subroutine/function.
    /// ```text
    /// int double(int b) is
    ///     return 2 * b
    /// end
    /// ```
    Return(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// End the program and return the exit code.
    /// ```text
    /// exit 0 ;
    /// ```
    Exit(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Print text to the console.
    /// ```text
    /// int a = 'a' ;
    /// print a ;
    /// ```
    Print(WrapSpan<'a, Expr<'a, IdRepr>>),

    /// Print text to the console and start a new line.
    /// ```text
    /// int a = 'a' ;
    /// print a + 9 ;
    /// ```
    PrintLn(WrapSpan<'a, Expr<'a, IdRepr>>),

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
        WrapSpan<'a, Expr<'a, IdRepr>>,
        Vec<WrapSpan<'a, Stat<'a, IdRepr>>>,
        Vec<WrapSpan<'a, Stat<'a, IdRepr>>>,
    ),

    /// While Loop control flow structure.
    /// ```text
    /// int n = 0 ;
    /// while n < 10 do
    ///     n = n + 1
    /// done ;
    /// ```
    While(
        WrapSpan<'a, Expr<'a, IdRepr>>,
        Vec<WrapSpan<'a, Stat<'a, IdRepr>>>,
    ),
}

/// Formal parameter used in functions, containing the type and identifier.
pub struct Param<IdRepr>(pub Type, IdRepr);

/// Function definition with the return type, name, parameters, and code body.
/// ```text
/// int sum(int a, int, b) is
///     return a + b
/// end
/// ```
/// ```
/// WrapSpan(
///     "int sum(int a, int, b)",
///     Function(
///         Type::Int,
///         String::from("sum"),
///         vec![WrapSpan("int a", Param(Type::Int, "a")),
///         WrapSpan("int b", Param(Type::Int, "b")),],
///         vec![
///             WrapSpan("return a + b", Stat::Return(WrapSpan("a + b", Expr::BinOp(
///                 Box::from(WrapSpan("a", Expr::Var("a"))),
///                 BinOp::Add,
///                 Box::from(WrapSpan("b", Expr::Var("b"))),
///             ))))
///         ]
///     )
/// )
/// ```
pub struct Function<'a, IdRepr>(
    pub Type,
    pub String,
    pub Vec<WrapSpan<'a, Param<IdRepr>>>,
    pub Vec<WrapSpan<'a, Stat<'a, IdRepr>>>,
);

/// Program is the root of the abstract syntax tree, containing all function
/// definitions and the main program body, with all nested structures being 
/// associated with the original source code by [wrappers](WrapSpan).
pub struct Program<'a, IdRepr>(
    pub Vec<WrapSpan<'a, Function<'a, IdRepr>>>,
    pub Vec<WrapSpan<'a, Stat<'a, IdRepr>>>,
);
