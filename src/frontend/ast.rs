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
//!                     box WrapSpan("9", Expr::Int(9)),
//!                     BinOp::Mul,
//!                     box WrapSpan("(3 + 7)", Expr::BinOp(
//!                         box (WrapSpan("3", Expr::Int(3)),
//!                         BinOp::Add,
//!                         box (WrapSpan("7", Expr::Int(7))
//!                     ))
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
//!             "function_name",
//!             vec![Param("int a", Type::Int, "a")],
//!             vec![Stat(
//!                 "return a + 9",
//!                 StatCode::Return(Expr(
//!                     "a + 9",
//!                     ExprCode::BinOp(
//!                         box (Expr("a", ExprCode::Var("a")),
//!                         BinOp::Add,
//!                         box (Expr("9", ExprCode::Int(9)),
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
//!                 "function_name",
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
#[derive(Clone, Debug, Hash, Eq)]
pub enum Type {
    /// Integer primitive type.
    Int,

    /// Boolean primitive type.
    Bool,

    /// Character primitive type.
    Char,

    /// String primitive type.
    String,

    /// Any type (matches all). Used for type coercion
    /// ```text
    /// pair(pair, pair)
    /// ```
    /// ```
    /// Type::Pair(box Type::Pair(box Type::Any), box Type::Pair(box Type::Any))
    /// ```
    Any,

    /// Used as a single generic type, must match other generic types in its
    /// context.
    Generic,

    /// Typed pair type. The arguments to the nameless tuple are the types of
    /// the first and second field of the pair respectively.
    /// ```text
    /// pair(int, bool)
    /// pair(int[], pair(char, char)[])
    /// ```
    /// ```
    /// // First type:
    /// Type::Pair(box Type::Int, box Type::Bool)
    ///
    /// // Second type:
    /// Type::Pair(
    ///     box Type::Array(box Type::Int, 1),
    ///     box Type::Array(box Type::Pair(box Type::Char, box Type::Char), 1),
    /// )
    /// ```
    Pair(Box<Type>, Box<Type>),

    /// Multidimentional array. The first argument is the type of elements in
    /// the arrays. The second argument is the dimensionality of the array.
    ///
    /// **Note**: For multidimensional arrays, the type is the element type,
    /// and the size is the number of dimensions.
    /// ```text
    /// int[][][]
    /// ```
    /// ```
    /// Type::Array(box Type::Int, 3)
    /// ```
    Array(Box<Type>, usize),
}

/// The semantics of type checking are encoded through implementing equality.
/// - primitive types match eachother
/// - Any type matches anything.
/// - The generic type matches itself, it is used by the semantic analyser,
///   when propagated as an expected type it is converted to an Any. It is used
///   to check types for '==' where both sides are an "Any" but must be the
///   same.
/// - Array Types match taking into account nesting of array types
///   (e.g Array(Array(int),1),1) == Array(int, 2) )
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::String, Type::String)
            | (Type::Generic, Type::Generic) => true,
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            (Type::Pair(box a1, box a2), Type::Pair(box b1, box b2)) => a1 == b1 && a2 == b2,
            (Type::Array(box a, dim_a), Type::Array(box b, dim_b)) => {
                if dim_a == dim_b {
                    a == b
                } else if dim_a > dim_b {
                    &Type::Array(box a.clone(), dim_a - dim_b) == b
                } else {
                    a == &Type::Array(box b.clone(), dim_b - dim_a)
                }
            }
            _ => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
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
///     box (WrapSpan("4 - 3", Expr=::BinOp(
///         box (WrapSpan("4", Expr::Int(4)),
///         BinOp::Sub,
///         box (WrapSpan("3", Expr::Int(3))
///     )),
///     BinOp::Add,
///     box WrapSpan("9 * 7", Expr::BinOp(
///         box WrapSpan("9", Expr::Int(9)),
///         BinOp::Mul,
///         box WrapSpan("7", Expr::Int(7))
///     )),
/// ))
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr<'a, VarIdRepr> {
    /// null value that can be used for pair values.
    /// ```text
    /// pair(int, int) example = null
    /// ```
    /// ```
    /// WrapSpan(
    ///     "pair(int, int) example = null",
    ///     Stat::Def(
    ///         Type::Pair(box Type::Int, box Type::Int),
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
    Var(VarIdRepr),

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
    ArrayElem(VarIdRepr, Vec<WrapSpan<'a, Expr<'a, VarIdRepr>>>),

    /// Unary operator application determined by the [UnOp enum](UnOp).
    /// ```text
    /// bool a = !true ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "bool a = true",
    ///     Stat::Def(
    ///         Type::Bool,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "true",
    ///             Expr::UnOp(UnOp::Neg, box WrapSpan("true", Expr::Bool(true))),
    ///         )),
    ///     ),
    /// )
    /// ``
    UnOp(UnOp, Box<WrapSpan<'a, Expr<'a, VarIdRepr>>>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    /// ```text
    /// int a = 3 + 7 ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = 3 + 7",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "3 + 7",
    ///             Expr::BinOp(
    ///                 box WrapSpan("3", Expr::Int(3)),
    ///                 BinOp::Add,
    ///                 box WrapSpan("7", Expr::Int(7)),
    ///             ),
    ///         )),
    ///     ),
    /// )
    /// ```
    BinOp(
        Box<WrapSpan<'a, Expr<'a, VarIdRepr>>>,
        BinOp,
        Box<WrapSpan<'a, Expr<'a, VarIdRepr>>>,
    ),
}

/// Lefthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum AssignLhs<'a, VarIdRepr> {
    /// Variable assignment.
    /// ```text
    /// int a = 3 ;
    /// ```
    Var(VarIdRepr),

    /// Array element assignment.
    /// ```text
    /// int[] a = [1,2,3,4] ;
    /// a[0] = 9 ;
    /// ```
    ArrayElem(VarIdRepr, Vec<WrapSpan<'a, Expr<'a, VarIdRepr>>>),

    /// Assign to the first element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// fst a = 3 ;
    /// ```
    PairFst(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Assign to the second element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// snd a = false ;
    /// ```
    PairSnd(WrapSpan<'a, Expr<'a, VarIdRepr>>),
}

/// Righthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum AssignRhs<'a, FunIdRepr, VarIdRepr> {
    /// Assigns an expression.
    /// ```text
    /// int a = 3 + (4 * 5) ;
    /// ```
    Expr(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Array assignment by expressions of the same type.
    /// ```text
    /// int[] int_arr = [2, 3 + 3, 4 * 7, 0] ;
    /// ```
    Array(Vec<WrapSpan<'a, Expr<'a, VarIdRepr>>>),

    /// Assigns to a new pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(3 + 3, true && false) ;
    /// ```
    NewPair(
        WrapSpan<'a, Expr<'a, VarIdRepr>>,
        WrapSpan<'a, Expr<'a, VarIdRepr>>,
    ),

    /// Assign the first element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// int a_fst = fst a_pair ;
    /// ```
    PairFst(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Assign the second element of a given pair.
    /// ```text
    /// pair(int, bool) a_pair = newpair(1, true) ;
    /// bool a_snd = snd a_pair ;
    /// ```
    PairSnd(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Assign the return value of a function, with arguments.
    /// ```text
    /// int add(int a, int b) is
    ///     return a + b
    /// end
    ///
    /// int a = add(3,4) ;
    /// ```
    Call(FunIdRepr, Vec<WrapSpan<'a, Expr<'a, VarIdRepr>>>),
}

/// Statements for assignment, control flow and definitions.
#[derive(Debug, PartialEq, Eq)]
pub enum Stat<'a, FunIdRepr, VarIdRepr> {
    /// A _no-operation_ instruction.
    Skip,

    /// Variable Definition
    /// ```text
    /// int a = 99;
    /// ```
    Def(Type, VarIdRepr, AssignRhs<'a, FunIdRepr, VarIdRepr>),

    /// Assignment (to variable, parts of pairs or arrays).
    /// ```text
    /// int a = 9 ;
    /// ```
    /// ```text
    /// int[] arr = [1,2,3,4] ;
    /// a[1] = call function(3, 4) ;
    /// ```
    Assign(
        AssignLhs<'a, VarIdRepr>,
        AssignRhs<'a, FunIdRepr, VarIdRepr>,
    ),

    /// Read from standard input.
    /// ```text
    /// int a = 9 ;
    /// read a;
    /// ```
    Read(AssignLhs<'a, VarIdRepr>),

    /// Free dynamically allocated data structures.
    /// ```text
    /// pair(int, int) a_pair = newpair(3,3) ;
    /// free a_pair ;
    /// ```
    Free(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Return value from a subroutine/function.
    /// ```text
    /// int double(int b) is
    ///     return 2 * b
    /// end
    /// ```
    Return(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// End the program and return the exit code.
    /// ```text
    /// exit 0 ;
    /// ```
    Exit(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Print text to the console.
    /// ```text
    /// int a = 'a' ;
    /// print a ;
    /// ```
    Print(WrapSpan<'a, Expr<'a, VarIdRepr>>),

    /// Print text to the console and start a new line.
    /// ```text
    /// int a = 'a' ;
    /// print a + 9 ;
    /// ```
    PrintLn(WrapSpan<'a, Expr<'a, VarIdRepr>>),

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
        WrapSpan<'a, Expr<'a, VarIdRepr>>,
        Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>,
        Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>,
    ),

    /// While Loop control flow structure.
    /// ```text
    /// int n = 0 ;
    /// while n < 10 do
    ///     n = n + 1
    /// done ;
    /// ```
    While(
        WrapSpan<'a, Expr<'a, VarIdRepr>>,
        Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>,
    ),

    Block(Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>),
}

/// Formal parameter used in functions, containing the type and identifier.
#[derive(Debug, PartialEq, Eq)]
pub struct Param<VarIdRepr>(pub Type, pub VarIdRepr);

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
///         "sum",
///         vec![WrapSpan("int a", Param(Type::Int, "a")),
///         WrapSpan("int b", Param(Type::Int, "b")),],
///         vec![
///             WrapSpan("return a + b", Stat::Return(WrapSpan("a + b", Expr::BinOp(
///                 box WrapSpan("a", Expr::Var("a")),
///                 BinOp::Add,
///                 box (WrapSpan("b", Expr::Var("b")),
///             ))))
///         ]
///     )
/// )
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct Function<'a, FunIdRepr, VarIdRepr>(
    pub Type,
    pub FunIdRepr,
    pub Vec<WrapSpan<'a, Param<VarIdRepr>>>,
    pub Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>,
);

/// Program is the root of the abstract syntax tree, containing all function
/// definitions and the main program body, with all nested structures being
/// associated with the original source code by [wrappers](WrapSpan).
pub struct Program<'a, FunIdRepr, VarIdRepr>(
    pub Vec<WrapSpan<'a, Function<'a, FunIdRepr, VarIdRepr>>>,
    pub Vec<WrapSpan<'a, Stat<'a, FunIdRepr, VarIdRepr>>>,
);

#[cfg(test)]
mod test {
    use super::*;

    // Tests for the implementation of equality on types
    #[test]
    fn type_eq_primitive_types_are_equal() {
        assert_eq!(Type::Int, Type::Int);
        assert_eq!(Type::Char, Type::Char);
        assert_eq!(Type::String, Type::String);
        assert_eq!(Type::Bool, Type::Bool);
    }

    #[test]
    fn type_eq_any_type_matches_anything() {
        assert_eq!(Type::Any, Type::Int);
        assert_eq!(Type::Int, Type::Any,);
        assert_eq!(Type::Any, Type::Char);
        assert_eq!(Type::Char, Type::Any,);
        assert_eq!(Type::Any, Type::Bool);
        assert_eq!(Type::Bool, Type::Any,);
        assert_eq!(Type::Any, Type::String);
        assert_eq!(Type::String, Type::Any,);
        assert_eq!(Type::Any, Type::Pair(box Type::Any, box Type::Any));
        assert_eq!(Type::Pair(box Type::Any, box Type::Any), Type::Any);
        assert_eq!(
            Type::Any,
            Type::Array(box Type::Pair(box Type::Int, box Type::Char), 4)
        );
        assert_eq!(
            Type::Array(box Type::Pair(box Type::Int, box Type::Char), 4),
            Type::Any
        );
        assert_eq!(Type::Any, Type::Generic);
        assert_eq!(Type::Generic, Type::Any,);
        assert_eq!(Type::Any, Type::Array(box Type::Int, 1));
        assert_eq!(Type::Array(box Type::Int, 1), Type::Any);
        assert_eq!(Type::Any, Type::Any);
    }

    #[test]
    fn type_eq_generic_type_matches_itself() {
        assert_eq!(Type::Generic, Type::Generic);
    }

    #[test]
    fn type_eq_differently_nested_array_types_are_equal() {
        assert_eq!(
            Type::Array(box Type::Array(box Type::Int, 2), 3),
            Type::Array(box Type::Int, 5)
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Any, 2), 3),
            Type::Array(box Type::Int, 5)
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Any, 2), 3),
            Type::Array(box Type::Array(box Type::Int, 2), 3)
        );
        assert_eq!(
            Type::Array(box Type::Any, 1),
            Type::Array(box Type::Pair(box Type::Generic, box Type::Int), 3)
        );
        assert_eq!(
            Type::Array(box Type::Char, 3),
            Type::Array(box Type::Array(box Type::Array(box Type::Char, 1), 1), 1)
        )
    }

    #[test]
    fn type_eq_differentiates_between_primitive_and_generic_types() {
        assert_ne!(Type::Int, Type::Bool);
        assert_ne!(Type::Int, Type::Char);
        assert_ne!(Type::Int, Type::String);
        assert_ne!(Type::Int, Type::Generic);
        assert_ne!(Type::Bool, Type::Int);
        assert_ne!(Type::Bool, Type::Char);
        assert_ne!(Type::Bool, Type::String);
        assert_ne!(Type::Bool, Type::Generic);
        assert_ne!(Type::Char, Type::Int);
        assert_ne!(Type::Char, Type::Bool);
        assert_ne!(Type::Char, Type::String);
        assert_ne!(Type::Char, Type::Generic);
        assert_ne!(Type::String, Type::Int);
        assert_ne!(Type::String, Type::Bool);
        assert_ne!(Type::String, Type::Char);
        assert_ne!(Type::String, Type::Generic);
        assert_ne!(Type::Generic, Type::Int);
        assert_ne!(Type::Generic, Type::Bool);
        assert_ne!(Type::Generic, Type::Char);
        assert_ne!(Type::Generic, Type::String);
    }
}
