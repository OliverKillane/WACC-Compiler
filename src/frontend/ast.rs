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

/// Statements
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
use std::fmt::Debug;
use std::rc::Rc;

/// Symbol Table data structure linked to the parent symbol tables for parent
/// scopes.
pub struct SymbolTable<'a, T> {
    scope: HashMap<&'a str, T>,
    parent: Option<Rc<SymbolTable<'a, T>>>,
}

/// Create new empty symbol table with no parent scopes.
/// ```
/// // An example source string
/// let a = "int var = 7; char b = 'a'";
/// let mut symbols = SymbolTable::<Type>::new();
///
/// // Can successfully add to symbol table
/// assert_eq!(symbols.add(&a[4..7], Type::Int), None);
///
/// // Second addition returns old value
/// assert_eq!(symbols.add("var", Type::Bool), Some(Type::Int));
///
/// // New symbol addition is successful
/// assert_eq!(symbols.add(&a[18..19], Type::Char), None);
///
/// // Can find symbols in synbol table
/// assert_eq!(symbols.find("var"), Some(&Type::Bool));
/// assert_eq!(symbols.find("b"), Some(&Type::Char));
/// ```
impl<'a, T> SymbolTable<'a, T> {
    /// Create a new empty symbol table with no parent scopes.
    fn new() -> SymbolTable<'a, T> {
        SymbolTable {
            scope: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new empty symbol table with a parent scope.
    fn new_child(parent: Rc<SymbolTable<'a, T>>) -> SymbolTable<'a, T> {
        SymbolTable::<'a, T> {
            scope: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Add a new symbol and value to the symbol table. If the symbol is not
    /// yet in the current scope, return none. Else the typedata of the token
    /// in the table is returned.
    /// ```
    /// let mut symbols = SymbolTable::<Type>::new();
    /// assert_eq!(symbols.add("variable1", Type::Char), None);
    /// assert_eq!(symbols.add("variable1", Type::Int), Some(Type::Char));
    /// ```
    fn add(&mut self, ident: &'a str, typedata: T) -> Option<T> {
        self.scope.insert(ident, typedata)
    }

    /// Search symbol table for an identifier, including all parent symbol
    /// tables/scopes.
    /// ```
    /// let mut parent = Rc::new(SymbolTable::<Type>::new());
    /// let mut child = SymbolTable::<Type>::new_child(parent);
    /// assert_eq!(parent.add("foo", Type::Bool), None);
    /// assert_eq!(child.find("foo"), Some(&Type::Bool));
    /// ```
    fn find(&self, ident: &'a str) -> Option<&T> {
        match self.scope.get(ident) {
            Some(a) => Some(a),
            None => match &self.parent {
                Some(p) => p.find(ident),
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_scope_is_empty() {
        let parent = Rc::new(SymbolTable::<Type>::new());
        assert!(parent.scope.is_empty());

        let child = SymbolTable::<Type>::new_child(parent);
        assert!(child.scope.is_empty())
    }

    #[test]
    fn can_add_to_table() {
        let mut symbols = SymbolTable::<Type>::new();

        assert_eq!(symbols.add("var", Type::Char), None);
        assert_eq!(
            symbols.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_eq!(symbols.add("foo", Type::Int), None);
        assert_eq!(
            symbols.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );

        assert_eq!(symbols.scope.len(), 4);
    }

    #[test]
    fn can_find_in_table() {
        let mut symbols = SymbolTable::<Type>::new();

        assert_eq!(symbols.add("var", Type::Char), None);
        assert_eq!(symbols.find("var"), Some(&Type::Char));

        assert_eq!(
            symbols.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_eq!(
            symbols.find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );

        assert_eq!(symbols.add("foo", Type::Int), None);
        assert_eq!(symbols.find("foo"), Some(&Type::Int));

        assert_eq!(
            symbols.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );
        assert_eq!(
            symbols.find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );

        assert_eq!(symbols.scope.len(), 4);
    }

    #[test]
    fn added_values_replace_old() {
        let mut symbols = SymbolTable::<Type>::new();

        assert_eq!(symbols.add("var", Type::Char), None);
        assert_eq!(symbols.find("var"), Some(&Type::Char));

        assert_eq!(symbols.add("var", Type::Int), Some(Type::Char));
        assert_eq!(symbols.find("var"), Some(&Type::Int));

        assert_eq!(symbols.scope.len(), 1);
    }

    #[test]
    fn child_can_find_in_parents_table() {
        let mut parent = SymbolTable::<Type>::new();

        assert_eq!(parent.add("var", Type::Char), None);
        assert_eq!(parent.add("foo", Type::Int), None);
        assert_eq!(
            parent.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_eq!(
            parent.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );

        let parent_ref = Rc::new(parent);
        let child = SymbolTable::<Type>::new_child(parent_ref.clone());

        assert_eq!(child.find("var"), Some(&Type::Char));
        assert_eq!(
            child.find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );
        assert_eq!(child.find("foo"), Some(&Type::Int));
        assert_eq!(
            child.find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );

        assert_eq!(parent_ref.clone().scope.len(), 4);
        assert!(child.scope.is_empty());
    }

    #[test]
    fn parent_cannot_find_in_parents_table() {
        let parent_ref = Rc::new(SymbolTable::<Type>::new());
        let mut child = SymbolTable::<Type>::new_child(parent_ref.clone());

        assert_eq!(child.add("var", Type::Char), None);
        assert_ne!(parent_ref.clone().find("var"), Some(&Type::Char));

        assert_eq!(
            child.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_ne!(
            parent_ref.clone().find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );

        assert_eq!(child.add("foo", Type::Int), None);
        assert_ne!(parent_ref.clone().find("foo"), Some(&Type::Int));

        assert_eq!(
            child.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );
        assert_ne!(
            parent_ref.clone().find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );

        assert_eq!(child.scope.len(), 4);
        assert!(parent_ref.clone().scope.is_empty());
    }

    #[test]
    fn many_children_can_find_in_parents_table() {
        let mut parent = SymbolTable::<Type>::new();

        assert_eq!(parent.add("var", Type::Char), None);
        assert_eq!(
            parent.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_eq!(parent.add("foo", Type::Int), None);
        assert_eq!(
            parent.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );

        let parent_ref = Rc::new(parent);
        let child_a = SymbolTable::<Type>::new_child(parent_ref.clone());
        let child_b = SymbolTable::<Type>::new_child(parent_ref.clone());

        assert_eq!(child_a.find("var"), Some(&Type::Char));
        assert_eq!(child_b.find("var"), Some(&Type::Char));

        assert_eq!(
            child_a.find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );
        assert_eq!(
            child_b.find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );
        assert_eq!(child_a.find("foo"), Some(&Type::Int));
        assert_eq!(child_b.find("foo"), Some(&Type::Int));
        assert_eq!(
            child_a.find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );
        assert_eq!(
            child_b.find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );

        assert_eq!(parent_ref.clone().scope.len(), 4);
        assert!(child_a.scope.is_empty());
        assert!(child_b.scope.is_empty());
    }

    #[test]
    fn child_scopes_are_isolated() {
        let parent_ref = Rc::new(SymbolTable::<Type>::new());
        let mut child_a = SymbolTable::<Type>::new_child(parent_ref.clone());
        let mut child_b = SymbolTable::<Type>::new_child(parent_ref.clone());

        assert_eq!(child_a.add("var", Type::Char), None);
        assert_ne!(parent_ref.clone().find("var"), Some(&Type::Char));
        assert_ne!(child_b.find("var"), Some(&Type::Char));

        assert_eq!(
            child_a.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_ne!(
            parent_ref.clone().find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );
        assert_ne!(
            child_b.find("abcde"),
            Some(&Type::Array(Box::from(Type::Char), 3))
        );

        assert_eq!(child_a.add("foo", Type::Int), None);
        assert_ne!(parent_ref.clone().find("foo"), Some(&Type::Int));
        assert_ne!(child_b.find("foo"), Some(&Type::Int));

        assert_eq!(
            child_a.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );
        assert_ne!(
            parent_ref.clone().find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );
        assert_ne!(
            child_b.find("var2"),
            Some(&Type::Pair(Box::from(Type::Char), Box::from(Type::Int)))
        );

        assert_eq!(child_b.add("var", Type::Char), None);
        assert_eq!(
            child_b.add("abcde", Type::Array(Box::from(Type::Char), 3)),
            None
        );
        assert_eq!(child_b.add("foo", Type::Int), None);
        assert_eq!(
            child_b.add(
                "var2",
                Type::Pair(Box::from(Type::Char), Box::from(Type::Int))
            ),
            None
        );

        assert_eq!(child_a.scope.len(), 4);
        assert_eq!(child_b.scope.len(), 4);
        assert!(parent_ref.clone().scope.is_empty());
    }
}
