//! # Semantic Analysis and Symbol Table Generation
//! This module takes the provided AST and returns:
//! - Flat Symbol table of variables (identified by usize number)
//! - Symbol table of functions (string identifiers maintained)
//! - AST with renamed variable symbols
//!
//! ## Error Results
//! Error printing is handled by the errors module, this module provides it
//! type, error and location (through string references) information.
//!
//! ## Expression type checking
//! Given an expression, we can check it by providing the allowed types. It will
//! then use the allowed types, types for operators to find any expression errors.
//!
//! As the expression is traversed, more restrictions on types are imposed by the
//! operators used, and the operator used can determine a subexpression's
//! correctness immediately.

use std::collections::HashMap;

use super::ast::{BinOp, Expr, Function, Program, Stat, Type, UnOp, WrapSpan};

type FunctionSymbolTable = HashMap<String, (Type, Vec<Type>)>;
type VariableSymbolTable = HashMap<usize, Type>;

/// A chainable local symbol table, used to translate the current (and parent)
/// scope variables to integers for use in the flat variable symbol table and the definition of the variable.
struct LocalSymbolTable<'a, 'b> {
    current: HashMap<&'a str, (usize, &'a str)>,
    parent: Option<&'b LocalSymbolTable<'a, 'b>>,
}

impl<'a, 'b> LocalSymbolTable<'a, 'b> {
    /// Create a new root scope (no parent)
    fn new_root() -> Self {
        LocalSymbolTable {
            current: HashMap::new(),
            parent: None,
        }
    }

    /// Create new child scope local symbol table
    fn new_child(parent: &'b LocalSymbolTable<'a, 'b>) -> Self {
        LocalSymbolTable {
            current: HashMap::new(),
            parent: Some(&parent),
        }
    }

    /// Checks if a variable is in the current scope level. If one is, then
    /// the span of the definition of the first is returned.
    fn in_scope(&self, ident: &'a str) -> Option<&'a str> {
        match self.current.get(ident) {
            Some((_, def)) => Some(def),
            None => None,
        }
    }

    /// Add a variable to the current scope. Assumes the variable is not already
    /// in the current scope, and panics in that case.
    ///
    /// Should only be called once it has been established the variable is not
    /// yet defined.
    fn add_var(&mut self, ident: &'a str, id: usize, span: &'a str) {
        if let Some(_) = self.current.insert(ident, (id, span)) {
            panic!("Repeat add of variable to local symbol table.")
        }
    }

    /// Get the renamed identifier and definition span of a variable.
    /// If the variable is undefined None is returned.
    fn get_var(&self, ident: &'a str) -> Option<(usize, &'a str)> {
        let mut symb = self;
        loop {
            if let Some((id, span)) = symb.current.get(ident) {
                break Some((*id, span));
            }

            match symb.parent {
                Some(parent) => symb = parent,
                None => break None,
            }
        }
    }
}

/// Holds the type constraints for an expression (concrete types, pointer type).
/// ```text
/// int a = <exp>
/// ```
/// ```
/// TypeConstraint(vec![Type::Int], false)
/// ```
/// ```text
/// free <exp>
/// ```
/// ```
/// TypeConstraint(vec![], true)
/// ```
/// ```text
/// println <exp>
/// ```
/// ```
/// TypeConstraint(vec![Type::Int, Type::Char, Type::String, Type::Bool], true)
/// ```
#[derive(Debug, PartialEq, Eq)]
pub struct TypeConstraint(Vec<Type>, bool);

/// Contains a single error within an expression.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprErrType<'a> {
    /// Invalid type, including when no type inference is possible.
    /// ```text
    /// bool a = 9 + 9
    /// ```
    /// ```
    /// ExpressionError::InvalidType("9 + 9", TypeConstraint(vec![Type::Bool], false), Some(Type::Int))
    /// ```
    /// ```text
    /// int a = 'a' || 15
    /// ```
    /// ```
    /// ExpressionError::InvalidType("'a' || 15", TypeConstraint(vec![Type::Int], false), None)
    /// ```
    InvalidType(&'a str, TypeConstraint, Option<Type>),

    /// Invalid Binary Operator, using a different operator the expression could be valid.
    /// - int a = <int> || && <= < > >= == != <int>
    /// - bool a = <bool> + / % * <bool>
    /// ```text
    /// int a = 9 || 8
    /// ```
    /// ```
    /// ExpressionError::BinOp("||", vec![BinOp::Add, BinOp::Mul, BinOp::Mod, BinOp::Div, BinOp::Sub], BinOp::Or)
    /// ```
    BinOp(&'a str, Vec<BinOp>, BinOp),

    /// Invalid Unary Operator, with different operator, the expression is valid
    /// ```text
    /// bool a = -true
    /// ```
    /// ```
    /// ExpressionError::UnOp("-", vec![UnOp::Neg], UnOp::Minus)
    /// ```
    UnOp(&'a str, Vec<UnOp>, UnOp),

    /// Use of undefined variable.
    /// ```text
    /// int a = 3 * b
    /// ```
    /// ```
    /// ExpressionError::UndefinedVar("b")
    /// ```
    UndefinedVar(&'a str),
}

/// Determines the expression error wrapping of an expression.
#[derive(Debug, PartialEq, Eq)]
pub enum ExpressionError<'a> {
    /// When the entire expression is erroneous.
    /// ```text
    /// int a = bool
    /// ```
    /// ```
    /// ExpressionError::Is(ExprErrType::InvalidType("bool", TypeConstraint(vec![Type::Int], false), Some(Type::Bool)))
    /// ```
    /// ```text
    /// char a = - true
    /// ```
    /// ```
    /// ExpressionError::Is(ExprErrType::InvalidType("- true", TypeConstraint(vec![Type::Char], false), None))
    /// ```
    Is(ExprErrType<'a>),

    /// The expression contains some errors, not all of the expression is erroneous
    /// ```text
    /// int a = (1 + true) + (3 * 'a')
    /// ```
    /// ```
    /// ExpressionError::Contains(vec![
    ///     ExprErrType::InvalidType("true", TypeConstraint(vec![Type::Int], false), Some(Type::Bool)),
    ///     ExprErrType::InvalidType("'a'", TypeConstraint(vec![Type::Int], false), Some(Type::Char)),
    /// ])
    /// ```
    Contains(Vec<ExprErrType<'a>>),
}

pub struct SemanticErrorSummary<'a>(Vec<WrapSpan<'a, SemanticError<'a>>>);

/// Semantic errors and their types.
/// Each variant corresponds to an error for which we want a distinct error message.
///
/// Many errors are the result of expression type mismatches, by having variants
/// to cover multiple instances of this (e.g return, function arguments, etc)
/// more detailed error messages can be produced.
#[derive(PartialEq, Eq, Debug)]
pub enum SemanticError<'a> {
    /// Function used is undefined.
    /// (function name - reference to source code)
    /// ```text
    /// begin
    ///     int a = call undefined_function(4)
    /// end
    /// ```
    /// ```
    /// UndefinedFunction("undefined_function")
    /// ```
    UndefinedFunction(&'a str),

    /// Variable assigned without definition.
    /// (Variable name - reference to source code)
    /// ```text
    /// begin
    ///     a = 3
    /// end
    /// ```
    /// ```
    /// UndefinedVariableAssignment("a")
    /// ```
    UndefinedVariableAssignment(&'a str),

    /// Variable and assigned type do not match.
    /// (Variable name, span of variable assignment, expected type, found type)
    /// ```text
    /// begin
    ///     int a = true
    ///     int b = (1 + true) * (1 + (true || false))
    /// end
    /// ```
    /// ```
    /// AssignmentMismatch("a", vec![ExpressionError("true", Type::Int, Type::Bool)])
    /// AssignmentMismatch("b",
    ///     vec![
    ///         ExpressionError("bool", Type::Int, Type::Bool),
    ///         ExpressionError("true || false", Type::Int, Type::Bool)
    ///     ]
    /// )
    /// ```
    AssignmentMismatch(&'a str, ExpressionError<'a>),

    /// Function of same name is defined twice
    /// (Function name, span of original definition)
    /// ```text
    /// begin
    ///     int function(int a) is
    ///         return 3
    ///     end
    ///
    ///     int function(int b, bool c) is
    ///         return 3
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// RepeatDefinitionFunction("function", "int function(int a = 3)")
    /// ```
    RepeatDefinitionFunction(&'a str, &'a str),

    /// Variable redefined in the same scope.
    /// (Variable name, original definition)
    /// ```text
    /// begin
    ///     int a = 3 ;
    ///     bool a = true
    /// end
    /// ```
    /// ```
    /// RepeatDefinitionVariable("a", "int a = 3")
    /// ```
    RepeatDefinitionVariable(&'a str, &'a str),

    /// Function argument types do not match parameters.
    /// (Function name, [ (param name, expression problems) ] )
    /// ```text
    /// begin
    ///     int function(int a, char b) is
    ///         return 3
    ///     end
    ///
    ///     int a = call function(true, true)
    /// end
    /// ```
    /// ```
    /// FunctionParametersMismatch("function", vec![Type::Int, Type::Char], vec![Type::Bool, Type::Bool])
    /// ```
    FunctionParametersMismatch(&'a str, Vec<(&'a str, ExpressionError<'a>)>),

    /// Function return does not match function return type.
    /// (Function name, expected type, found type)
    /// ```text
    /// begin
    ///     bool function(int a) is
    ///         return a
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// FunctionReturnMismatch("function", Type::Bool, Type::Int)
    /// ```
    FunctionReturnMismatch(&'a str, ExpressionError<'a>),

    /// Function has a control flow that does not end in return or exit.
    /// (Function name)
    /// ```text
    /// begin
    ///     bool function(int a) is
    ///         int i = 0 ;
    ///         print i
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// FunctionNoReturnOrExit("function")
    /// ```
    /// or
    /// ```text
    /// begin
    ///     bool function(int a) is
    ///         if ( b ) then
    ///             return b
    ///         else
    ///             skip
    ///         fi
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// FunctionNoReturnOrExit("function")
    /// ```
    FunctionNoReturnOrExit(&'a str),

    /// Function name is a reserved keyword
    /// (Function name)
    /// ```text
    /// begin
    ///     bool begin(int a) is
    ///         return a
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// KeywordFunctionName("begin")
    /// ```
    KeywordFunctionName(&'a str),

    /// Parameter name is a reserved keyword
    /// (Function name, Parameter name)
    /// ```text
    /// begin
    ///     bool function(int true) is
    ///         return a
    ///     end
    ///
    ///     skip
    /// end
    /// ```
    /// ```
    /// KeywordParameterName("function", "true")
    /// ```
    KeywordParameterName(&'a str, &'a str),

    /// Variable name is a reserved keyword
    /// (Variable name)
    /// ```text
    /// begin
    ///     int begin = 4
    /// end
    /// ```
    /// ```
    /// KeywordVariableName("begin")
    /// ```
    KeywordVariableName(&'a str),

    /// Read statement invalid (wrong type)
    /// (variable/lvalue identifier, type found)
    /// ```text
    /// begin
    ///     int[] a = [9,3,4] ;
    ///     read a
    /// end
    /// ```
    /// ```
    /// ReadStatementMismatch("a", Type::Array(box Type::Int, 1))
    /// ```
    ReadStatementMismatch(&'a str, Type),

    /// Free statement invalid (wrong type)
    /// (span of expression, expression errors found)
    /// ```text
    /// begin
    ///     int a = 4 ;
    ///     free a
    /// end
    /// ```
    /// ```
    /// FreeStatementMismatch("a", vec![ExpressionError("a", )])
    /// ```
    FreeStatementMismatch(&'a str, ExpressionError<'a>),

    /// Exit Statement Invalid (wrong type)
    /// (span of expression, type found)
    /// ```text
    /// begin
    ///     exit (true || false)
    /// end
    /// ```
    /// ```
    /// ExitStatementMismatch("")
    /// ```
    ExitStatementMismatch(&'a str, ExpressionError<'a>),

    /// Print/Println Statement invalid (wrong type).
    /// Currently all types can be printed, this is here for extensibility.
    /// (span of print/println statement, type found)
    PrintStatementMisMatch(&'a str, ExpressionError<'a>),

    /// Return statement outside of a function
    /// Only occurs in the main code block.
    /// (span of return statement)
    /// ```text
    /// begin
    ///     return 3
    /// end
    /// ```
    ReturnStatementMisplaced,
}

const KEYWORDS: [&'static str; 32] = [
    "begin", "end", "is", "skip", "read", "free", "return", "exit", "print", "println", "if",
    "then", "else", "fi", "while", "do", "done", "newpair", "call", "fst", "snd", "int", "bool",
    "char", "string", "pair", "len", "ord", "chr", "true", "false", "null",
];

/// Gets the current symbol table and returns:
/// - Symbol table of function identifier to function type
/// - Vector of functions with correct declarations (no repeated/keyword name/s
///   for function or parameters), these functions can be further analysed.
/// - Vector of errors from invalid functions.
fn get_fn_symbols<'a>(
    fn_defs: Vec<Function<&'a str, &'a str>>,
) -> (
    FunctionSymbolTable,
    Vec<Function<'a, String, &'a str>>,
    SemanticErrorSummary<'a>,
) {
    todo!()
}

fn analyse_scope<'a>(
    scope: Vec<WrapSpan<'a, Stat<'a, String, &'a str>>>,
    local_symb: &LocalSymbolTable,
    var_symb: &mut VariableSymbolTable,
    fun_symb: &FunctionSymbolTable,
    term: bool,
) -> (
    Vec<WrapSpan<'a, Stat<'a, String, usize>>>,
    SemanticErrorSummary<'a>,
    bool,
) {
    todo!()
}

fn analyse_expr<'a>(
    expr: WrapSpan<'a, Expr<'a, &'a str>>,
    cons: TypeConstraint,
    local_symb: &LocalSymbolTable,
    var_symb: &mut VariableSymbolTable,
) -> SemanticErrorSummary<'a> {
    todo!()
}

pub fn semantic_analysis<'a>(
    prog: Program<'a, &'a str, &'a str>,
) -> Result<
    (
        FunctionSymbolTable,
        VariableSymbolTable,
        Program<'a, String, usize>,
    ),
    SemanticErrorSummary<'a>,
> {
    todo!()
}

/*
  Todo:
  1. Remove references to ExpressionError (the old version)
  2. Wrap local and variable table and impl add and find
  3. comment the function prototypes just above ^^
  4. analyse_expr
  5. get_fn_symbols
  6. analyse_scope
  7. semantic_analysis
*/

#[cfg(test)]
mod tests {
    use super::*;

    // Local Symbol Table Tests
    #[test]
    fn local_symb_can_add_and_find_vars_in_current_scope() {
        let mut symb_tab = LocalSymbolTable::new_root();

        symb_tab.add_var("var1", 0, "int var1 = 9");
        symb_tab.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_tab.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_tab.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_tab.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_tab.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_tab.current.len(), 2);
    }

    #[test]
    fn local_symb_can_add_variables_to_child() {
        let symb_parent = LocalSymbolTable::new_root();

        let mut symb_child = LocalSymbolTable::new_child(&symb_parent);

        symb_child.add_var("var2", 1, "bool var2 = true");
        symb_child.add_var("var1", 0, "int var1 = 9");

        assert_eq!(symb_child.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_child.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_child.current.len(), 2);
        assert_eq!(symb_parent.current.len(), 0);
    }

    #[test]
    fn local_symb_can_find_variables_in_parent() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_parent.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_parent.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_parent.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_parent.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_parent.current.len(), 2);

        let mut symb_child = LocalSymbolTable::new_child(&symb_parent);

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));
    }

    #[test]
    fn local_symb_variables_can_shadow() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_parent.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_parent.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_parent.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_parent.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_parent.current.len(), 2);

        let mut symb_child = LocalSymbolTable {
            current: HashMap::new(),
            parent: Some(&symb_parent),
        };

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));

        symb_child.add_var("var1", 2, "char var2 = 'a'");
        symb_child.add_var("var2", 3, "string var1 = \"hello\"");

        assert_eq!(symb_child.get_var("var1"), Some((2, "char var2 = 'a'")));
        assert_eq!(
            symb_child.get_var("var2"),
            Some((3, "string var1 = \"hello\""))
        );
    }

    #[test]
    fn local_symb_sibling_scopes_disjoint() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        {
            let mut symb_child1 = LocalSymbolTable::new_child(&symb_parent);

            assert_eq!(symb_child1.get_var("var1"), Some((0, "int var1 = 9")));
            assert_eq!(symb_child1.get_var("var2"), Some((1, "bool var2 = true")));
            assert_eq!(symb_child1.get_var("var3"), None);

            assert_eq!(symb_parent.current.len(), 2);

            symb_child1.add_var("var3", 2, "char var3 = 'b'");

            assert_eq!(symb_child1.get_var("var3"), Some((2, "char var3 = 'b'")));
        }

        {
            let symb_child2 = LocalSymbolTable::new_child(&symb_parent);

            assert_eq!(symb_child2.get_var("var1"), Some((0, "int var1 = 9")));
            assert_eq!(symb_child2.get_var("var2"), Some((1, "bool var2 = true")));

            assert_eq!(symb_parent.current.len(), 2);

            assert_eq!(symb_child2.get_var("var3"), None);
        }
    }

    #[test]
    #[should_panic]
    fn local_symb_fails_to_redefine_in_same_scope() {
        let mut symb_tab = LocalSymbolTable::new_root();

        symb_tab.add_var("var1", 0, "int var1 = 9");

        symb_tab.add_var("var1", 1, "bool var2 = true");
    }
}
