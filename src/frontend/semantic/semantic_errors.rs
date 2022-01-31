//! Semantic errors account for expression (operator and type mismatches), as
//! well as control flow (require termination and return), variable and function
//! definition (and more).
//!
//! The semantic analyser can find multiple semantic errors, including multiple
//! in a single statement or expression.

use crate::frontend::ast::*;
use crate::frontend::semantic::type_constraints::*;

/// Contains all errors which can occur in expressions
#[derive(Debug, PartialEq, Eq)]
pub enum ExprError<'a> {
    /// An undefined variable is used.
    /// (span of variable use)
    UndefinedVariable(&'a str),

    /// Invalid Index (A non-integer was used as an index)
    /// (span of indexing expression)
    InvalidIndex(&'a str),

    /// A variable was used that was not the correct type
    /// (span of variable, possible types, found type)
    InvalidVariableType(&'a str, Vec<Type>, Type),

    /// A binary operator is applied incorrectly
    /// (span of whole binary application, possible input types, possible operators, found types, found operator)
    InvalidBinOp(&'a str, Vec<(Type, Type)>, Vec<BinOp>, (Type, Type), BinOp),

    /// Left of a binary operator is incorrect
    /// (span of left, possible types, possible binops, found type, found binop)
    InvalidBinOpLeft(&'a str, Vec<Type>, Vec<BinOp>, Type, BinOp),

    /// Left of a binary operator is incorrect
    /// (span of right, possible types, possible binops, found type, found binop)
    InvalidBinOpRight(&'a str, Vec<Type>, Vec<BinOp>, Type, BinOp),

    /// Invalid unary operator application
    /// (span of application, possible types, possible unops, found type, found unop)
    InvalidUnOp(&'a str, Vec<Type>, Vec<BinOp>, Type, UnOp),
}

/// Semantic errors with their spans
pub type SemanticErrorSummary<'a> = Vec<WrapSpan<'a, SemanticError<'a>>>;

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

    /// Variable assigned incorrectly, either undefined assignment or
    /// expression errors.
    /// (Variable name - reference to source code)
    /// ```text
    /// begin
    ///     a = 3
    /// end
    /// ```
    /// ```
    /// UndefinedVariableAssignment(Some("a"), vec![])
    /// ```
    /// ```text
    /// begin
    ///     a = 3 + true
    /// end
    /// ```
    /// ```
    /// UndefinedVariableAssignment(Some("a"), vec![ExprError::InvalidBinOpRight("true", vec![Type::Int], vec![], Type::Bool, BinOp::Add)])
    /// ```
    InvalidVariableAssignment(Option<&'a str>, Vec<ExprError<'a>>),

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

    /// The number of arguments provided is too small or large
    /// (function name, expected length, found length)
    /// ```text
    /// begin
    ///     int fun1(int a, int b) is
    ///         return 9
    ///     end
    ///     
    ///     int a = call fun1(3)
    /// end
    /// ```
    /// ```
    /// FunctionParametersLengthMismatch("fun1", 2, 1)
    /// ```
    FunctionParametersLengthMismatch(&'a str, usize, usize),

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
    InvalidFunctionArguments(&'a str, Vec<(&'a str, Vec<ExprError<'a>>)>),

    /// Function return does not match function return type.
    /// (Function name, Result(type, expression's errors), expected type)
    InvalidFunctionReturn(&'a str, Result<Type, Vec<ExprError<'a>>>, Type),

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

    /// Read statement invalid (wrong type)
    /// (variable/lvalue identifier, Result(type found, expression had errors))
    /// ```text
    /// begin
    ///     int[] a = [9,3,4] ;
    ///     read a
    /// end
    /// ```
    ReadStatementMismatch(&'a str, Result<Type, Vec<ExprError<'a>>>),

    /// Free statement invalid (wrong type)
    /// (span of expression, Result(type found, expression had errors))
    /// ```text
    /// begin
    ///     int a = 4 ;
    ///     free a
    /// end
    /// ```
    FreeStatementMismatch(&'a str, Result<Type, Vec<ExprError<'a>>>),

    /// Exit Statement Invalid (wrong type)
    /// (span of expression, type found)
    /// ```text
    /// begin
    ///     exit (true || false)
    /// end
    /// ```
    ExitStatementMismatch(&'a str, Result<Type, Vec<ExprError<'a>>>),

    /// Print/Println Statement invalid (wrong type).
    /// Currently all types can be printed, this is here for extensibility.
    /// (span of print/println statement, type found)
    PrintStatementMisMatch(&'a str, Result<Type, Vec<ExprError<'a>>>),

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
