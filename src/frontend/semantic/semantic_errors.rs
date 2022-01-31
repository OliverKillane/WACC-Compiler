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
pub enum SemanticError<'a> {
    /// An undefined variable is used.
    /// (span of variable in assignment)
    UndefinedVariableAssignment(&'a str),

    /// An undefined variable is used.
    /// (span of variable use)
    UndefinedVariableUse(&'a str),

    /// Invalid Index (A non-integer was used as an index)
    /// (span of indexing expression)
    InvalidIndex(&'a str),

    /// A variable was used that was not the correct type
    /// (span of variable, possible types, found type)
    InvalidVariableType(&'a str, Vec<Type>, Type),

    /// A binary operator is applied incorrectly
    /// (span of left, span of op, span of right, possible input types, possible operators, found types, found operator)
    InvalidBinOp(
        &'a str,
        &'a str,
        &'a str,
        Vec<(Type, Type)>,
        Vec<&'static BinOp>,
        (Type, Type),
        BinOp,
    ),

    /// Invalid unary operator application
    /// (span of operator, span inner expr, possible types, possible unops, found type, found unop)
    InvalidUnOp(&'a str, &'a str, Vec<Type>, Vec<&'static UnOp>, Type, UnOp),

    /// Variable redefined in the same scope.
    /// (Variable name, original definition)
    RepeatDefinitionVariable(&'a str, &'a str),

    /// An undefined function was called
    /// (span of function name in call)
    UndefinedFunction(&'a str),

    /// Function of same name is defined twice
    /// (Function name, span of original definition)
    RepeatDefinitionFunction(&'a str, &'a str),

    /// The number of arguments provided is too small or large
    /// (function name, expected length, found length)
    FunctionParametersLengthMismatch(&'a str, usize, usize),

    /// Invalid function argument
    /// (span of argument, type expected, type found)
    FunctionArgumentTypeInvalid(&'a str, Type, Type),

    /// Function return type is incorrect
    /// (span of return expression, type expected, type found)
    InvalidFunctionReturn(&'a str, Type, Type),

    /// Function has a control flow that does not end in return or exit.
    /// (Function name)
    FunctionNoReturnOrExit(&'a str),

    /// Read statement invalid (wrong type)
    /// (variable/lvalue identifier, Result(type found, expression had errors))
    ReadStatementMismatch(&'a str, Result<Type, Vec<SemanticError<'a>>>),

    /// Free statement invalid (wrong type)
    /// (span of expression, Result(type found, expression had errors))
    FreeStatementMismatch(&'a str, Result<Type, Vec<SemanticError<'a>>>),

    /// Exit Statement Invalid (wrong type)
    /// (span of expression, type found)
    ExitStatementMismatch(&'a str, Result<Type, Vec<SemanticError<'a>>>),

    /// Print/Println Statement invalid (wrong type).
    /// Currently all types can be printed, this is here for extensibility.
    /// (span of print/println statement, type found)
    PrintStatementMisMatch(&'a str, Type, Type),

    /// Return statement outside of a function Only occurs in the main block.
    ReturnStatementMisplaced,
}

pub type SemanticErrorSummary<'a> = Vec<WrapSpan<'a, SemanticError<'a>>>;