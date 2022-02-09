//! Semantic errors account for expression (operator and type mismatches), as
//! well as control flow (require termination and return), variable and function
//! definition (and more).
//!
//! The semantic analyser can find multiple semantic errors, including multiple
//! in a single statement or expression.

use super::super::ast::{BinOp, Type, UnOp, WrapSpan};

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

    /// Invalid type for expression
    /// (span of expression, expected type, found type)
    ///
    /// e.g a: int
    /// a[0] <- invalid variable type
    InvalidVariableType(&'a str, Type, Type),

    /// Invalid type for expression
    /// (span of expression, expected type, found type)
    InvalidType(&'a str, Type, Type),

    /// Value of function call is invalid
    /// (function name in span, expected type, found type)
    InvalidCallType(&'a str, Type, Type),

    /// Invalid array literal (when an array literal is assigned to a non array
    /// type).
    /// (span of literal, found types)
    InvalidArrayLiteral(&'a str, Vec<Type>),

    /// A binary operator is applied incorrectly
    /// (span of binary op, possible input types, possible operators, found types, found operator)
    InvalidBinOp(
        &'a str,
        Vec<(Type, Type)>,
        Vec<&'static BinOp>,
        (Type, Type),
        BinOp,
    ),

    /// Invalid unary operator application
    /// (span of operation, possible types, possible unops, found type, found unop)
    InvalidUnOp(&'a str, Vec<Type>, Vec<&'static UnOp>, Type, UnOp),

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
    /// (span of argument, name of parameter type expected, type found)
    FunctionArgumentTypeInvalid(&'a str, &'a str, Type, Type),

    /// Function return type is incorrect
    /// (span of return expression, type expected, type found)
    InvalidFunctionReturn(&'a str, Type, Type),

    /// Function has a control flow that does not end in return or exit.
    /// (Function name)
    FunctionNoReturnOrExit(&'a str),

    /// Read statement invalid (wrong type)
    /// (variable/lvalue identifier, type found)
    ReadStatementMismatch(&'a str, Type),

    /// Free statement invalid (wrong type)
    /// (span of expression, type found)
    FreeStatementMismatch(&'a str, Type),

    /// Exit Statement Invalid (wrong type)
    /// (span of expression, type found)
    ExitStatementMismatch(&'a str, Type),

    /// Print/Println Statement invalid (wrong type).
    /// Currently all types can be printed, this is here for extensibility.
    /// (span of print/println statement, type found)
    PrintStatementMisMatch(&'a str, Type, Type),

    /// If statement must take a bool
    /// (span of expression, type found)
    InvalidIfCondition(&'a str, Type),

    /// If statement must take a bool
    /// (span of expression, type found)
    InvalidWhileCondition(&'a str, Type),

    /// Return statement outside of a function Only occurs in the main block.
    ReturnStatementMisplaced,
}

pub type SemanticErrorSummary<'a> = Vec<WrapSpan<'a, Vec<SemanticError<'a>>>>;
