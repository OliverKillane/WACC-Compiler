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
/// scope variables to integers for use in the flat variable symbol table.
struct LocalSymbolTable<'a, 'b>(
    HashMap<&'a str, usize>,
    Option<&'b LocalSymbolTable<'a, 'b>>,
);

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
pub enum ExpressionError<'a> {
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
    AssignmentMismatch(&'a str, Vec<ExpressionError<'a>>),

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
    FunctionParametersMismatch(&'a str, Vec<(&'a str, Vec<ExpressionError<'a>>)>),

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
    FunctionReturnMismatch(&'a str, Vec<ExpressionError<'a>>),

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
    FreeStatementMismatch(&'a str, Vec<ExpressionError<'a>>),

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
    ExitStatementMismatch(&'a str, Vec<ExpressionError<'a>>),

    /// Print/Println Statement invalid (wrong type).
    /// Currently all types can be printed, this is here for extensibility.
    /// (span of print/println statement, type found)
    PrintStatementMisMatch(&'a str, Vec<ExpressionError<'a>>),

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