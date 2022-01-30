//! Semantic errors account for expression (operator and type mismatches), as 
//! well as control flow (require termination and return), variable and function
//! definition (and more).
//! 
//! The semantic analyser can find multiple semantic errors, including multiple 
//! in a single statement or expression.

use crate::frontend::semantic::type_constraints::*;
use crate::frontend::ast::*;

/// Contains a single error within an expression.
#[derive(Debug, PartialEq, Eq)]
pub enum ExprErrType<'a> {
    /// Invalid type, including when no type inference is possible.
    /// ```text
    /// bool a = 9
    /// ```
    /// ```
    /// ExprErrType::InvalidType("9", TypeConstraint(vec![Type::Bool]), Type::Int)
    /// ```
    /// ```text
    /// bool a = true || 15
    /// ```
    /// ```
    /// ExprErrType::InvalidType("15", TypeConstraint(vec![Type::Bool]), Type::Int))
    /// ```
    InvalidType(&'a str, TypeConstraint, Type),

    /// Invalid Binary Operator, using a different operator the expression could be valid.
    /// - int a = <int> || && <= < > >= == != <int>
    /// - bool a = <bool> + / % * <bool>
    /// ```text
    /// int a = 9 || 8
    /// ```
    /// ```
    /// ExprErrType::BinOp("||", vec![BinOp::Add, BinOp::Mul, BinOp::Mod, BinOp::Div, BinOp::Sub], BinOp::Or)
    /// ```
    InvalidBinOp(&'a str, Vec<BinOp>, BinOp),

    /// Invalid Unary Operator, with different operator, the expression is valid
    /// ```text
    /// bool a = -true
    /// ```
    /// ```
    /// ExprErrType::UnOp("-", vec![UnOp::Neg], UnOp::Minus)
    /// ```
    InvalidUnOp(&'a str, Vec<UnOp>, UnOp),

    /// Use of undefined variable.
    /// ```text
    /// int a = 3 * b
    /// ```
    /// ```
    /// ExprErrType::UndefinedVar("b")
    /// ```
    UndefinedVar(&'a str),

    /// No match generic
    /// ```text
    /// bool a = 1 == true
    /// ```
    /// ```
    /// ExprErrType::InvalidGeneric("1","==", "true", Type::Int, BinOp::Eq, Type::Bool)
    InvalidGeneric(&'a str, &'a str, &'a str, Type, BinOp, Type),
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

impl<'a> ExpressionError<'a> {
    /// Create a contains with a single error
    pub fn new_contains(err: ExprErrType<'a>) -> Self {
        Self::Contains(vec![err])
    }

    /// Convert asn expression error into a list of errors, maintaining it if
    /// it is already a list.
    pub fn to_contains(self) -> Self {
        match self {
            ExpressionError::Is(err) => ExpressionError::Contains(vec![err]),
            exprerror => exprerror,
        }
    }

    /// Add a new error onto the end of an existing error, potentially
    /// converting it into a list of errors.
    pub fn append_err(self, new_err: ExprErrType<'a>) -> Self {
        match self {
            ExpressionError::Is(err) => ExpressionError::Contains(vec![err, new_err]),
            ExpressionError::Contains(mut errs) => {
                errs.push(new_err);
                ExpressionError::Contains(errs)
            }
        }
    }

    /// Combine two Expression errors together (appending lists)
    pub fn add_errs(self, other: Self) -> Self {
        match (self, other) {
            (ExpressionError::Contains(mut errs1), ExpressionError::Contains(mut errs2)) => {
                errs1.append(&mut errs2);
                ExpressionError::Contains(errs1)
            }
            (ExpressionError::Is(err1), ExpressionError::Is(err2)) => {
                ExpressionError::Contains(vec![err1, err2])
            }
            (ExpressionError::Is(err), errs) => errs.append_err(err),
            (errs, ExpressionError::Is(err)) => errs.append_err(err),
        }
    }
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