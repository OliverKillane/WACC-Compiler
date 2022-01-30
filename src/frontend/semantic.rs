//! # Semantic Analysis and Symbol Table Generation
//! This module takes the provided AST and returns:
//! - Flat Symbol table of variables (identified by usize number)
//! - Symbol table of functions (string identifiers maintained)
//! - AST with renamed variable symbols (to usize integers)
//!
//! ## Error Results
//! Error printing is handled by the errors module, this module provides it
//! type, error and location (through string references) information.
//!
//! Errors returned can include the [expression errors](ExpressionError) which can
//! contain multiple errors, and indicates if they are from one part, or the
//! whole expression.
//!
//! ## Expression type checking
//! Given an expression, we can check it by providing the allowed types. It will
//! then use the allowed types, types for operators to find any expression errors.
//!
//! A type constraint system is used, by which constraints can be transformed by
//! operators and allow for expressions to evaluate to multiple types (e.g
//! println <expr>).
//!
//! Types for operators are defined in the [unops](UNOPS) and [binops](BINOPS)
//! tables such that any new tuples added to the table can update an operators
//! semantics with *no other code changes*.
//!
//! expressions can have multiple errors in different parts, and rich
//! suggestions for types and operators are provided.

use std::collections::HashMap;

use super::ast::{BinOp, Expr, Function, Param, Program, Stat, Type, UnOp, WrapSpan};

/// Function symbol table, holds the globally accessible functions identifier
/// by strings.
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionSymbolTable(HashMap<String, (Type, Vec<Type>)>);

impl FunctionSymbolTable {
    fn new() -> Self {
        Self(HashMap::new())
    }

    /// Define a function with the type (Return type, [param types]).
    fn def_fun(&mut self, ident: String, fun_type: (Type, Vec<Type>)) {
        self.0.insert(ident, fun_type);
    }

    /// Get the type of a given function by its identifier.
    fn get_fun(&self, ident: &str) -> Option<(Type, Vec<Type>)> {
        match self.0.get(ident) {
            Some(fun_type) => Some(fun_type.clone()),
            None => None,
        }
    }
}

/// A flat variable symbol table with all variables renamed to integers,
/// and associated with types identifiable through usize integers (same as in
/// the renamed AST).
pub struct VariableSymbolTable(HashMap<usize, Type>);

impl VariableSymbolTable {
    fn new() -> Self {
        Self(HashMap::new())
    }

    /// Define a variable with a type, identifier, and span of definition within
    /// the scope of the local symbol table.
    fn def_var<'a, 'b>(
        &mut self,
        ident: &'a str,
        var_type: Type,
        span: &'a str,
        local: &mut LocalSymbolTable<'a, 'b>,
    ) -> Option<SemanticError<'a>> {
        match local.in_scope(ident) {
            Some(def) => Some(SemanticError::RepeatDefinitionVariable(ident, def)),
            None => {
                let id = self.0.len();
                self.0.insert(id, var_type);
                local.add_var(ident, id, span);
                None
            }
        }
    }

    /// Get the type of a variable from the vantage point of the local scope.
    fn get_type<'a, 'b>(
        &self,
        ident: &'a str,
        local: &LocalSymbolTable<'a, 'b>,
    ) -> Option<(usize, Type)> {
        match local.get_var(ident) {
            Some((id, _)) => Some((
                id,
                self.0
                    .get(&id)
                    .expect("Failure: symbol in local table but not global.")
                    .clone(),
            )),
            None => None,
        }
    }
}

/// A chainable local symbol table, used to translate the current (and parent)
/// scope variables to integers for use in the flat variable symbol table and
/// the definition of the variable.
///
/// Used as a key to get variables by their string identifiers from the
/// variable symbol table.
/// ```
/// let mut var_symb = VariableSymbolTable::new();
/// let mut local_symb = LocalSymbolTable::new_root();
///
/// // define in current (local_symb) scope
/// var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb);
/// ```
struct LocalSymbolTable<'a, 'b> {
    current: HashMap<&'a str, (usize, &'a str)>,
    parent: Option<&'b Self>,
}

impl<'a, 'b> LocalSymbolTable<'a, 'b> {
    /// Create a new root scope (no parent).
    fn new_root() -> Self {
        LocalSymbolTable {
            current: HashMap::new(),
            parent: None,
        }
    }

    /// Create new child scope local symbol table.
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

impl Type {
    /// The semantics of type checking/when types can coalesce.
    /// - primitive types match eachother
    /// - Any type matches anything.
    /// - The generic type matches itself, it is used by the semantic analyser,
    ///   when propagated as an expected type it is converted to an Any. It is used
    ///   to check types for '==' where both sides are an "Any" but must be the
    ///   same.
    /// - Array Types match taking into account nesting of array types
    ///   (e.g Array(Array(int),1),1) == Array(int, 2) )
    pub fn coalesce(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Any | Type::Generic, _) => true,
            (_, Type::Any | Type::Generic) => true,
            (Type::Pair(box a1, box a2), Type::Pair(box b1, box b2)) => {
                a1.coalesce(b1) && a2.coalesce(b2)
            }
            (Type::Array(box a, dim_a), Type::Array(box b, dim_b)) => {
                if dim_a == dim_b {
                    a.coalesce(b)
                } else if dim_a > dim_b {
                    (&Type::Array(box a.clone(), dim_a - dim_b)).coalesce(b)
                } else {
                    a.coalesce(&Type::Array(box b.clone(), dim_b - dim_a))
                }
            }
            (ta, tb) => ta == tb,
        }
    }

    /// Reduce the indexing depth of a type.
    /// ```
    /// assert_eq!(Type::Array(Type::Int, 4).reduce_index_depth(4), Type::Int)
    /// ```
    pub fn reduce_index_depth(self, levels: usize) -> Option<Self> {
        if let Type::Array(box t, dim) = self {
            if levels == dim {
                Some(t)
            } else if levels < dim {
                Some(Type::Array(box t, dim - levels))
            } else {
                t.reduce_index_depth(levels - dim)
            }
        } else {
            None
        }
    }

    /// Index index depth of a type
    pub fn increase_index_depth(self, levels: usize) -> Self {
        Type::Array(box self, levels)
    }
}

/// Holds the type constraints for an expression.
/// ```text
/// int a = <exp>
/// ```
/// ```
/// TypeConstraint(vec![Type::Int])
/// ```
/// ```text
/// free <exp>
/// ```
/// ```
/// TypeConstraint(vec![Type::Array(box Type::Any, 1), Type::Pair(box Type::Any, box Type::Any)])
/// ```
/// ```text
/// if (<exp> == <exp>)
/// ```
/// ```
/// // for both exp:
/// TypeConstraint(vec![Type::Generic], true)
///
/// // for full expression
/// TypeConstraint(vec![Type::Bool], true)
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeConstraint(Vec<Type>);

impl TypeConstraint {
    /// Check if a type is within the constraint.
    ///
    /// If the type will coalesce to any in the constraint then it is 'inside'.
    fn inside(&self, check_type: &Type) -> bool {
        self.0.iter().any(|con_type| con_type.coalesce(check_type))
    }

    /// Given a unary operator, and the current constraints:
    /// - If the unary operator output matches the constraints, return the new
    ///   type constraint with its input types.
    /// - If the unary operator does not match, check for all possible
    ///   operators.
    fn new_from_unop(&self, unop: &UnOp) -> Result<Self, Self> {
        if self.unop_output_inside(unop) {
            // unary operator output matches the constraint, so use in
            // determining new constraint
            Ok(TypeConstraint(
                UNOPS
                    .iter()
                    .filter_map(|(op, input_type, output_type)| {
                        if self.inside(output_type) && op == unop {
                            Some(input_type.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
            ))
        } else {
            // unary operator does not work, so attempt with all possible
            // unary operators
            Err(TypeConstraint(
                UNOPS
                    .iter()
                    .filter_map(|(_, input_type, output_type)| {
                        if self.inside(output_type) {
                            Some(input_type.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
            ))
        }
    }

    /// Check that unary operator output is inside the constraint.
    fn unop_output_inside(&self, unop: &UnOp) -> bool {
        UNOPS
            .iter()
            .any(|(op, _, out)| op == unop && self.inside(out))
    }

    /// Given the constraint and the input type, get all possible unary
    /// operators that take the input and whose output is inside the constraint.
    fn get_possible_unops(&self, input_type: &Type) -> Vec<UnOp> {
        UNOPS
            .iter()
            .filter_map(|(unop, in_type, out_type)| {
                if input_type.coalesce(&in_type) && self.inside(out_type) {
                    Some(*unop)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Given the type constraint and the binary operator:
    /// - If the binary operator is valid, constrain using the binary operator.
    /// - If the binary operator is invalid, constrain using all binary
    ///   operators.
    pub fn new_from_binop(&self, binop: &BinOp) -> Result<(Self, Self), (Self, Self)> {
        if self.binop_output_inside(binop) {
            let mut left_cons = Vec::new();
            let mut right_cons = Vec::new();
            for (op, output_type, left_input_type, right_input_type) in BINOPS.iter() {
                if self.inside(output_type) && op == binop {
                    left_cons.push(left_input_type.clone());
                    right_cons.push(right_input_type.clone());
                }
            }
            Ok((TypeConstraint(left_cons), TypeConstraint(right_cons)))
        } else {
            let mut left_cons = Vec::new();
            let mut right_cons = Vec::new();
            for (_, output_type, left_input_type, right_input_type) in BINOPS.iter() {
                if self.inside(output_type) {
                    left_cons.push(left_input_type.clone());
                    right_cons.push(right_input_type.clone());
                }
            }
            Err((TypeConstraint(left_cons), TypeConstraint(right_cons)))
        }
    }

    /// Check binary operator output matches constraints.
    fn binop_output_inside(&self, binop: &BinOp) -> bool {
        BINOPS
            .iter()
            .any(|(op, out, _, _)| op == binop && self.inside(out))
    }

    /// Given an operators, the concrete input types and the constraint,
    /// determines if a generic definition is required to match, and if so that
    /// the right and left coalesce.
    fn binop_generic_check(&self, left_type: &Type, right_type: &Type, binop: &BinOp) -> bool {
        BINOPS.iter().any(|(op, _, left_in, right_in)| {
            op == binop
                && left_type.coalesce(left_in)
                && right_type.coalesce(right_in)
                && if left_in == &Type::Generic && right_in == &Type::Generic {
                    left_type.coalesce(right_type)
                } else {
                    true
                }
        })
    }

    /// Get all possible binary operators that could be applied to the inputs
    /// with an output inside the constraint.
    fn get_possible_binops(&self, left_input: &Type, right_input: &Type) -> Vec<BinOp> {
        BINOPS
            .iter()
            .filter_map(|(binop, out, left_in, right_in)| {
                if self.inside(out)
                    && left_input.coalesce(left_in)
                    && right_in.coalesce(right_in)
                    && if left_in == &Type::Generic && right_in == &Type::Generic {
                        left_input.coalesce(right_input)
                    } else {
                        true
                    }
                {
                    Some(*binop)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Indent all the type constraints (used for type checking array indexing).
    fn index_constraints(&self, level: usize) -> Self {
        TypeConstraint(
            self.0
                .iter()
                .map(|t| t.clone().increase_index_depth(level))
                .collect(),
        )
    }
}

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
    fn new_contains(err: ExprErrType<'a>) -> Self {
        Self::Contains(vec![err])
    }

    /// Convert asn expression error into a list of errors, maintaining it if
    /// it is already a list.
    fn to_contains(self) -> Self {
        match self {
            ExpressionError::Is(err) => ExpressionError::Contains(vec![err]),
            exprerror => exprerror,
        }
    }

    /// Add a new error onto the end of an existing error, potentially
    /// converting it into a list of errors.
    fn append_err(self, new_err: ExprErrType<'a>) -> Self {
        match self {
            ExpressionError::Is(err) => ExpressionError::Contains(vec![err, new_err]),
            ExpressionError::Contains(mut errs) => {
                errs.push(new_err);
                ExpressionError::Contains(errs)
            }
        }
    }

    /// Combine two Expression errors together (appending lists)
    fn add_errs(self, other: Self) -> Self {
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
type SemanticErrorSummary<'a> = Vec<WrapSpan<'a, SemanticError<'a>>>;

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

lazy_static! {
    /// Unary Operations allowed (operator, input type, output type)
    /// - To add overloading, simply add more tuples
    /// - Handled by the type constrain system when analysing expressions
    static ref UNOPS: [(UnOp, Type, Type); 5] = [
        (UnOp::Neg, Type::Bool, Type::Bool),
        (UnOp::Minus, Type::Int, Type::Int),
        (UnOp::Len, Type::Array(box Type::Any, 1), Type::Int),
        (UnOp::Ord, Type::Char, Type::Int),
        (UnOp::Chr, Type::Int, Type::Char)
    ];
}

/// Given a unary operator and input type gets the output type.
fn get_unop_output_type(unop: &UnOp, input_type: &Type) -> Option<Type> {
    for (op, in_type, out_type) in UNOPS.iter() {
        if unop == op && in_type.coalesce(input_type) {
            return Some(out_type.clone());
        }
    }
    None
}

/// Binary operations allowed (operator, output, left input, right input)
/// - To add overloading, simply add more tuples
/// - Handled by the type constrain system when analysing expressions
const BINOPS: [(BinOp, Type, Type, Type); 17] = [
    (BinOp::Add, Type::Int, Type::Int, Type::Int),
    (BinOp::Sub, Type::Int, Type::Int, Type::Int),
    (BinOp::Mul, Type::Int, Type::Int, Type::Int),
    (BinOp::Div, Type::Int, Type::Int, Type::Int),
    (BinOp::Mod, Type::Int, Type::Int, Type::Int),
    (BinOp::Gt, Type::Bool, Type::Int, Type::Int),
    (BinOp::Gt, Type::Bool, Type::Char, Type::Char),
    (BinOp::Gte, Type::Bool, Type::Int, Type::Int),
    (BinOp::Gte, Type::Bool, Type::Char, Type::Char),
    (BinOp::Lt, Type::Bool, Type::Int, Type::Int),
    (BinOp::Lt, Type::Bool, Type::Char, Type::Char),
    (BinOp::Lte, Type::Bool, Type::Int, Type::Int),
    (BinOp::Lte, Type::Bool, Type::Char, Type::Char),
    (BinOp::Eq, Type::Bool, Type::Generic, Type::Generic),
    (BinOp::Ne, Type::Bool, Type::Generic, Type::Generic),
    (BinOp::And, Type::Bool, Type::Bool, Type::Bool),
    (BinOp::Or, Type::Bool, Type::Bool, Type::Bool),
];

/// Given a binary operator and both input types gets the output type.
fn get_binop_output_type(
    binop: &BinOp,
    left_input_type: &Type,
    right_input_type: &Type,
) -> Option<Type> {
    for (op, left_in_type, right_in_type, out_type) in BINOPS.iter() {
        if binop == op
            && left_in_type.coalesce(left_input_type)
            && right_in_type.coalesce(right_input_type)
        {
            return Some(out_type.clone());
        }
    }
    None
}

/// Gets the current symbol table and returns:
/// - Symbol table of function identifier to function type
/// - Vector of functions with correct declarations (no repeated/keyword name/s
///   for function or parameters), these functions can be further analysed.
/// - Vector of errors from invalid functions.
///
/// The errors considered are:
/// - Function name is repeated, cannot process further semantic error thrown.
/// - Function name is keyword is reported, but analyser can continue.
///
/// *Note:* the parameter identifiers are not checked, that is done when the
/// function body analysis is done.
fn get_fn_symbols<'a>(
    fn_defs: Vec<WrapSpan<'a, Function<'a, &'a str, &'a str>>>,
) -> (
    FunctionSymbolTable,
    Vec<Function<'a, &'a str, &'a str>>,
    SemanticErrorSummary<'a>,
) {
    let mut fun_symb = FunctionSymbolTable::new();
    let mut def_table: HashMap<&str, &str> = HashMap::new();
    let mut valid_fun: Vec<Function<'a, &'a str, &'a str>> = Vec::new();
    let mut errors: SemanticErrorSummary = Vec::new();

    for WrapSpan(fn_def_span, Function(ret_type, fn_name, params, stats)) in fn_defs {
        match def_table.insert(fn_name, fn_def_span) {
            Some(orig) => errors.push(WrapSpan(
                fn_def_span,
                SemanticError::RepeatDefinitionFunction(fn_name, orig),
            )),
            None => {
                if KEYWORDS.contains(&fn_name) {
                    errors.push(WrapSpan(
                        fn_def_span,
                        SemanticError::KeywordFunctionName(fn_name),
                    ))
                }

                fun_symb.def_fun(
                    String::from(fn_name),
                    (
                        ret_type.clone(),
                        params
                            .iter()
                            .map(|WrapSpan(_, Param(t, _))| t.clone())
                            .collect(),
                    ),
                );
                valid_fun.push(Function::<'a>(ret_type, fn_name, params, stats))
            }
        }
    }

    (fun_symb, valid_fun, errors)
}

fn analyse_expression<'a, 'b>(
    WrapSpan(span, expr): WrapSpan<'a, Expr<'a, &'a str>>,
    type_cons: TypeConstraint,
    local_symb: &LocalSymbolTable<'a, 'b>,
    var_symb: &VariableSymbolTable,
) -> Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>> {
    // primitive check of base cases for expr (for conciseness)
    let prim_check = |prim_type: Type,
                      prim_expr: Expr<'a, usize>,
                      span: &'a str,
                      type_cons: TypeConstraint|
     -> Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>> {
        if type_cons.inside(&prim_type) {
            Ok((prim_type, WrapSpan(span, prim_expr)))
        } else {
            Err(ExpressionError::Is(ExprErrType::InvalidType(
                span, type_cons, prim_type,
            )))
        }
    };

    match expr {
        Expr::Null => prim_check(
            Type::Pair(box Type::Any, box Type::Any),
            Expr::Null,
            span,
            type_cons,
        ),
        Expr::Int(i) => prim_check(Type::Int, Expr::Int(i), span, type_cons),
        Expr::Bool(b) => prim_check(Type::Bool, Expr::Bool(b), span, type_cons),
        Expr::Char(c) => prim_check(Type::Char, Expr::Char(c), span, type_cons),
        Expr::String(s) => prim_check(Type::String, Expr::String(s), span, type_cons),
        Expr::Var(var_id) => match var_symb.get_type(var_id, &local_symb) {
            Some((var_rename, var_type)) => {
                if type_cons.inside(&var_type) {
                    Ok((var_type, WrapSpan(span, Expr::Var(var_rename))))
                } else {
                    Err(ExpressionError::Is(ExprErrType::InvalidType(
                        span, type_cons, var_type,
                    )))
                }
            }
            None => Err(ExpressionError::Is(ExprErrType::UndefinedVar(var_id))),
        },
        Expr::ArrayElem(var_id, indexes) => {
            // To get expected type for the variable, increase the
            // indentation of the constraints by the index_dim. unless ANY
            let index_level = indexes.len();
            let indexed_type_cons = type_cons.index_constraints(index_level);

            // check the expressions indexing
            let index_exprs: Vec<
                Result<(Type, WrapSpan<'a, Expr<'a, usize>>), ExpressionError<'a>>,
            > = indexes
                .into_iter()
                .map(|wrapped_exp: WrapSpan<'a, Expr<'a, &'a str>>| {
                    analyse_expression(
                        wrapped_exp,
                        TypeConstraint(vec![Type::Int]),
                        local_symb,
                        var_symb,
                    )
                })
                .collect();

            // check the identifier
            match var_symb.get_type(var_id, &local_symb) {
                Some((var_rename, var_type)) => {
                    // Get all errors from the indexes
                    let mut errors = vec![];
                    let mut correct = vec![];
                    for res in index_exprs {
                        match res {
                            Ok((_, expr)) => correct.push(expr),
                            Err(ExpressionError::Is(err)) => errors.push(err),
                            Err(ExpressionError::Contains(mut errs)) => errors.append(&mut errs),
                        }
                    }

                    if indexed_type_cons.inside(&var_type) {
                        // the type of the
                        if errors.len() == 0 {
                            // The variable is the correct type and all indexing expressions are correctly typed
                            Ok(
                                (var_type.reduce_index_depth(index_level).expect("expression reduced index could not match, despite the increased matching type constraints"), 
                                WrapSpan(span, Expr::ArrayElem(var_rename, correct)))
                            )
                        } else {
                            // Some of the expressions were erroneous e.g a[true]['a']
                            Err(ExpressionError::Contains(errors))
                        }
                    } else {
                        if errors.len() == 0 {
                            match var_type.clone().reduce_index_depth(index_level) {
                                // The subexpression is valid, but the whole expression evaluates to the wrong type
                                Some(t) => Err(ExpressionError::Is(ExprErrType::InvalidType(
                                    span, type_cons, t,
                                ))),
                                // The indexing is wrong for the variable being indexed, but all indexing expressions are well typed
                                None => {
                                    Err(ExpressionError::Contains(vec![ExprErrType::InvalidType(
                                        var_id, type_cons, var_type,
                                    )]))
                                }
                            }
                        } else {
                            // The variable being indexed is the wrong type, and there are errors in some of the indexing expressions.
                            errors.push(ExprErrType::InvalidType(var_id, type_cons, var_type));
                            Err(ExpressionError::Contains(errors))
                        }
                    }
                }
                None => {
                    let mut errors = vec![ExprErrType::UndefinedVar(var_id)];
                    for res in index_exprs {
                        match res {
                            Ok(_) => (),
                            Err(ExpressionError::Is(err)) => errors.push(err),
                            Err(ExpressionError::Contains(mut errs)) => errors.append(&mut errs),
                        }
                    }
                    // The variable being indexed is undefined, if any errors occur in the indexing expressions they are passed forwards
                    Err(ExpressionError::Contains(errors))
                }
            }
        }
        Expr::UnOp(WrapSpan(unop_span, unop), box inner_expr) => {
            match type_cons.new_from_unop(&unop) {
                Ok(inner_cons) => {
                    match analyse_expression(inner_expr, inner_cons, local_symb, var_symb) {
                        Ok((inner_type, inner)) => Ok((get_unop_output_type(&unop, &inner_type).expect("The operator type was correct, and the inner expression matched so it must have an output type"), WrapSpan(span, Expr::UnOp(WrapSpan(unop_span, unop), box inner)))),
                        Err(err) => Err(err.to_contains()),
                    }
                }
                Err(inner_cons) => {
                    match analyse_expression(inner_expr, inner_cons, local_symb, var_symb) {
                        Ok((inner_type, _)) => {
                            Err(ExpressionError::Contains(vec![ExprErrType::InvalidUnOp(
                                unop_span,
                                type_cons.get_possible_unops(&inner_type),
                                unop,
                            )]))
                        }
                        Err(ExpressionError::Is(ExprErrType::InvalidType(
                            inner_span,
                            inner_cons,
                            inner_type,
                        ))) => match get_unop_output_type(&unop, &inner_type) {
                            Some(out_type) => Err(ExpressionError::Is(ExprErrType::InvalidType(
                                span, type_cons, out_type,
                            ))),
                            None => Err(ExpressionError::Contains(vec![
                                ExprErrType::InvalidUnOp(
                                    unop_span,
                                    type_cons.get_possible_unops(&inner_type),
                                    unop,
                                ),
                                ExprErrType::InvalidType(inner_span, inner_cons, inner_type),
                            ])),
                        },
                        Err(other) => {
                            Err(other.append_err(ExprErrType::InvalidUnOp(unop_span, vec![], unop)))
                        }
                    }
                }
            }
        }
        Expr::BinOp(
            box WrapSpan(left_span, left_expr),
            WrapSpan(binop_span, binop),
            box WrapSpan(right_span, right_expr),
        ) => {
            match type_cons.new_from_binop(&binop) {
                Ok((left_cons, right_cons)) => {
                    match (
                        analyse_expression(
                            WrapSpan(left_span, left_expr),
                            left_cons,
                            local_symb,
                            var_symb,
                        ),
                        analyse_expression(
                            WrapSpan(right_span, right_expr),
                            right_cons,
                            local_symb,
                            var_symb,
                        ),
                    ) {
                        (Ok((left_type, left)), Ok((right_type, right))) => {
                            if type_cons.binop_generic_check(&left_type, &right_type, &binop) {
                                Ok((
                                    get_binop_output_type(&binop, &left_type, &right_type)
                                        .expect("Types matched, so must have an output type"),
                                    WrapSpan(
                                        span,
                                        Expr::BinOp(
                                            box left,
                                            WrapSpan(binop_span, binop),
                                            box right,
                                        ),
                                    ),
                                ))
                            } else {
                                Err(ExpressionError::Is(ExprErrType::InvalidGeneric(
                                    left_span, binop_span, right_span, left_type, binop, right_type,
                                )))
                            }
                        }
                        (Ok(_), Err(err)) => Err(err.to_contains()),
                        (Err(err), Ok(_)) => Err(err.to_contains()),
                        (Err(err1), Err(err2)) => Err(err1.add_errs(err2)),
                    }
                }
                Err((left_cons, right_cons)) => {
                    // the operator is invalid, searching with constraint from all binary operators
                    match (
                        analyse_expression(
                            WrapSpan(left_span, left_expr),
                            left_cons,
                            local_symb,
                            var_symb,
                        ),
                        analyse_expression(
                            WrapSpan(right_span, right_expr),
                            right_cons,
                            local_symb,
                            var_symb,
                        ),
                    ) {
                        (Ok((left_type, _)), Ok((right_type, _))) => {
                            Err(ExpressionError::Contains(vec![ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&left_type, &right_type),
                                binop,
                            )]))
                        }
                        (Ok((left_type, _)), Err(err)) => {
                            Err(err.append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&left_type, &Type::Any),
                                binop,
                            )))
                        }
                        (Err(err), Ok((right_type, _))) => {
                            Err(err.append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&Type::Any, &right_type),
                                binop,
                            )))
                        }
                        (Err(left_err), Err(right_err)) => Err(left_err
                            .add_errs(right_err)
                            .append_err(ExprErrType::InvalidBinOp(
                                binop_span,
                                type_cons.get_possible_binops(&Type::Any, &Type::Any),
                                binop,
                            ))),
                    }
                }
            }
        }
    }
}

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

        let symb_child = LocalSymbolTable::new_child(&symb_parent);

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
    fn local_symb_sibling_scopes_isolated() {
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

    #[test]
    fn var_symb_can_define_variables() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        assert_eq!(local_symb.current.len(), 2);
    }

    #[test]
    fn var_symb_cannot_redefine_variables_in_same_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        assert_eq!(
            var_symb.def_var("var1", Type::Bool, "bool var1 = true", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var1",
                "int var1 = 9"
            ))
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'b'", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var2",
                "char var2 = 'a'"
            ))
        );

        assert_eq!(
            var_symb.get_type("var1", &mut local_symb),
            Some((0, Type::Int))
        );
        assert_eq!(
            var_symb.get_type("var2", &mut local_symb),
            Some((1, Type::Char))
        );

        assert_eq!(local_symb.current.len(), 2);
        assert_eq!(var_symb.0.len(), 2);
    }

    #[test]
    fn var_symb_can_reference_variables_in_parent_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        let mut symb_child = LocalSymbolTable::new_child(&local_symb);

        assert_eq!(
            var_symb.get_type("var1", &mut symb_child),
            Some((0, Type::Int))
        );
        assert_eq!(
            var_symb.get_type("var2", &mut symb_child),
            Some((1, Type::Char))
        );
    }

    #[test]
    fn var_symb_can_define_variables_in_child_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut parent_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut parent_symb),
            None
        );

        {
            let mut symb_child = LocalSymbolTable::new_child(&parent_symb);

            assert_eq!(
                var_symb.def_var(
                    "var3",
                    Type::String,
                    "string var3 = \"hello world\"",
                    &mut symb_child
                ),
                None
            );
            assert_eq!(
                var_symb.def_var(
                    "var4",
                    Type::Pair(box Type::Int, box Type::Int),
                    "pair(int, int) var3 = null",
                    &mut symb_child
                ),
                None
            );

            assert_eq!(
                var_symb.get_type("var3", &mut symb_child),
                Some((2, Type::String))
            );
            assert_eq!(
                var_symb.get_type("var4", &mut symb_child),
                Some((3, Type::Pair(box Type::Int, box Type::Int)))
            );
        }

        assert_eq!(var_symb.get_type("var3", &mut parent_symb), None);
        assert_eq!(var_symb.get_type("var4", &mut parent_symb), None);
    }

    #[test]
    fn var_symb_can_shadow_variables_in_parent_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut parent_symb),
            None
        );

        let mut child_symb = LocalSymbolTable::new_child(&parent_symb);

        assert_eq!(
            var_symb.def_var("var1", Type::Char, "char var1 = 'a'", &mut child_symb),
            None
        );

        assert_eq!(var_symb.0.len(), 2);
    }

    #[test]
    fn var_symb_sibling_scopes_isolated() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut parent_symb),
            None
        );

        {
            let mut child_symb = LocalSymbolTable::new_child(&parent_symb);
            assert_eq!(
                var_symb.def_var("var3", Type::Char, "char var3 = 'a'", &mut child_symb),
                None
            );
            assert_eq!(
                var_symb.get_type("var1", &mut child_symb),
                Some((0, Type::Int))
            );
            assert_eq!(
                var_symb.get_type("var3", &mut child_symb),
                Some((1, Type::Char))
            );
            assert_eq!(var_symb.0.len(), 2);
        }

        {
            let mut child_symb = LocalSymbolTable::new_child(&parent_symb);
            assert_eq!(
                var_symb.def_var("var3", Type::Int, "int var1 = 3", &mut child_symb),
                None
            );
            assert_eq!(
                var_symb.get_type("var1", &mut child_symb),
                Some((0, Type::Int))
            );
            assert_eq!(
                var_symb.get_type("var3", &mut child_symb),
                Some((2, Type::Int))
            );
            assert_eq!(var_symb.0.len(), 3);
        }
    }

    #[test]
    fn var_symb_cannot_redefine_in_the_same_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var1", Type::Char, "char var1 = 'a'", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var1",
                "int var1 = 5"
            ))
        );
    }

    #[test]
    fn fun_symb_can_define_functions() {
        let mut fun_symb = FunctionSymbolTable::new();

        fun_symb.def_fun(
            String::from("fun1"),
            (Type::Int, vec![Type::Int, Type::Char]),
        );
        assert_eq!(
            fun_symb.get_fun("fun1"),
            Some((Type::Int, vec![Type::Int, Type::Char]))
        );
    }

    #[test]
    fn get_fn_symbols_can_get_valid_function_symbols() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool logical_or(bool x, bool y)",
                Function(
                    Type::Bool,
                    "logical_or",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun(String::from("fun1"), (Type::Int, vec![Type::Int]));
        fn_symb_expected.def_fun(
            String::from("example"),
            (Type::Char, vec![Type::Int, Type::String]),
        );
        fn_symb_expected.def_fun(
            String::from("logical_or"),
            (Type::Bool, vec![Type::Bool, Type::Bool]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "logical_or",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_detects_function_redefinition() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool fun1(bool x, bool y)",
                Function(
                    Type::Bool,
                    "fun1",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun(String::from("fun1"), (Type::Int, vec![Type::Int]));
        fn_symb_expected.def_fun(
            String::from("example"),
            (Type::Char, vec![Type::Int, Type::String]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![WrapSpan(
            "bool fun1(bool x, bool y)",
            SemanticError::RepeatDefinitionFunction("fun1", "int fun1(int a)"),
        )];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_detects_keyword_named_functions() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool begin(bool x, bool y)",
                Function(
                    Type::Bool,
                    "begin",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool fi(bool x, bool y)",
                Function(
                    Type::Bool,
                    "fi",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun(String::from("fun1"), (Type::Int, vec![Type::Int]));
        fn_symb_expected.def_fun(
            String::from("example"),
            (Type::Char, vec![Type::Int, Type::String]),
        );
        fn_symb_expected.def_fun(
            String::from("begin"),
            (Type::Bool, vec![Type::Bool, Type::Bool]),
        );
        fn_symb_expected.def_fun(
            String::from("fi"),
            (Type::Bool, vec![Type::Bool, Type::Bool]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "begin",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "fi",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![
            WrapSpan(
                "bool begin(bool x, bool y)",
                SemanticError::KeywordFunctionName("begin"),
            ),
            WrapSpan(
                "bool fi(bool x, bool y)",
                SemanticError::KeywordFunctionName("fi"),
            ),
        ];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_ignores_parameter_names() {
        // As noted in get_fn_symbols, parameter naming issues are ignored and checked later.
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string begin)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string begin", Param(Type::String, "begin")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool logical_or(bool x, bool y)",
                Function(
                    Type::Bool,
                    "logical_or",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun(String::from("fun1"), (Type::Int, vec![Type::Int]));
        fn_symb_expected.def_fun(
            String::from("example"),
            (Type::Char, vec![Type::Int, Type::String]),
        );
        fn_symb_expected.def_fun(
            String::from("logical_or"),
            (Type::Bool, vec![Type::Bool, Type::Bool]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string begin", Param(Type::String, "begin")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "logical_or",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_can_combine_multiple_errors() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool begin(bool x, bool y)",
                Function(
                    Type::Bool,
                    "begin",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "char begin(char x, bool y)",
                Function(
                    Type::Char,
                    "begin",
                    vec![
                        WrapSpan("char x", Param(Type::Char, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun(String::from("fun1"), (Type::Int, vec![Type::Int]));
        fn_symb_expected.def_fun(
            String::from("example"),
            (Type::Char, vec![Type::Int, Type::String]),
        );
        fn_symb_expected.def_fun(
            String::from("begin"),
            (Type::Bool, vec![Type::Bool, Type::Bool]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "begin",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![
            WrapSpan(
                "bool begin(bool x, bool y)",
                SemanticError::KeywordFunctionName("begin"),
            ),
            WrapSpan(
                "char begin(char x, bool y)",
                SemanticError::RepeatDefinitionFunction("begin", "bool begin(bool x, bool y)"),
            ),
        ];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn type_constraint_can_detect_inside() {
        let type_cons = TypeConstraint(vec![Type::Int, Type::Char]);

        assert!(type_cons.inside(&Type::Int));
        assert!(type_cons.inside(&Type::Char));

        // Any can match any type
        assert!(type_cons.inside(&Type::Any));
        assert!(type_cons.inside(&Type::Generic));

        // Check for constraint
        assert_eq!(type_cons.inside(&Type::String), false);
        assert_eq!(
            type_cons.inside(&Type::Pair(box Type::Char, box Type::Any)),
            false
        );
        assert_eq!(type_cons.inside(&Type::Array(box Type::Any, 1)), false);
        assert_eq!(type_cons.inside(&Type::Bool), false);
    }

    #[test]
    fn type_constraint_any_matches_all() {
        let type_cons = TypeConstraint(vec![Type::Any]);

        assert!(type_cons.inside(&Type::Int));
        assert!(type_cons.inside(&Type::Char));
        assert!(type_cons.inside(&Type::Bool));
        assert!(type_cons.inside(&Type::String));
        assert!(type_cons.inside(&Type::Pair(box Type::Any, box Type::Any)));
        assert!(type_cons.inside(&Type::Array(box Type::Int, 1)));
        assert!(type_cons.inside(&Type::Generic));
        assert!(type_cons.inside(&Type::Any));
        assert!(type_cons.inside(&Type::Pair(
            box Type::Array(box Type::Int, 2),
            box Type::Generic
        )));
    }

    #[test]
    fn type_constraint_empty_matches_nothing() {
        let type_cons = TypeConstraint(vec![]);

        assert_eq!(type_cons.inside(&Type::Int), false);
        assert_eq!(type_cons.inside(&Type::Char), false);
        assert_eq!(type_cons.inside(&Type::Bool), false);
        assert_eq!(type_cons.inside(&Type::String), false);
        assert_eq!(
            type_cons.inside(&Type::Pair(box Type::Any, box Type::Any)),
            false
        );
        assert_eq!(type_cons.inside(&Type::Array(box Type::Int, 1)), false);
        assert_eq!(type_cons.inside(&Type::Generic), false);
        assert_eq!(type_cons.inside(&Type::Any), false);
        assert_eq!(
            type_cons.inside(&Type::Pair(
                box Type::Array(box Type::Int, 2),
                box Type::Generic
            )),
            false
        );
    }

    #[test]
    fn type_constraint_gets_unary_ops() {
        let res = TypeConstraint(vec![Type::Int]).get_possible_unops(&Type::Any);

        assert!(res.contains(&(UnOp::Len)));
        assert!(res.contains(&(UnOp::Minus)));
        assert!(res.contains(&(UnOp::Ord)));

        assert_eq!(res.len(), 3);
    }

    #[test]
    fn type_constraints_can_constraint_to_pointers() {
        let ptr_cons = TypeConstraint(vec![
            Type::Array(box Type::Any, 1),
            Type::Pair(box Type::Any, box Type::Any),
        ]);

        assert!(ptr_cons.inside(&Type::Array(box Type::Int, 4)));
        assert!(ptr_cons.inside(&Type::Array(
            box Type::Pair(box Type::Char, box Type::Char),
            1
        )));
        assert!(ptr_cons.inside(&Type::Array(box Type::Array(box Type::Bool, 3), 4)));
        assert!(ptr_cons.inside(&Type::Pair(box Type::Int, box Type::Char)));
        assert!(ptr_cons.inside(&Type::Pair(
            box Type::Pair(box Type::Any, box Type::Any),
            box Type::Pair(box Type::Any, box Type::Any)
        )));
        assert!(ptr_cons.inside(&Type::Any));

        assert_eq!(ptr_cons.inside(&Type::Char), false);
        assert_eq!(ptr_cons.inside(&Type::Bool), false);
        assert_eq!(ptr_cons.inside(&Type::String), false);
    }

    #[test]
    fn type_constraint_gets_binary_ops() {
        let res_int = TypeConstraint(vec![Type::Int]).get_possible_binops(&Type::Any, &Type::Any);

        assert!(res_int.contains(&BinOp::Add));
        assert!(res_int.contains(&BinOp::Sub));
        assert!(res_int.contains(&BinOp::Mul));
        assert!(res_int.contains(&BinOp::Div));
        assert!(res_int.contains(&BinOp::Mod));

        assert_eq!(res_int.len(), 5);

        let res_bool = TypeConstraint(vec![Type::Bool]).get_possible_binops(&Type::Any, &Type::Any);

        assert!(res_bool.contains(&BinOp::Gt));
        assert!(res_bool.contains(&BinOp::Gt));
        assert!(res_bool.contains(&BinOp::Gte));
        assert!(res_bool.contains(&BinOp::Gte));
        assert!(res_bool.contains(&BinOp::Lt));
        assert!(res_bool.contains(&BinOp::Lt));
        assert!(res_bool.contains(&BinOp::Lte));
        assert!(res_bool.contains(&BinOp::Lte));
        assert!(res_bool.contains(&BinOp::Eq));
        assert!(res_bool.contains(&BinOp::Ne));
        assert!(res_bool.contains(&BinOp::And));
        assert!(res_bool.contains(&BinOp::Or));

        assert_eq!(res_bool.len(), 12);
    }

    #[test]
    fn type_constraint_gets_operators_with_any() {
        let res_binops =
            TypeConstraint(vec![Type::Any]).get_possible_binops(&Type::Any, &Type::Any);

        assert!(res_binops.contains(&BinOp::Add));
        assert!(res_binops.contains(&BinOp::Sub));
        assert!(res_binops.contains(&BinOp::Mul));
        assert!(res_binops.contains(&BinOp::Div));
        assert!(res_binops.contains(&BinOp::Mod));
        assert!(res_binops.contains(&BinOp::Gt));
        assert!(res_binops.contains(&BinOp::Gt));
        assert!(res_binops.contains(&BinOp::Gte));
        assert!(res_binops.contains(&BinOp::Gte));
        assert!(res_binops.contains(&BinOp::Lt));
        assert!(res_binops.contains(&BinOp::Lt));
        assert!(res_binops.contains(&BinOp::Lte));
        assert!(res_binops.contains(&BinOp::Lte));
        assert!(res_binops.contains(&BinOp::Eq));
        assert!(res_binops.contains(&BinOp::Ne));
        assert!(res_binops.contains(&BinOp::And));
        assert!(res_binops.contains(&BinOp::Or));

        assert_eq!(res_binops.len(), 17);
    }

    #[test]
    fn type_constraints_determines_if_unop_is_applicable() {
        assert_eq!(
            TypeConstraint(vec![Type::Int]).new_from_unop(&UnOp::Ord),
            Ok(TypeConstraint(vec![Type::Char]))
        );
        assert_eq!(
            TypeConstraint(vec![Type::Char]).new_from_unop(&UnOp::Ord),
            Err(TypeConstraint(vec![Type::Int]))
        );
        match TypeConstraint(vec![Type::Int]).new_from_unop(&UnOp::Chr) {
            Ok(_) => assert!(false), // operator chr produces char, not int,
            Err(type_cons) => {
                type_cons.inside(&Type::Int);
                type_cons.inside(&Type::Array(box Type::Char, 4));
                type_cons.inside(&Type::Char);
            }
        }
    }

    #[test]
    fn type_constraint_gets_new_from_binary_operators() {
        match TypeConstraint(vec![Type::Bool]).new_from_binop(&BinOp::Add) {
            Ok(_) => assert!(false), // Add does not produce a boolean
            Err((left_cons, right_cons)) => {
                // due to T == T we should get any (any two types can be compared, must be the same)
                assert!(left_cons.inside(&Type::Int));
                assert!(right_cons.inside(&Type::Int));
                assert!(left_cons.inside(&Type::Bool));
                assert!(right_cons.inside(&Type::Bool));
                assert!(left_cons.inside(&Type::Char));
                assert!(right_cons.inside(&Type::Char));
                println!("{:?} ::: {:?}", left_cons, right_cons);
                assert!(left_cons.inside(&Type::String));
                assert!(right_cons.inside(&Type::String));
                assert!(left_cons.inside(&Type::Pair(box Type::Int, box Type::Char)));
                assert!(right_cons.inside(&Type::Pair(box Type::Int, box Type::Char)));
                assert!(left_cons.inside(&Type::Array(box Type::Int, 3)));
                assert!(right_cons.inside(&Type::Array(box Type::Int, 3)));
            }
        }

        match TypeConstraint(vec![Type::Int]).new_from_binop(&BinOp::And) {
            Ok(_) => assert!(false), // and produces a boolean not an int
            Err((left_cons, right_cons)) => {
                // Currently the only binary operators returning integers are +,-,/,%,*
                assert!(left_cons.inside(&Type::Int));
                assert!(right_cons.inside(&Type::Int));
                assert_eq!(left_cons.inside(&Type::Bool), false);
                assert_eq!(right_cons.inside(&Type::Bool), false);
                assert_eq!(left_cons.inside(&Type::Char), false);
                assert_eq!(right_cons.inside(&Type::Char), false);
                assert_eq!(left_cons.inside(&Type::String), false);
                assert_eq!(right_cons.inside(&Type::String), false);
                assert_eq!(
                    left_cons.inside(&Type::Pair(box Type::Int, box Type::Char)),
                    false
                );
                assert_eq!(
                    right_cons.inside(&Type::Pair(box Type::Int, box Type::Char)),
                    false
                );
                assert_eq!(left_cons.inside(&Type::Array(box Type::Int, 3)), false);
                assert_eq!(right_cons.inside(&Type::Array(box Type::Int, 3)), false);
            }
        }
    }

    #[test]
    fn type_constraints_can_index_constraints() {
        let type_cons = TypeConstraint(vec![Type::Int]);

        let new_type_cons = type_cons.index_constraints(3);

        assert_eq!(new_type_cons.inside(&Type::Int), false);
        assert!(new_type_cons.inside(&Type::Array(box Type::Int, 3)));
        assert!(new_type_cons.inside(&Type::Array(box Type::Any, 1)));
    }

    // Tests for the implementation of equality on types
    #[test]
    fn type_coalesce_primitive_types_are_equal() {
        assert!(Type::Int.coalesce(&Type::Int));
        assert!(Type::Char.coalesce(&Type::Char));
        assert!(Type::String.coalesce(&Type::String));
        assert!(Type::Bool.coalesce(&Type::Bool));
    }

    #[test]
    fn type_coalesce_any_type_matches_anything() {
        assert!(Type::Any.coalesce(&Type::Int));
        assert!(Type::Int.coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Char));
        assert!(Type::Char.coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Bool));
        assert!(Type::Bool.coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::String));
        assert!(Type::String.coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Pair(box Type::Any, box Type::Any)));
        assert!(Type::Pair(box Type::Any, box Type::Any).coalesce(&Type::Any));
        assert!(Type::Any.coalesce(&Type::Array(
            box Type::Pair(box Type::Int, box Type::Char),
            4
        )));
        assert!(Type::Array(box Type::Pair(box Type::Int, box Type::Char), 4).coalesce(&Type::Any));
        assert!(Type::Any.coalesce(&Type::Generic));
        assert!(Type::Generic.coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Array(box Type::Int, 1)));
        assert!(Type::Array(box Type::Int, 1).coalesce(&Type::Any));
        assert!(Type::Any.coalesce(&Type::Any));
    }

    #[test]
    fn type_coalesce_generic_type_matches_itself() {
        assert!(Type::Generic.coalesce(&Type::Generic));
    }

    #[test]
    fn type_coalesce_differently_nested_array_types_are_equal() {
        assert!(Type::Array(box Type::Array(box Type::Int, 2), 3)
            .coalesce(&Type::Array(box Type::Int, 5)));
        assert!(Type::Array(box Type::Array(box Type::Any, 2), 3)
            .coalesce(&Type::Array(box Type::Int, 5)));
        assert!(Type::Array(box Type::Array(box Type::Any, 2), 3)
            .coalesce(&Type::Array(box Type::Array(box Type::Int, 2), 3)));
        assert!(Type::Array(box Type::Any, 1).coalesce(&Type::Array(
            box Type::Pair(box Type::Generic, box Type::Int),
            3
        )));
        assert!(Type::Array(box Type::Char, 3).coalesce(&Type::Array(
            box Type::Array(box Type::Array(box Type::Char, 1), 1),
            1
        )))
    }

    #[test]
    fn type_coalesce_differentiates_between_primitive_types() {
        assert_eq!(Type::Int.coalesce(&Type::Bool), false);
        assert_eq!(Type::Int.coalesce(&Type::Char), false);
        assert_eq!(Type::Int.coalesce(&Type::String), false);
        assert_eq!(Type::Bool.coalesce(&Type::Int), false);
        assert_eq!(Type::Bool.coalesce(&Type::Char), false);
        assert_eq!(Type::Bool.coalesce(&Type::String), false);
        assert_eq!(Type::Char.coalesce(&Type::Int), false);
        assert_eq!(Type::Char.coalesce(&Type::Bool), false);
        assert_eq!(Type::Char.coalesce(&Type::String), false);
        assert_eq!(Type::String.coalesce(&Type::Int), false);
        assert_eq!(Type::String.coalesce(&Type::Bool), false);
        assert_eq!(Type::String.coalesce(&Type::Char), false);
    }

    #[test]
    fn type_reduce_index_reduces() {
        assert_eq!(
            Type::Array(box Type::Int, 3).reduce_index_depth(3),
            Some(Type::Int)
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Int, 2), 3).reduce_index_depth(5),
            Some(Type::Int)
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Int, 2), 3).reduce_index_depth(4),
            Some(Type::Array(box Type::Int, 1))
        );
        assert_eq!(
            Type::Array(
                box Type::Array(
                    box Type::Array(box Type::Array(box Type::Array(box Type::Any, 1), 1), 1),
                    1
                ),
                1
            )
            .reduce_index_depth(4),
            Some(Type::Array(box Type::Any, 1))
        );
    }

    #[test]
    fn type_reduce_index_throw_none_if_impossible() {
        assert_eq!(
            Type::Array(box Type::Int, 3).reduce_index_depth(3),
            Some(Type::Int)
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Int, 2), 3).reduce_index_depth(7),
            None
        );
        assert_eq!(
            Type::Array(box Type::Array(box Type::Int, 2), 3).reduce_index_depth(10),
            None
        );
        assert_eq!(
            Type::Array(
                box Type::Array(
                    box Type::Array(box Type::Array(box Type::Array(box Type::Any, 1), 1), 1),
                    1
                ),
                1
            )
            .reduce_index_depth(7),
            None
        );
        assert_eq!(Type::Int.reduce_index_depth(1), None);
    }

    #[test]
    fn type_increase_index_increases_index() {
        assert_eq!(
            Type::Int.increase_index_depth(4),
            Type::Array(box Type::Int, 4)
        );
        assert_eq!(
            Type::Char.increase_index_depth(2),
            Type::Array(box Type::Char, 2)
        );
        assert_eq!(
            Type::Any.increase_index_depth(3),
            Type::Array(box Type::Any, 3)
        );
    }

    // #[test]
    fn scratch_test() {
        let local_symb = LocalSymbolTable::new_root();
        let var_symb = VariableSymbolTable::new();

        let expr1: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) || (3 * -true)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        WrapSpan("+", BinOp::Add),
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                ),
                WrapSpan("||", BinOp::Or),
                box WrapSpan(
                    "3 * -true",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan(
                            "-true",
                            Expr::UnOp(
                                WrapSpan("-", UnOp::Minus),
                                box WrapSpan("true", Expr::Bool(true)),
                            ),
                        ),
                    ),
                ),
            ),
        );

        // (1 + 1) || (3 * 3)
        // If we want Int: || incorrect, should be +, -, *, /, %
        // If we want bool: + and * are incorrect, should be >=,
        let expr2: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) || (3 * 3)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        WrapSpan("+", BinOp::Add),
                        box WrapSpan("1", Expr::Int(1)),
                    ),
                ),
                WrapSpan("||", BinOp::Or),
                box WrapSpan(
                    "3 * 3",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("3", Expr::Int(3)),
                    ),
                ),
            ),
        );

        // (1 + true) || (3 * 3)
        // If we want Int: || incorrect, should be +, -, *, /, %
        // If we want bool: + and * are incorrect, should be >=,
        let expr3: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + true) || (3 * 3)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        WrapSpan("+", BinOp::Add),
                        box WrapSpan("4", Expr::Bool(true)),
                    ),
                ),
                WrapSpan("-", BinOp::Sub),
                box WrapSpan(
                    "3 * 3",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("3", Expr::Int(3)),
                    ),
                ),
            ),
        );

        // (1 + 4) - (3 * 3)
        let expr4: WrapSpan<Expr<&str>> = WrapSpan(
            "(1 + 1) - (3 * 3)",
            Expr::BinOp(
                box WrapSpan(
                    "1 + 1",
                    Expr::BinOp(
                        box WrapSpan("1", Expr::Int(1)),
                        WrapSpan("+", BinOp::Add),
                        box WrapSpan("4", Expr::Int(4)),
                    ),
                ),
                WrapSpan("-", BinOp::Sub),
                box WrapSpan(
                    "3 * 3",
                    Expr::BinOp(
                        box WrapSpan("3", Expr::Int(3)),
                        WrapSpan("*", BinOp::Mul),
                        box WrapSpan("3", Expr::Int(3)),
                    ),
                ),
            ),
        );

        let type_cons = TypeConstraint(vec![Type::Int]);
        let res = analyse_expression(expr3, type_cons, &local_symb, &var_symb);

        println!("{:?}", res);
        assert!(false);
    }
}

/*
  1. Add more type info to InvalidBinOp and InvalidUnOp
  2. Write tests for expression
  3. Write statement checker
  4. Write block checker
  5. write function linkup
*/
