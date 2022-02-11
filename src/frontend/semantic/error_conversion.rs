//! Convert the semantic errors generated by semantic analysis into errors to be
//! printed in the error system.
//!
//! As a function not returning or exiting is a syntax error, we must separate
//! the FunctionNoExitOrReturn semanticError from the others and report as a
//! syntax error
//!
//! ## The errors reported are as follows:
//! | Error Type                         | Summary Code | Description                                                                                                                               |
//! |------------------------------------|--------------|-------------------------------------------------------------------------------------------------------------------------------------------|
//! | Undefined Variable Assigned To     | 201          | Variable was not defined, but a value was assigned to it                                                                                  |
//! | Undefined Variable Used            | 202          | A variable was used in an expression or other statement                                                                                   |
//! | Invalid Index Invalid              | 203          | Can only index arrays with integers, e.g `a[true]` throws this error                                                                      |
//! | Invalid Variable Type              | 204          | The type of a varible used is not correct, for example in array indexing, e.g `int a = 8; a[7] = 9`                                       |
//! | Invalid Type                       | 205          | The type of an expression is wrong or cannot be coerced to the expected, e.g `bool a = 3 + 3`                                             |
//! | Invalid Call Type                  | 206          | An invalid type, but from a function call, e.g where `int fun(int a)` an error is thrown on `bool a = call fun(9)`                        |
//! | Invalid Array Literal              | 207          | An array literal's elements are not the same type, e.g `[1,2,3,true]`                                                                     |
//! | Invalid Binary Operation           | 208          | A binary operation is not well typed, e.g `true + 3` or `'c' \|\| 'a'`                                                                    |
//! | Invalid Unary Operation            | 209          | A unary operation is not well typed, e.g `-true` or `ord 9`                                                                               |
//! | Repeat Definition of Variable      | 210          | A variable definition is repeated in the same scope, e.g `int a = 3; bool a = true`                                                       |
//! | Undefined Function                 | 211          | A call is made to an undefined function, e.g `int a = call undefined_fun(99)`                                                             |
//! | Repeat Definition of Function      | 212          | A function name is used in two definitions, e.g `int fun1(int a) ... char fun1(bool b)`                                                   |
//! | Function Parameter Length Mismatch | 213          | A function is provided an incorrect number of arguments, e.g `int fun1(int a) ... int a = call fun1(3,true)`                              |
//! | Function Argument Type Invalid     | 214          | A function call has the correct number of arguments, but some of their types are wrong, e.g `int fun1(int a) ... int b = call fun1(true)` |
//! | Invalid Function Return            | 215          | A function's return returns the wrong type, e.g `int fun1(int a) is ... return true end`                                                  |
//! | Function No Return Or Exit         | 199          | A function has a control flow path that does not return or exit. This is a syntax error.                                                  |
//! | Read Statement Mismatch            | 216          | The read statement is given an identifier/left assignment that is not a char or int, e.g `string a = "hey" ; read a`                      |
//! | Free Statement Mismatch            | 217          | The free statement is given an identifier/left assignment that is not an array or pair, e.g `int a = 9 ; free a`                          |
//! | Exit Statement Mismatch            | 218          | The exit statement is given a value that is not an integer, e.g `exit "hello"`                                                            |
//! | Print Statement Mismatch           | 219          | A print statement is given the wrong type, currently all types can be printed, so this is here for future extension.                      |
//! | Invalid If Condition               | 220          | The condition for an if statement is not a boolean, e.g `if (9 + 9) ...`                                                                  |
//! | Invalid While Condition            | 221          | The condition for a while statement is not a boolean, e.g `while (9+9) ...`                                                               |
//! | Return Statement Misplaced         | 222          | There is a return statement in the main program body, returns are only allowed in functions.                                              |
//! | InvalidPairOp                      | 223          | Fst or Snd directly applied to null pair literal, e.g `fst null` `snd null`                                                               |
//! | Function Last Is While             | 198          | The last statement in a function is a while loop, which may not return (undecidable)                                                      |

use super::{
    super::{
        ast::{BinOp, GenericId, Type, UnOp, WrapSpan},
        error::{Summary, SummaryCell, SummaryComponent, SummaryStage, SummaryType},
    },
    semantic_errors::{self, SemanticError, StatementErrors},
};
use std::{fmt::Display, ops::Add};

/// Takes errors from semantic analysis and converts them into:
/// - A vector of erroneous (semantic) statements/definitions
/// - A vector of erroneous (syntactic) statements
pub fn convert_errors<'a>(
    def_errs: Vec<StatementErrors<'a>>,
    main_errs: Vec<StatementErrors<'a>>,
    fun_errs: Vec<(&str, Vec<StatementErrors<'a>>)>,
    source_code: &'a str,
) -> Vec<Summary<'a>> {
    let mut semantic_errors = Summary::new(source_code, SummaryStage::Semantic);
    let mut syntax_errors = Summary::new(source_code, SummaryStage::Parser);

    create_cells(
        String::from("In Function Declarations"),
        def_errs,
        &mut semantic_errors,
        &mut syntax_errors,
    );
    create_cells(
        String::from("In Main Program Body"),
        main_errs,
        &mut semantic_errors,
        &mut syntax_errors,
    );
    for (fun_name, errs) in fun_errs {
        create_cells(
            format!("In Function {}", fun_name),
            errs,
            &mut semantic_errors,
            &mut syntax_errors,
        )
    }

    let mut summaries = Vec::with_capacity(1);
    if !semantic_errors.is_empty() {
        summaries.push(semantic_errors)
    }

    if !syntax_errors.is_empty() {
        summaries.push(syntax_errors)
    }

    summaries
}

/// Generates an error cell for each statement in the vector of [statement errors](StatementErrors)
/// and splits the syntax and semantic errors into different cells.
fn create_cells<'a>(
    title: String,
    statements: Vec<StatementErrors<'a>>,
    semantic_errs: &mut Summary<'a>,
    syntax_errs: &mut Summary<'a>,
) {
    for WrapSpan(span, errs) in statements {
        let (syn, sem): (Vec<SemanticError>, Vec<SemanticError>) =
            errs.into_iter().partition(|err| {
                matches!(err, SemanticError::FunctionNoReturnOrExit(_))
                    || matches!(err, SemanticError::FunctionLastStatIsWhile(_))
            });

        if !syn.is_empty() {
            let mut syntax_cell = SummaryCell::new(span);
            syntax_cell.set_title(title.clone());

            for err in syn {
                syntax_cell.add_component(err.into());
            }
            syntax_errs.add_cell(syntax_cell);
        }

        if !sem.is_empty() {
            let mut semantic_cell = SummaryCell::new(span);
            semantic_cell.set_title(title.clone());

            for err in sem {
                semantic_cell.add_component(err.into());
            }
            semantic_errs.add_cell(semantic_cell);
        }
    }
}

#[allow(clippy::from_over_into)]
impl<'a> Into<SummaryComponent<'a>> for SemanticError<'a> {
    /// Convert semantic errors into summary components.
    fn into(self) -> SummaryComponent<'a> {
        match self {
            SemanticError::UndefinedVariableAssignment(var_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    201,
                    var_span,
                    format!("Assignment to undefined variable {}.", var_span)
                ),
            SemanticError::UndefinedVariableUse(var_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    202,
                    var_span,
                    format!("Use of undeclared variable {}.", var_span)
                ),
            SemanticError::InvalidIndex(index_span, index_type) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    203,
                    index_span,
                    format!("Incorrect type of index, only {} can be used to index, but here an expression of type {} was used.", Type::Int, index_type)
                ),
            SemanticError::InvalidVariableType(var_span, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    204,
                    var_span,
                    format!("Incorrect type for variable, expected a {} but found {}", expected, found)
                ),
            SemanticError::InvalidType(expr_span, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    205,
                    expr_span,
                    format!("Wrong type! Expected {} but found {}", expected, found)
                ),
            SemanticError::InvalidCallType(call_span, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    206,
                    call_span,
                    format!("Wrong type returned from call! Expected {} but found {}", expected, found)
                ),
            SemanticError::InvalidArrayLiteral(lit_span, types) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    207,
                    lit_span,
                    format!("Invalid array literal, all must be the same type, but found an array with types: [{}]", types.into_iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(","))
                ),
            SemanticError::InvalidBinOp(binop_span, possible_types, possible_ops, (found_left, found_right), found_op) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    208,
                    binop_span,
                    format!("Invalid application of operator {} on {} and {}.", found_left, found_right, found_op)
                ).set_note(format!("{} {}",
                        if possible_types.is_empty() {
                            format!("There are no possible input types for the operator {}.", found_op)
                        } else {
                            format!("Possible input types for {} are {}.", found_op, possible_types.into_iter().map(|(left_t, right_t)| format!("{} and {}", left_t, right_t)).collect::<Vec<_>>().join(","))
                        },
                        if possible_ops.is_empty() {
                            format!("There are no possible operators for the input types {} and {}.", found_left, found_right)
                        } else {
                            format!("Possible operators for {} and {} are {}.", found_left, found_right, possible_ops.into_iter().map(|op| format!("{}", op)).collect::<Vec<_>>().join(",") )
                        }
                    )
                )
                ,
            SemanticError::InvalidUnOp(op_span, possible_types, possible_ops, found_type, found_op) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    209,
                    op_span,
                    format!("Invalid application of {} on {}.", found_op, found_type)
                ).set_note(format!("{} {}",
                        if possible_types.is_empty() {
                            format!("There are no possible input types for {}.", found_op) // there will *always* be an input type, this is there in the event that we remove an operator from the unops table, but leave in the ast.
                        } else {
                            format!("Possible input types for {} include {}.", found_op, possible_types.into_iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(","))
                        }, if possible_ops.is_empty() {
                            format!("There are no possible unary operators for the type {}.", found_type)
                        } else {
                            format!("Possible operators on the type {} are {}.", found_type, possible_ops.into_iter().map(|op| format!("{}", op)).collect::<Vec<_>>().join(","))
                        }
                    )
                ),
            SemanticError::RepeatDefinitionVariable(var_span, orig_def_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    210,
                    var_span,
                    format!("Repeat definition of variable {}.", var_span)
                ).set_declaration(orig_def_span),
            SemanticError::UndefinedFunction(fun_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    211,
                    fun_span,
                    format!("Use of undefined function {}.", fun_span)
                ),
            SemanticError::RepeatDefinitionFunction(fun_span, orig_def_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    212,
                    fun_span,
                    format!("Repeat definition of function {}.", fun_span)
                ),
            SemanticError::FunctionParametersLengthMismatch(fun_span, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    213,
                    fun_span,
                    format!("Call to function {} expects {} arguments, but {} were provided", fun_span, expected, found)
                ),
            SemanticError::FunctionArgumentTypeInvalid(arg_span, param_name, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    214,
                    arg_span,
                    format!("Function argument for parameter {} is of type {} but should have been {}.", param_name, found, expected)
                ),
            SemanticError::InvalidFunctionReturn(expr_span, expected, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    215,
                    expr_span,
                    format!("Incorrect type returned, should have been {} but found {}.", expected, found)
                ),
            SemanticError::FunctionNoReturnOrExit(ending_stat_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    199,
                    ending_stat_span,
                    String::from("The last statement on any path through a function must be a return or exit.")
                ),
            SemanticError::ReadStatementMismatch(read_ident, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    216,
                    read_ident,
                    format!("Wrong type of identifier used in read statement, found {} but should have been an {} or {}.", found, Type::Int, Type::Char)
                ),
            SemanticError::FreeStatementMismatch(expr_span, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    217,
                    expr_span,
                    format!("Cannot free {}, needs to be a pointer types such as an {} or {}", found, Type::Pair(box Type::Any, box Type::Any), Type::Array(box Type::Any, 1))
                ),
            SemanticError::ExitStatementMismatch(expr_span, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    218,
                    expr_span,
                    format!("Cannot exit with code of {}, must be an {}.", found, Type::Int)
                ),
            SemanticError::PrintStatementMisMatch(print_span, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    219,
                    print_span,
                    format!("Cannot print an {}, must be of type {}.", found, Type::Any)
                ),
            SemanticError::InvalidIfCondition(cond_span, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    220,
                    cond_span,
                    format!("If condition must be a {}, however {} was found.", Type::Bool, found)
                ),
            SemanticError::InvalidWhileCondition(cond_span, found) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    221,
                    cond_span,
                    format!("While loop condition must be a {}, however {} was found.", Type::Bool, found)
                ),
            SemanticError::ReturnStatementMisplaced(stat_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    222,
                    stat_span,
                    String::from("Cannot have a return here!")
                ),
            SemanticError::InvalidPairOp(null_span) => SummaryComponent::new(
                SummaryType::Error,
                223,
                null_span,
                String::from("Cannot use the fst or snd operators directly on a null.")
            ).set_note(
                String::from("Either create a new pair, or assign the null to a variable.")
            ),
            SemanticError::FunctionLastStatIsWhile(while_span) =>
                SummaryComponent::new(
                    SummaryType::Error,
                    198,
                    while_span,
                    String::from("A while loop cannot be the last statement on a path through the function")
                ).set_note(String::from("All functions must return or exit, and this cannot be determined for while loops even if they contain a return or exit."))
        }
    }
}

impl Display for BinOp {
    /// Formatting for binary operators to display in the error summaries.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Gt => ">",
                BinOp::Gte => ">=",
                BinOp::Lt => "<",
                BinOp::Lte => "<=",
                BinOp::Eq => "==",
                BinOp::Ne => "!=",
                BinOp::And => "&&",
                BinOp::Or => "||",
                BinOp::Newpair => "newpair",
            }
        )
    }
}

impl Display for UnOp {
    /// Formatting for unary operators to display in the error summaries.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnOp::Neg => "!",
                UnOp::Minus => "-",
                UnOp::Len => "len",
                UnOp::Ord => "ord",
                UnOp::Chr => "chr",
                UnOp::Fst => "fst",
                UnOp::Snd => "snd",
            }
        )
    }
}

impl Display for Type {
    /// Formatting for types to display in the error summaries.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::String => write!(f, "string"),
            Type::Any => write!(f, ""),
            Type::Generic(n) => write!(f, "{}", generic_to_alpha(*n)),
            Type::Pair(box t1, box t2) => write!(f, "pair({},{})", t1, t2),
            Type::Array(box t, n) => {
                write!(f, "{}{}", t, (0..*n).map(|_| "[]").collect::<String>())
            }
        }
    }
}

/// Generates the alphabetical names for generics based of their identifier.
fn generic_to_alpha(n: GenericId) -> String {
    let mut val = n;
    let mut result = String::new();
    loop {
        result.push((val % 26 + 65) as u8 as char);
        val /= 26;
        if val == 0 {
            break result;
        } else {
            val -= 1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generic_to_alpha_converts_correctly() {
        assert_eq!(generic_to_alpha(0), String::from("A"));
        assert_eq!(generic_to_alpha(1), String::from("B"));
        assert_eq!(generic_to_alpha(25), String::from("Z"));
        assert_eq!(generic_to_alpha(26), String::from("AA"));
        assert_eq!(generic_to_alpha(27), String::from("BA"));
    }
}
