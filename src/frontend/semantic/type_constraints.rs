//! A module containing functions for checking type coercions are valid, as
//! well as binary and unary operations.
//!
//! The module is intended

use std::{cmp::Ordering, collections::HashMap};

use super::super::ast::{BinOp, GenericId, Type, UnOp};

lazy_static! {
    /// Unary Operations allowed (operator, output type, input type)
    /// - To add overloading, simply add more tuples
    /// - Operator application will use first tuple to match
    /// - Handled by the type constrain system when analysing expressions
    static ref UNOPS: [(UnOp, Type, Type); 7] = [
        (UnOp::Neg, Type::Bool, Type::Bool),
        (UnOp::Minus, Type::Int, Type::Int),
        (UnOp::Len, Type::Int, Type::Array(box Type::Any, 1)),
        (UnOp::Ord, Type::Int, Type::Char),
        (UnOp::Chr, Type::Char, Type::Int),
        (UnOp::Fst, Type::Generic(0), Type::Pair(box Type::Generic(0), box Type::Any)),
        (UnOp::Snd, Type::Generic(0), Type::Pair(box Type::Any, box Type::Generic(0))),
    ];
}

lazy_static! {
    /// Binary operations allowed (operator, output, left input, right input)
    /// - To add overloading, simply add more tuples
    /// - Operator application will use first tuple to match
    /// - Handled by the type constrain system when analysing expressions
    static ref BINOPS: [(BinOp, Type, Type, Type); 18] = [
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
        (BinOp::Eq, Type::Bool, Type::Generic(0), Type::Generic(0)),
        (BinOp::Ne, Type::Bool, Type::Generic(0), Type::Generic(0)),
        (BinOp::And, Type::Bool, Type::Bool, Type::Bool),
        (BinOp::Or, Type::Bool, Type::Bool, Type::Bool),
        (BinOp::Newpair, Type::Pair(box Type::Generic(0), box Type::Generic(1)), Type::Generic(0), Type::Generic(1)),
    ];
}

/// Traverse through types until a match or not can be discerned. If generic is
/// used, generic matches will result in new concrete types for the generic.
///
/// The check type should never be generic as this is only use by operators.
///
/// can define the concrete types of generics in the current context.
///
/// (must match, type to match) -> Result(Correct concrete type, wrong - should have been type)
///
/// As a precondition the check type must not be generic, it must be a concrete
/// type!
fn type_match(
    type_truth: &Type,
    check_type: &Type,
    generics: &mut HashMap<GenericId, Type>,
) -> Result<Type, Type> {
    match (type_truth, check_type) {
        // Any type is acceptable
        (Type::Any, t) => Ok(t.clone()),

        // Type must match a generic (either add to generics or if already define
        // match against it.)
        (Type::Generic(a), t) => match generics.get(a) {
            Some(conc_type) => {
                let gen_type = conc_type.clone();
                match type_match(&gen_type, t, generics) {
                    Ok(conc_type) => Ok(conc_type),
                    Err(t) => Err(t),
                }
            }
            None => {
                generics.insert(*a, t.clone());
                Ok(t.clone())
            }
        },

        // When matched with any, any generics contained inside must be updated.
        (Type::Pair(box a, box b), Type::Any) => {
            type_match(a, &Type::Any, generics).expect("Any must match anything");
            type_match(b, &Type::Any, generics).expect("Any must match anything");
            Ok(Type::Any)
        }

        // When matched with an any, any generics contained inside must be updated.
        (Type::Array(box a, _), Type::Any) => {
            type_match(a, &Type::Any, generics).expect("Any must match anything");
            Ok(Type::Any)
        }

        // If the type is Any, then is matches, but is not narrowed.
        (_, Type::Any) => Ok(Type::Any),

        // Checking inside of arrays and pairs
        (Type::Pair(box a1, box a2), Type::Pair(box b1, box b2)) => {
            match (type_match(a1, b1, generics), type_match(a2, b2, generics)) {
                (Err(t1), Err(t2)) => Err(Type::Pair(box t1, box t2)),
                (Err(t1), Ok(a2_conc)) => Err(Type::Pair(box t1, box a2_conc)),
                (Ok(a1_conc), Err(t2)) => Err(Type::Pair(box a1_conc, box t2)),
                (Ok(a1_conc), Ok(a2_conc)) => Ok(Type::Pair(box a1_conc, box a2_conc)),
            }
        }
        (Type::Array(box a, dim_a), Type::Array(box b, dim_b)) => match dim_a.cmp(dim_b) {
            Ordering::Equal => match type_match(a, b, generics) {
                Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_b)),
                Err(t) => Err(Type::Array(box t, *dim_a)),
            },
            Ordering::Less => {
                match type_match(a, &Type::Array(box b.clone(), dim_b - dim_a), generics) {
                    Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_a)),
                    Err(t) => Err(Type::Array(box t, *dim_a)),
                }
            }
            Ordering::Greater => {
                match type_match(&Type::Array(box a.clone(), dim_a - dim_b), b, generics) {
                    Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_b)),
                    Err(t) => Err(Type::Array(box t, *dim_b)),
                }
            }
        },

        (Type::String, Type::Array(box Type::Char, 1)) => Ok(Type::String),
        // Primitive types must be equal
        (a, b) => {
            if a == b {
                Ok(b.clone())
            } else {
                Err(a.clone())
            }
        }
    }
}

/// Use the provided generics hashmap to set concrete values for all generic
/// types in the type.
fn apply_generics(generic_type: &Type, generics: &HashMap<GenericId, Type>) -> Type {
    match generic_type {
        Type::Generic(n) => generics
            .get(n)
            .expect("Generic was added to the hashmap, and should still be there")
            .clone(),
        Type::Pair(box a, box b) => Type::Pair(
            box apply_generics(a, generics),
            box apply_generics(b, generics),
        ),
        Type::Array(box a, n) => Type::Array(box apply_generics(a, generics), *n),
        t => t.clone(),
    }
}

type PossibleTypes = Vec<(Type, Type)>;

/// Match a binary operator and two input types.
/// - On success, return the output type
/// - On failure output the potential input types for the operator, and
///   potential operators for the input type.
pub fn binop_match(
    binop: &BinOp,
    left_type: &Type,
    right_type: &Type,
) -> Result<Type, (PossibleTypes, Vec<&'static BinOp>)> {
    let mut generics = HashMap::with_capacity(0);
    let mut possible_types = Vec::new();
    let mut possible_binops = Vec::new();

    for (op, output, left, right) in BINOPS.iter() {
        generics.clear();
        if binop == op {
            match (
                type_match(left, left_type, &mut generics),
                type_match(right, right_type, &mut generics),
            ) {
                (Ok(_), Ok(_)) => return Ok(apply_generics(output, &generics)),
                _ => possible_types.push((left.clone(), right.clone())),
            };
        } else if type_match(left, left_type, &mut generics).is_ok()
            && type_match(right, right_type, &mut generics).is_ok()
        {
            possible_binops.push(op)
        }
    }
    Err((possible_types, possible_binops))
}

/// Match a unary operator and two input types.
/// - On success, return the output type
/// - On failure output the potential input types for the operator, and
///   potential operators for the input type.
pub fn unop_match(unop: &UnOp, expr_type: &Type) -> Result<Type, (Vec<Type>, Vec<&'static UnOp>)> {
    let mut generics = HashMap::with_capacity(0);
    let mut possible_types = Vec::new();
    let mut possible_unops = Vec::new();
    for (op, out, input) in UNOPS.iter() {
        generics.clear();
        if op == unop {
            match type_match(input, expr_type, &mut generics) {
                Ok(_) => return Ok(apply_generics(out, &generics)),
                Err(t) => possible_types.push(t),
            }
        } else if type_match(input, expr_type, &mut generics).is_ok() {
            possible_unops.push(op)
        }
    }
    Err((possible_types, possible_unops))
}

/// Given a type returns:
/// - The de-indexed type (dimension reduced by dim)
/// - None signifies that the type could not be deindexed
///
/// e.g
/// ```text
/// int[][] a = [b,c,d];
/// a[1][2] = 9; # De-Index by 2, and the types are still valid.
/// ```
pub fn de_index(t: &Type, dim: usize) -> Option<Type> {
    match t {
        Type::Array(box t_inner, n) => match n.cmp(&dim) {
            Ordering::Less => None,
            Ordering::Equal => Some(t_inner.clone()),
            Ordering::Greater => Some(Type::Array(box t_inner.clone(), n - dim)),
        },
        _ => None,
    }
}

/// Given two concrete (non-generic) types, can the second be coerced into the
/// first.
pub fn can_coerce(main_type: &Type, into_type: &Type) -> bool {
    match (main_type, into_type) {
        (Type::Any, _) | (_, Type::Any) => true,
        (Type::String, Type::Array(box Type::Char, 1)) => true,
        (Type::Pair(box a1, box a2), Type::Pair(box b1, box b2)) => {
            can_coerce(a1, b1) && can_coerce(a2, b2)
        }
        (Type::Array(box a, dim_a), Type::Array(box b, dim_b)) => match dim_a.cmp(dim_b) {
            Ordering::Less => can_coerce(a, &Type::Array(box b.clone(), dim_b - dim_a)),
            Ordering::Equal => can_coerce(a, b),
            Ordering::Greater => can_coerce(&Type::Array(box a.clone(), dim_a - dim_b), b),
        },
        (a, b) => a == b,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_match_matches_primitives() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(&Type::Int, &Type::Int, &mut generics),
            Ok(Type::Int)
        );
        assert_eq!(
            type_match(&Type::Char, &Type::Char, &mut generics),
            Ok(Type::Char)
        );
        assert_eq!(
            type_match(&Type::Bool, &Type::Bool, &mut generics),
            Ok(Type::Bool)
        );
        assert_eq!(
            type_match(&Type::String, &Type::String, &mut generics),
            Ok(Type::String)
        );
    }

    #[test]
    fn type_match_does_not_match_different_primitives() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(&Type::Int, &Type::Char, &mut generics),
            Err(Type::Int)
        );
        assert_eq!(
            type_match(&Type::Char, &Type::Bool, &mut generics),
            Err(Type::Char)
        );
        assert_eq!(
            type_match(&Type::Bool, &Type::String, &mut generics),
            Err(Type::Bool)
        );
        assert_eq!(
            type_match(&Type::String, &Type::Int, &mut generics),
            Err(Type::String)
        );
    }

    #[test]
    fn type_match_matches_pairs() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(
                &Type::Pair(box Type::Int, box Type::Int),
                &Type::Pair(box Type::Int, box Type::Int),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Int, box Type::Int))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Char, box Type::Char),
                &Type::Pair(box Type::Char, box Type::Char),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Char, box Type::Char))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Bool, box Type::Bool),
                &Type::Pair(box Type::Bool, box Type::Bool),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Bool, box Type::Bool))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::String, box Type::String),
                &Type::Pair(box Type::String, box Type::String),
                &mut generics
            ),
            Ok(Type::Pair(box Type::String, box Type::String))
        );

        assert_eq!(
            type_match(
                &Type::Pair(box Type::Int, box Type::String),
                &Type::Pair(box Type::Int, box Type::String),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Int, box Type::String))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Char, box Type::String),
                &Type::Pair(box Type::Char, box Type::String),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Char, box Type::String))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Char, box Type::Bool),
                &Type::Pair(box Type::Char, box Type::Bool),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Char, box Type::Bool))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Bool, box Type::Int),
                &Type::Pair(box Type::Bool, box Type::Int),
                &mut generics
            ),
            Ok(Type::Pair(box Type::Bool, box Type::Int))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::String, box Type::Bool),
                &Type::Pair(box Type::String, box Type::Bool),
                &mut generics
            ),
            Ok(Type::Pair(box Type::String, box Type::Bool))
        );
    }

    #[test]
    fn type_match_does_not_match_different_nested_pairs() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Int,
                    box Type::Pair(box Type::Int, box Type::String)
                ),
                &Type::Pair(
                    box Type::Char,
                    box Type::Pair(box Type::Bool, box Type::Int)
                ),
                &mut generics
            ),
            Err(Type::Pair(
                box Type::Int,
                box Type::Pair(box Type::Int, box Type::String)
            ))
        );
        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Char,
                    box Type::Pair(box Type::Bool, box Type::Int)
                ),
                &Type::Pair(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Pair(box Type::Int, box Type::String)
                    ),
                    box Type::Bool
                ),
                &mut generics
            ),
            Err(Type::Pair(
                box Type::Char,
                box Type::Pair(box Type::Bool, box Type::Int)
            ))
        );
        assert_eq!(
            type_match(
                &Type::Pair(box Type::Char, box Type::Bool),
                &Type::Pair(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Pair(box Type::Int, box Type::String)
                    ),
                    box Type::Bool
                ),
                &mut generics
            ),
            Err(Type::Pair(box Type::Char, box Type::Bool))
        );
        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Bool,
                    box Type::Pair(
                        box Type::Int,
                        box Type::Pair(box Type::Int, box Type::String)
                    )
                ),
                &Type::Pair(
                    box Type::Int,
                    box Type::Pair(box Type::Int, box Type::String)
                ),
                &mut generics
            ),
            Err(Type::Pair(
                box Type::Bool,
                box Type::Pair(
                    box Type::Int,
                    box Type::Pair(box Type::Int, box Type::String)
                )
            ))
        );
        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Pair(box Type::Int, box Type::String)
                    ),
                    box Type::Bool
                ),
                &Type::Pair(
                    box Type::Char,
                    box Type::Pair(box Type::Bool, box Type::Int)
                ),
                &mut generics
            ),
            Err(Type::Pair(
                box Type::Pair(
                    box Type::Int,
                    box Type::Pair(box Type::Int, box Type::String)
                ),
                box Type::Bool
            ))
        );
    }

    #[test]
    fn type_matches_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(
                &Type::Array(box Type::Int, 3),
                &Type::Array(box Type::Int, 3),
                &mut generics
            ),
            Ok(Type::Array(box Type::Int, 3))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Char, 2),
                &Type::Array(box Type::Char, 2),
                &mut generics
            ),
            Ok(Type::Array(box Type::Char, 2))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::String, 2),
                &Type::Array(box Type::String, 2),
                &mut generics
            ),
            Ok(Type::Array(box Type::String, 2))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2),
                &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Pair(box Type::Int, box Type::String),
                2
            ))
        );
        assert_eq!(
            type_match(
                &Type::Array(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                    ),
                    2
                ),
                &Type::Array(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                    ),
                    2
                ),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Pair(
                    box Type::Int,
                    box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                ),
                2
            ))
        );
    }

    #[test]
    fn type_matches_same_but_differently_nested_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(
                &Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1),
                &Type::Array(box Type::Int, 3),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Array(box Type::Array(box Type::Int, 1), 1),
                1
            ))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Array(box Type::Int, 2), 1),
                &Type::Array(box Type::Int, 3),
                &mut generics
            ),
            Ok(Type::Array(box Type::Array(box Type::Int, 2), 1))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Int, 3),
                &Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Array(box Type::Array(box Type::Int, 1), 1),
                1
            ))
        );
    }

    #[test]
    fn type_matches_does_not_match_different_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(
                &Type::Array(box Type::Int, 3),
                &Type::Array(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                    ),
                    2
                ),
                &mut generics
            ),
            Err(Type::Array(box Type::Array(box Type::Int, 1), 2))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Char, 2),
                &Type::Array(box Type::String, 2),
                &mut generics
            ),
            Err(Type::Array(box Type::Char, 2))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::String, 2),
                &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2),
                &mut generics
            ),
            Err(Type::Array(box Type::String, 2))
        );
        assert_eq!(
            type_match(
                &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2),
                &Type::Array(box Type::Char, 2),
                &mut generics
            ),
            Err(Type::Array(
                box Type::Pair(box Type::Int, box Type::String),
                2
            ))
        );
        assert_eq!(
            type_match(
                &Type::Array(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                    ),
                    2
                ),
                &Type::Array(box Type::Int, 3),
                &mut generics
            ),
            Err(Type::Array(
                box Type::Pair(
                    box Type::Int,
                    box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                ),
                2
            ))
        );
    }

    #[test]
    fn type_matches_any_matches_anything() {
        let mut generics = HashMap::new();

        assert_eq!(
            type_match(&Type::Any, &Type::Array(box Type::Int, 3), &mut generics),
            Ok(Type::Array(box Type::Int, 3))
        );
        assert_eq!(
            type_match(&Type::Any, &Type::Array(box Type::Char, 2), &mut generics),
            Ok(Type::Array(box Type::Char, 2))
        );
        assert_eq!(
            type_match(&Type::Any, &Type::Array(box Type::String, 2), &mut generics),
            Ok(Type::Array(box Type::String, 2))
        );
        assert_eq!(
            type_match(
                &Type::Any,
                &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Pair(box Type::Int, box Type::String),
                2
            ))
        );
        assert_eq!(
            type_match(
                &Type::Any,
                &Type::Array(
                    box Type::Pair(
                        box Type::Int,
                        box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                    ),
                    2
                ),
                &mut generics
            ),
            Ok(Type::Array(
                box Type::Pair(
                    box Type::Int,
                    box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)
                ),
                2
            ))
        );
    }

    #[test]
    fn type_match_generics_constrain_types() {
        let mut generics = HashMap::new();

        // The only match is for generic 0 to be an Int
        assert_eq!(
            type_match(
                &Type::Array(box Type::Generic(0), 3),
                &Type::Array(box Type::Int, 3),
                &mut generics
            ),
            Ok(Type::Array(box Type::Int, 3))
        );
        assert_eq!(generics.get(&0), Some(&Type::Int));

        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Array(box Type::Generic(0), 2),
                    box Type::Generic(1)
                ),
                &Type::Pair(
                    box Type::Array(box Type::Array(box Type::Int, 1), 1),
                    box Type::Char
                ),
                &mut generics
            ),
            Ok(Type::Pair(
                box Type::Array(box Type::Array(box Type::Int, 1), 1),
                box Type::Char
            ))
        );
        assert_eq!(generics.get(&1), Some(&Type::Char));

        assert_eq!(
            type_match(
                &Type::Pair(
                    box Type::Array(box Type::Generic(0), 2),
                    box Type::Generic(1)
                ),
                &Type::Pair(
                    box Type::Array(box Type::Array(box Type::Int, 1), 1),
                    box Type::String
                ),
                &mut generics
            ),
            Err(Type::Pair(
                box Type::Array(box Type::Array(box Type::Int, 1), 1),
                box Type::Char
            ))
        );
    }

    #[test]
    fn apply_generics_correct() {
        let generics = HashMap::from([
            (0, Type::Int),
            (1, Type::Char),
            (2, Type::Array(box Type::Char, 3)),
        ]);

        assert_eq!(apply_generics(&Type::Generic(0), &generics), Type::Int);
        assert_eq!(apply_generics(&Type::Generic(1), &generics), Type::Char);
        assert_eq!(
            apply_generics(&Type::Generic(2), &generics),
            Type::Array(box Type::Char, 3)
        );
        assert_eq!(
            apply_generics(
                &Type::Pair(box Type::Generic(1), box Type::Generic(0)),
                &generics
            ),
            Type::Pair(box Type::Char, box Type::Int)
        );
        assert_eq!(
            apply_generics(
                &Type::Pair(box Type::Generic(1), box Type::Generic(2)),
                &generics
            ),
            Type::Pair(box Type::Char, box Type::Array(box Type::Char, 3))
        );
    }

    #[test]
    fn binop_match_matches_correct_applications() {
        assert_eq!(
            binop_match(&BinOp::Add, &Type::Int, &Type::Int),
            Ok(Type::Int)
        );
        assert_eq!(
            binop_match(&BinOp::Gt, &Type::Char, &Type::Char),
            Ok(Type::Bool)
        );
        assert_eq!(
            binop_match(&BinOp::Or, &Type::Bool, &Type::Bool),
            Ok(Type::Bool)
        );
        assert_eq!(
            binop_match(
                &BinOp::Eq,
                &Type::Pair(box Type::Int, box Type::Char),
                &Type::Pair(box Type::Int, box Type::Char)
            ),
            Ok(Type::Bool)
        );
    }

    #[test]
    fn binop_match_correctly_identifies_errors() {
        assert_eq!(
            binop_match(&BinOp::Add, &Type::Int, &Type::Char),
            Err((vec![(Type::Int, Type::Int)], vec![&BinOp::Newpair]))
        );
        assert_eq!(
            binop_match(&BinOp::Eq, &Type::Int, &Type::Char),
            Err((
                vec![(Type::Generic(0), Type::Generic(0))],
                vec![&BinOp::Newpair]
            ))
        );
        assert_eq!(
            binop_match(&BinOp::Lt, &Type::Int, &Type::Char),
            Err((
                vec![(Type::Int, Type::Int), (Type::Char, Type::Char)],
                vec![&BinOp::Newpair]
            ))
        );
    }

    #[test]
    fn unop_match_matches_correct_applications() {
        assert_eq!(
            unop_match(&UnOp::Len, &Type::Array(box Type::Int, 3)),
            Ok(Type::Int)
        );
        assert_eq!(unop_match(&UnOp::Neg, &Type::Bool), Ok(Type::Bool));
        assert_eq!(unop_match(&UnOp::Minus, &Type::Int), Ok(Type::Int));
        assert_eq!(unop_match(&UnOp::Chr, &Type::Int), Ok(Type::Char));
        assert_eq!(unop_match(&UnOp::Ord, &Type::Char), Ok(Type::Int));
    }

    #[test]
    fn unop_match_correctly_identifies_errors() {
        assert_eq!(
            unop_match(&UnOp::Len, &Type::Int),
            Err((
                vec![Type::Array(box Type::Any, 1)],
                vec![&UnOp::Minus, &UnOp::Chr]
            ))
        );
        assert_eq!(
            unop_match(&UnOp::Neg, &Type::Int),
            Err((vec![Type::Bool], vec![&UnOp::Minus, &UnOp::Chr]))
        );
        assert_eq!(
            unop_match(&UnOp::Neg, &Type::Array(box Type::Any, 1)),
            Err((vec![Type::Bool], vec![&UnOp::Len]))
        );
        assert_eq!(
            unop_match(&UnOp::Minus, &Type::Char),
            Err((vec![Type::Int], vec![&UnOp::Ord]))
        );
        assert_eq!(
            unop_match(&UnOp::Chr, &Type::Char),
            Err((vec![Type::Int], vec![&UnOp::Ord]))
        );
        assert_eq!(
            unop_match(&UnOp::Ord, &Type::Int),
            Err((vec![Type::Char], vec![&UnOp::Minus, &UnOp::Chr]))
        );
    }

    #[test]
    fn can_coerce_matches_any_correctly() {
        assert!(can_coerce(&Type::Int, &Type::Any));
        assert!(can_coerce(&Type::Any, &Type::Int));
        assert!(can_coerce(&Type::Any, &Type::String));
        assert!(can_coerce(&Type::String, &Type::Any));
        assert!(can_coerce(&Type::Bool, &Type::Any));
        assert!(can_coerce(&Type::Any, &Type::Bool));
        assert!(can_coerce(&Type::Any, &Type::Char));
        assert!(can_coerce(&Type::Char, &Type::Any));
        assert!(can_coerce(&Type::String, &Type::Array(box Type::Char, 1)));
    }

    #[test]
    fn can_coerce_does_not_match_incorrect_coercions() {
        assert_eq!(
            can_coerce(&Type::Array(box Type::Char, 1), &Type::String),
            false
        );
        assert_eq!(
            can_coerce(
                &Type::Array(box Type::Char, 2),
                &Type::Array(box Type::Char, 3)
            ),
            false
        );
    }

    #[test]
    fn can_coerce_matches_arrays_and_pairs_correctly() {
        assert!(can_coerce(
            &Type::Pair(box Type::Array(box Type::Int, 3), box Type::Char),
            &Type::Pair(box Type::Array(box Type::Any, 2), box Type::Any)
        ));
        assert!(can_coerce(
            &Type::Array(box Type::Char, 3),
            &Type::Array(box Type::Array(box Type::Char, 1), 2)
        ));
        assert!(can_coerce(
            &Type::Array(box Type::Array(box Type::Char, 2), 1),
            &Type::Array(box Type::Array(box Type::Char, 1), 2)
        ));
    }
}
