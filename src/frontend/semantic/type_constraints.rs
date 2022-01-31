//! The type constraint struct can be used to do type checking over operators,
//! and allow type coercion. A list of constraining types are given, and types
//! can be matched against the constraint.
//!
//! ```
//! assert_eq!(TypeConstraint::new(Type::Int).inside(Type::char), false)
//! assert_eq!(TypeConstraint::new(Type::Array(box Type::Any, 1)).inside(Type::array(box Type::char, 2)), true)
//!```

use std::collections::HashMap;

use crate::frontend::ast::*;

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
    (BinOp::Eq, Type::Bool, Type::Generic(0), Type::Generic(0)),
    (BinOp::Ne, Type::Bool, Type::Generic(0), Type::Generic(0)),
    (BinOp::And, Type::Bool, Type::Bool, Type::Bool),
    (BinOp::Or, Type::Bool, Type::Bool, Type::Bool),
];

/// Traverse through types until a match or not can be discerned. If generic is
/// used, generic matches will result in new concrete types for the generic.
/// 
/// The check type should never be generic as this is only use by operators.
/// 
/// can define the concrete types of generics in the current context.
/// 
/// (must match, type to match) -> Result(Correct concrete type, wrong - should have been type)
fn type_match(type_truth: &Type, check_type: &Type, generics: &mut HashMap<GenericId, Type>) -> Result<Type, Type> {
    match (type_truth, check_type) {
        // Any matches anything, and can be used for type coersion
        (Type::Any, t) => Ok(t.clone()),
        (_, Type::Any) => Ok(Type::Any),
        (Type::Generic(a), t) => {
            match generics.get(a) {
            Some(conc_type) => {
                let gen_type = conc_type.clone();
                match type_match(&gen_type, t, generics) {
                    Ok(conc_type) => Ok(conc_type),
                    Err(t) => Err(t),
                }
            },
            None => {
                generics.insert(*a, t.clone());
                Ok(t.clone())
            },}
        }
        (Type::Pair(box a1, box a2), Type::Pair(box b1, box b2)) => {
            match (type_match(a1, b1, generics), type_match(a2, b2, generics)) {
                (Err(t1), Err(t2)) => Err(Type::Pair(box t1, box t2)),
                (Err(t1), Ok(a2_conc)) => Err(Type::Pair(box t1, box a2_conc)),
                (Ok(a1_conc), Err(t2)) => Err(Type::Pair(box a1_conc, box t2)),
                (Ok(a1_conc), Ok(a2_conc)) => Ok(Type::Pair(box a1_conc, box a2_conc)),
            }
        }
        (Type::Array(box a, dim_a), Type::Array(box b, dim_b)) => {
            if dim_a == dim_b {
                match type_match(a, b, generics) {
                    Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_b)),
                    Err(t) => Err(Type::Array(box t, *dim_a)),
                }
            } else if dim_a > dim_b {
                match type_match(&Type::Array(box a.clone(), dim_a - dim_b), b, generics) {
                    Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_b)),
                    Err(t) => Err(Type::Array(box t, *dim_b)),
                }
            } else {
                match type_match(a, &Type::Array(box b.clone(), dim_b - dim_a), generics) {
                    Ok(a_conc) => Ok(Type::Array(box a_conc, *dim_a)),
                    Err(t) => Err(Type::Array(box t, *dim_a)),
                }
            }
        }
        (a, b) => if a == b {
            Ok(b.clone())
        } else {
            Err(a.clone())
        },
    }
}


pub fn binop_match(binop: &BinOp, left_type: &Type, right_type: &Type) -> Result<Type, (Vec<(Type, Type)>, Vec<BinOp>)> {
    todo!()
}

pub fn binop_match_left(binop: &BinOp, left_type: &Type) -> Result<Type, (Vec<Type>, Vec<BinOp>)> {
    todo!()
}

pub fn binop_match_right(binop: &BinOp, right_type: &Type) -> Result<Type, (Vec<Type>, Vec<BinOp>)> {
    todo!()
}

pub fn unop_match(unop: &UnOp, expr_type: &Type) -> Result<Type, (Vec<Type>, Vec<UnOp>)> {
    todo!()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_match_matches_primitives() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Int, &Type::Int, &mut generics), Ok(Type::Int));
        assert_eq!(type_match(&Type::Char, &Type::Char, &mut generics), Ok(Type::Char));
        assert_eq!(type_match(&Type::Bool, &Type::Bool, &mut generics), Ok(Type::Bool));
        assert_eq!(type_match(&Type::String, &Type::String, &mut generics), Ok(Type::String));
    }

    #[test]
    fn type_match_does_not_match_different_primitives() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Int, &Type::Char, &mut generics), Err(Type::Int));
        assert_eq!(type_match(&Type::Char, &Type::Bool, &mut generics), Err(Type::Char));
        assert_eq!(type_match(&Type::Bool, &Type::String, &mut generics), Err(Type::Bool));
        assert_eq!(type_match(&Type::String, &Type::Int, &mut generics), Err(Type::String));
    }

    #[test]
    fn type_match_matches_pairs() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Pair(box Type::Int, box Type::Int), &Type::Pair(box Type::Int, box Type::Int), &mut generics), Ok(Type::Pair(box Type::Int, box Type::Int)));
        assert_eq!(type_match(&Type::Pair(box Type::Char, box Type::Char), &Type::Pair(box Type::Char, box Type::Char), &mut generics), Ok(Type::Pair(box Type::Char, box Type::Char )));
        assert_eq!(type_match(&Type::Pair(box Type::Bool, box Type::Bool), &Type::Pair(box Type::Bool, box Type::Bool), &mut generics), Ok(Type::Pair(box Type::Bool, box Type::Bool)));
        assert_eq!(type_match(&Type::Pair(box Type::String, box Type::String), &Type::Pair(box Type::String, box Type::String ), &mut generics), Ok(Type::Pair(box Type::String, box Type::String)));

        assert_eq!(type_match(&Type::Pair(box Type::Int, box Type::String), &Type::Pair(box Type::Int, box Type::String), &mut generics), Ok(Type::Pair(box Type::Int, box Type::String)));
        assert_eq!(type_match(&Type::Pair(box Type::Char, box Type::String), &Type::Pair(box Type::Char, box Type::String), &mut generics), Ok(Type::Pair(box Type::Char, box Type::String )));
        assert_eq!(type_match(&Type::Pair(box Type::Char, box Type::Bool), &Type::Pair(box Type::Char, box Type::Bool), &mut generics), Ok(Type::Pair(box Type::Char, box Type::Bool )));
        assert_eq!(type_match(&Type::Pair(box Type::Bool, box Type::Int), &Type::Pair(box Type::Bool, box Type::Int), &mut generics), Ok(Type::Pair(box Type::Bool, box Type::Int)));
        assert_eq!(type_match(&Type::Pair(box Type::String, box Type::Bool), &Type::Pair(box Type::String, box Type::Bool ), &mut generics), Ok(Type::Pair(box Type::String, box Type::Bool)));
    }

    #[test]
    fn type_match_does_not_match_different_nested_pairs() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), &Type::Pair(box Type::Char, box Type::Pair(box Type::Bool, box Type::Int)), &mut generics), Err(Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String))));
        assert_eq!(type_match(&Type::Pair(box Type::Char, box Type::Pair(box Type::Bool, box Type::Int)), &Type::Pair(box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), box Type::Bool ), &mut generics), Err(Type::Pair(box Type::Char, box Type::Pair(box Type::Bool, box Type::Int))));
        assert_eq!(type_match(&Type::Pair(box Type::Char, box Type::Bool), &Type::Pair(box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), box Type::Bool), &mut generics), Err(Type::Pair(box Type::Char, box Type::Bool )));
        assert_eq!(type_match(&Type::Pair(box Type::Bool, box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String))), &Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), &mut generics), Err(Type::Pair(box Type::Bool, box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)))));
        assert_eq!(type_match(&Type::Pair(box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), box Type::Bool), &Type::Pair(box Type::Char, box Type::Pair(box Type::Bool, box Type::Int)), &mut generics), Err(Type::Pair(box Type::Pair(box Type::Int, box Type::Pair(box Type::Int, box Type::String)), box Type::Bool)));
    }

    #[test]
    fn type_matches_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Array(box Type::Int, 3) , &Type::Array(box Type::Int, 3), &mut generics), Ok(Type::Array(box Type::Int, 3)));
        assert_eq!(type_match(&Type::Array(box Type::Char, 2) , &Type::Array(box Type::Char, 2), &mut generics), Ok(Type::Array(box Type::Char, 2)));
        assert_eq!(type_match(&Type::Array(box Type::String, 2) , &Type::Array(box Type::String, 2), &mut generics), Ok(Type::Array(box Type::String, 2)));
        assert_eq!(type_match(&Type::Array(box Type::Pair(box Type::Int, box Type::String), 2) , &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2), &mut generics), Ok(Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)));
        assert_eq!(type_match(&Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2) , &Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2), &mut generics), Ok(Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2)));
    }

    #[test]
    fn type_matches_same_but_differently_nested_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1) , &Type::Array(box Type::Int, 3), &mut generics), Ok(Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1) ));
        assert_eq!(type_match(&Type::Array(box Type::Array(box Type::Int, 2), 1) , &Type::Array(box Type::Int, 3), &mut generics), Ok(Type::Array(box Type::Array(box Type::Int, 2), 1)));
        assert_eq!(type_match(&Type::Array(box Type::Int, 3) , &Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1), &mut generics), Ok(Type::Array(box Type::Array(box Type::Array(box Type::Int, 1), 1), 1) ));
    }

    #[test]
    fn type_matches_does_not_match_different_arrays() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Array(box Type::Int, 3) , &Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2), &mut generics), Err(Type::Array(box Type::Array(box Type::Int, 1), 2)));
        assert_eq!(type_match(&Type::Array(box Type::Char, 2) , &Type::Array(box Type::String, 2), &mut generics), Err(Type::Array(box Type::Char, 2)));
        assert_eq!(type_match(&Type::Array(box Type::String, 2) , &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2), &mut generics), Err(Type::Array(box Type::String, 2)));
        assert_eq!(type_match(&Type::Array(box Type::Pair(box Type::Int, box Type::String), 2) , &Type::Array(box Type::Char, 2), &mut generics), Err(Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)));
        assert_eq!(type_match(&Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2) , &Type::Array(box Type::Int, 3), &mut generics), Err(Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2)));
    }

    #[test]
    fn type_matches_any_matches_anything() {
        let mut generics = HashMap::new();

        assert_eq!(type_match(&Type::Any , &Type::Array(box Type::Int, 3), &mut generics), Ok(Type::Array(box Type::Int, 3)));
        assert_eq!(type_match(&Type::Any, &Type::Array(box Type::Char, 2), &mut generics), Ok(Type::Array(box Type::Char, 2)));
        assert_eq!(type_match(&Type::Any, &Type::Array(box Type::String, 2), &mut generics), Ok(Type::Array(box Type::String, 2)));
        assert_eq!(type_match(&Type::Any, &Type::Array(box Type::Pair(box Type::Int, box Type::String), 2), &mut generics), Ok(Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)));
        assert_eq!(type_match(&Type::Any, &Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2), &mut generics), Ok(Type::Array(box Type::Pair(box Type::Int, box Type::Array(box Type::Pair(box Type::Int, box Type::String), 2)), 2)));
    }

    #[test]
    fn type_match_generics_constrain_types() {
        let mut generics = HashMap::new();

        // The only match is for generic 0 to be an Int
        assert_eq!(type_match(&Type::Array(box Type::Generic(0), 3) , &Type::Array(box Type::Int, 3), &mut generics), Ok(Type::Array(box Type::Int, 3)));
        assert_eq!(generics.get(&0), Some(&Type::Int));

        assert_eq!(type_match(&Type::Pair(box Type::Array(box Type::Generic(0), 2), box Type::Generic(1)), &Type::Pair(box Type::Array(box Type::Array(box Type::Int, 1), 1), box Type::Char), &mut generics), Ok(Type::Pair(box Type::Array(box Type::Array(box Type::Int, 1), 1), box Type::Char)));
        assert_eq!(generics.get(&1), Some(&Type::Char));

        assert_eq!(type_match(&Type::Pair(box Type::Array(box Type::Generic(0), 2), box Type::Generic(1)), &Type::Pair(box Type::Array(box Type::Array(box Type::Int, 1), 1), box Type::String), &mut generics), Err(Type::Pair(box Type::Array(box Type::Array(box Type::Int, 1), 1), box Type::Char)));
    }
}
