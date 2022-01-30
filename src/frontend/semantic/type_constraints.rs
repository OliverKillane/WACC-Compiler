//! The type constraint struct can be used to do type checking over operators,
//! and allow type coercion. A list of constraining types are given, and types 
//! can be matched against the constraint.
//! 
//! ```
//! assert_eq!(TypeConstraint::new(Type::Int).inside(Type::char), false)
//! assert_eq!(TypeConstraint::new(Type::Array(box Type::Any, 1)).inside(Type::array(box Type::char, 2)), true)
//!```

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

/// Given a unary operator and input type gets the output type.
pub fn get_unop_output_type(unop: &UnOp, input_type: &Type) -> Option<Type> {
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
    (BinOp::Eq, Type::Bool, Type::Generic(0), Type::Generic(0)),
    (BinOp::Ne, Type::Bool, Type::Generic(0), Type::Generic(0)),
    (BinOp::And, Type::Bool, Type::Bool, Type::Bool),
    (BinOp::Or, Type::Bool, Type::Bool, Type::Bool),
];

/// Given a binary operator and both input types gets the output type.
pub fn get_binop_output_type(
    binop: &BinOp,
    left_input_type: &Type,
    right_input_type: &Type,
) -> Option<Type> {
    for (op, out_type, left_in_type, right_in_type) in BINOPS.iter() {
        if binop == op
            && left_in_type.coalesce(left_input_type)
            && right_in_type.coalesce(right_input_type)
        {
            return Some(out_type.clone());
        }
    }
    None
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
            (Type::Any | Type::Generic(_), _) => true,
            (_, Type::Any | Type::Generic(_)) => true,
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

    /// Creates a new type constraint from a list of types
    pub fn new_from_vec(cons: Vec<Type>) -> Self {
        Self(cons)
    }

    /// Creates a new type constraint from a single type
    pub fn new(cons: Type) -> Self {
        Self(vec![cons])
    }

    /// Check if a type is within the constraint.
    ///
    /// If the type will coalesce to any in the constraint then it is 'inside'.
    pub fn inside(&self, check_type: &Type) -> bool {
        self.0.iter().any(|con_type| con_type.coalesce(check_type))
    }

    /// Given a unary operator, and the current constraints:
    /// - If the unary operator output matches the constraints, return the new
    ///   type constraint with its input types.
    /// - If the unary operator does not match, check for all possible
    ///   operators.
    pub fn new_from_unop(&self, unop: &UnOp) -> Result<Self, Self> {
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
    pub fn unop_output_inside(&self, unop: &UnOp) -> bool {
        UNOPS
            .iter()
            .any(|(op, _, out)| op == unop && self.inside(out))
    }

    /// Given the constraint and the input type, get all possible unary
    /// operators that take the input and whose output is inside the constraint.
    pub fn get_possible_unops(&self, input_type: &Type) -> Vec<UnOp> {
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
    pub fn binop_output_inside(&self, binop: &BinOp) -> bool {
        BINOPS
            .iter()
            .any(|(op, out, _, _)| op == binop && self.inside(out))
    }

    /// Given an operators, the concrete input types and the constraint,
    /// determines if a generic definition is required to match, and if so that
    /// the right and left coalesce.
    pub fn binop_generic_check(&self, left_type: &Type, right_type: &Type, binop: &BinOp) -> bool {
        BINOPS.iter().any(|(op, _, left_in, right_in)| {
            op == binop
                && left_type.coalesce(left_in)
                && right_type.coalesce(right_in)
                && if let (Type::Generic(a), Type::Generic(b)) = (left_in, right_in) {
                    a == b && left_type.coalesce(right_type)
                } else {
                    true
                }
        })
    }

    /// Get all possible binary operators that could be applied to the inputs
    /// with an output inside the constraint.
    pub fn get_possible_binops(&self, left_type: &Type, right_type: &Type) -> Vec<BinOp> {
        BINOPS
            .iter()
            .filter_map(|(binop, out, left_in, right_in)| {
                if self.inside(out)
                    && left_type.coalesce(left_in)
                    && right_in.coalesce(right_in)
                    && if let (Type::Generic(a), Type::Generic(b)) = (left_in, right_in) {
                        a == b && left_type.coalesce(right_type)
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
    pub fn index_constraints(&self, level: usize) -> Self {
        TypeConstraint(
            self.0
                .iter()
                .map(|t| t.clone().increase_index_depth(level))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_constraint_can_detect_inside() {
        let type_cons = TypeConstraint(vec![Type::Int, Type::Char]);

        assert!(type_cons.inside(&Type::Int));
        assert!(type_cons.inside(&Type::Char));

        // Any can match any type
        assert!(type_cons.inside(&Type::Any));
        assert!(type_cons.inside(&Type::Generic(0)));

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
        assert!(type_cons.inside(&Type::Generic(0)));
        assert!(type_cons.inside(&Type::Any));
        assert!(type_cons.inside(&Type::Pair(
            box Type::Array(box Type::Int, 2),
            box Type::Generic(0)
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
        assert_eq!(type_cons.inside(&Type::Generic(0)), false);
        assert_eq!(type_cons.inside(&Type::Any), false);
        assert_eq!(
            type_cons.inside(&Type::Pair(
                box Type::Array(box Type::Int, 2),
                box Type::Generic(0)
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
        assert!(Type::Any.coalesce(&Type::Generic(0)));
        assert!(Type::Generic(0).coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Array(box Type::Int, 1)));
        assert!(Type::Array(box Type::Int, 1).coalesce(&Type::Any));
        assert!(Type::Any.coalesce(&Type::Any));
    }

    #[test]
    fn type_coalesce_generic_type_matches_itself() {
        assert!(Type::Generic(0).coalesce(&Type::Generic(0)));
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
            box Type::Pair(box Type::Generic(0), box Type::Int),
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
}