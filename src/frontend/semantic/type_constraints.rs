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

impl Type {
    /// If the other type can match the current style, they coalesce.
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

#[cfg(test)]
mod tests {
    use super::*;

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
        assert!(Type::Any.coalesce(&Type::Char));
        assert!(Type::Any.coalesce(&Type::Bool));
        assert!(Type::Any.coalesce(&Type::String));
        assert!(Type::Any.coalesce(&Type::Pair(box Type::Any, box Type::Any)));
        assert!(Type::Any.coalesce(&Type::Array(
            box Type::Pair(box Type::Int, box Type::Char),
            4
        )));
        assert!(Type::Any.coalesce(&Type::Generic(0)));
        assert!(Type::Generic(0).coalesce(&Type::Any,));
        assert!(Type::Any.coalesce(&Type::Array(box Type::Int, 1)));
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
