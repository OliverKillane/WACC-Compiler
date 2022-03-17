//! Constrained integers. Many arm instructions require integers in specific ranges.
//! Hence, we can express said ranges with a type, and assert these ranges
//! during the conversions from one type to another.

/// L is the lower bound, U is the upper bound. Tuple struct contains the
/// represented value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstrainedInt<const L: i32, const U: i32>(i32);

impl<const L: i32, const U: i32> From<i32> for ConstrainedInt<L, U> {
    fn from(x: i32) -> Self {
        assert!((L..=U).contains(&x));
        Self(x)
    }
}

impl<const L: i32, const U: i32> From<ConstrainedInt<L, U>> for i32 {
    fn from(x: ConstrainedInt<L, U>) -> Self {
        x.0
    }
}
