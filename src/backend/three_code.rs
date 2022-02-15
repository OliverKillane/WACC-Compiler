use super::Options;
use crate::intermediate::*;

pub(super) struct ThreeCode;

impl Into<ThreeCode> for (&Program, &Options) {
    fn into(self) -> ThreeCode {
        let (program, options) = self;
        todo!()
    }
}
