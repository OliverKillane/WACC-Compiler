use super::ssa::StaticSingleAssignment;

pub(super) struct GeneralAssembly;

impl From<StaticSingleAssignment> for GeneralAssembly {
    fn from(ssa: StaticSingleAssignment) -> GeneralAssembly {
        todo!()
    }
}
