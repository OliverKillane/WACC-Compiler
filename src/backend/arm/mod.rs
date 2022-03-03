//! Convert the three code graph representation into an arm representation that
//! is writable to a file.

use self::{
    arm_repr::ArmCode, register_allocation::allocate_registers, translation::translate_threecode,
};
use super::three_code::ThreeCode;

mod allocation_state;
pub mod arm_display;
mod arm_graph_utils;
mod arm_repr;
mod int_constraints;
mod live_ranges;
mod register_allocation;
mod translation;

/// Generate strings from both the translated & temporary named, and the final
/// (register allocated) arm representation.
pub(super) fn get_intermediates(three_code: ThreeCode) -> (String, String) {
    let translated = translate_threecode(three_code);
    let temp_string = format!("{}", translated);
    let reg_allocated = allocate_registers(translated);
    let reg_string = format!("{}", reg_allocated);
    (temp_string, reg_string)
}

impl From<ThreeCode> for ArmCode {
    fn from(three_code: ThreeCode) -> Self {
        allocate_registers(translate_threecode(three_code))
    }
}
