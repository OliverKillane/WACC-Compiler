//! Convert the three code graph representation into an arm representation that
//! is writable to a file.
//!
//! Optionally produces a string of the translated (but still containing
//! temporary identifiers) assembly graph.

use self::{
    peephole_opts::remove_self_moves,
    register_allocation::allocate_registers,
    translation::{translate_threecode, validate_connections},
};
use super::{three_code::ThreeCode, Options};

mod allocation_state;
pub mod arm_display;
mod arm_graph_utils;
mod arm_repr;
mod int_constraints;
mod live_ranges;
mod peephole_opts;
mod register_allocation;
mod translation;

// Re-export the ArmCode representation
pub use arm_repr::ArmCode;

pub struct ArmResult(pub ArmCode, pub Option<String>);

impl From<(ThreeCode, &Options)> for ArmResult {
    /// Using the provided options and the three code representation, generate
    /// arm assembly and optionally the assembly using temporaries.
    fn from((three_code, options): (ThreeCode, &Options)) -> Self {
        if options.show_arm_temp_rep {
            let arm_temp = translate_threecode(three_code);
            validate_connections(&arm_temp.cfg);
            let temp_string = arm_temp.to_string();
            ArmResult(
                remove_self_moves(allocate_registers(arm_temp)),
                Some(temp_string),
            )
        } else {
            ArmResult(
                remove_self_moves(allocate_registers(translate_threecode(three_code))),
                None,
            )
        }
    }
}
