//! Convert the three code graph representation into an arm representation that 
//! is writable to a file.

use self::{arm_repr::Program, translation::translate_threecode, register_allocation::allocate_registers};
use super::three_code::ThreeCode;

pub mod arm_display;
mod arm_graph_utils;
mod arm_repr;
mod int_constraints;
mod live_ranges;
mod register_allocation;
mod translation;


impl From<ThreeCode> for Program {
    fn from(three_code: ThreeCode) -> Self {
        allocate_registers(translate_threecode(three_code))
    }
}