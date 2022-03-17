//! Translate the [threecode](ThreeCode) representation of the program into
//! valid arm assembly.
//!
//! Translation occurs in five stages:
//! 1. Threecode is translated to arm representation with special nodes (call,
//!    return), and all registers as temporary identifiers.
//! 2. Live range analysis is performed on the arm with temporaries, to get
//!    livein & liveout for all nodes, as well as the 'instructions till next use'
//!    for each temporary to use in spilling.
//! 3. The arm representation has register allocated (all temporaries replaced),
//!    all spilling/sipping from the stack frame is performed and special instructions
//!    (call, return etc) are translated into series of arm instructions.
//! 4. Apply peephole optimisations (removing self moves).
//! 5. The final arm representation is traversed, and converted from a graph to a
//!    linear representation in text. Labels are added before nodes appropriately.
//!
//! This approach allows for efficient register allocation, without having to do
//! full graph colouring. And allows for further analysis on the arm assembly as
//! a graph (e.g for peephole optimisations).
//!
//! Furthermore every stage of this process is printable (temporaries, final
//! arm, and between), which makes it significantly easier to debug.

use self::{register_allocation::allocate_registers, translation::translate_threecode};
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
            let temp_string = arm_temp.to_string();
            ArmResult(allocate_registers(arm_temp), Some(temp_string))
        } else {
            ArmResult(allocate_registers(translate_threecode(three_code)), None)
        }
    }
}
