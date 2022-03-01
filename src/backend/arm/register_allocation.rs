//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use'.

use std::cmp::min;

use super::{
    super::super::graph::Graph,
    arm_graph_utils::{simple_node, Chain},
    arm_repr::{
        Cond, ControlFlow, FlexOperand, Ident, RegOp, Register, Stat, Temporary,
    },
};

type Preserved = u128;
type Reserved = i128;

/// A type for each
#[derive(Debug, Clone, PartialEq, Eq)]
enum Alloc {
    Temp(Temporary),
    Preserve(Preserved),
    Free,
    StackReserve,
    Protected,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct AllocationState {
    /// Represents the usable registers:
    /// ```text
    /// [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15]
    /// ```
    /// The following cannot be used:
    /// - R13 contains the stack pointer
    /// - R15 is the program counter
    registers: [Alloc; 16],

    /// Represents the stack, the end of the vector is the current position of
    /// the stack pointer.
    stack: Vec<Alloc>,

    /// Represents the displacement of the stack pointer from when the function
    /// was called.
    sp_displacement: i32,
}

impl AllocationState {
    fn new(
        args: Vec<Temporary>,
        reserved_stack: u8,
        graph: &mut Graph<ControlFlow>,
    ) -> (Self, Option<Chain>) {
        // allocate arguments on the registers r0-r3, then the stack. Reserved
        // space goes after the arguments, and immediately generates an
        // instruction to move the stack pointer offset accordingly

        // Calling convention is encoded within the start state
        let mut registers: [Alloc; 16] = [
            Alloc::Free,        // R0
            Alloc::Free,        // R1
            Alloc::Free,        // R2
            Alloc::Free,        // R3
            Alloc::Preserve(0), // R4
            Alloc::Preserve(1), // R5
            Alloc::Preserve(2), // R6
            Alloc::Preserve(3), // R7
            Alloc::Preserve(4), // R8
            Alloc::Preserve(5), // R9
            Alloc::Preserve(6), // R10
            Alloc::Preserve(7), // R11
            Alloc::Preserve(8), // R12
            Alloc::Protected,   // R13 - SP
            Alloc::Preserve(9), // R14 - LR
            Alloc::Protected,   // R15 - PC
        ];

        // allocate arguments to registers
        for reg in 0..min(args.len(), 4) {
            registers[reg] = Alloc::Temp(args[reg]);
        }

        // allocate remaining arguments on the stack
        let mut stack = Vec::new();
        if args.len() > 4 {
            for arg in args.iter().skip(4) {
                stack.push(Alloc::Temp(*arg))
            }
        }

        let (start_instruction, sp_displacement) = if reserved_stack > 0 {
            for reserved in 0..reserved_stack {
                stack.push(Alloc::StackReserve)
            }

            (
                Some(simple_node(
                    Stat::ApplyOp(
                        RegOp::Sub,
                        Cond::Al,
                        false,
                        Ident::Register(Register::Sp),
                        Ident::Register(Register::Sp),
                        FlexOperand::Imm(reserved_stack as u32),
                    ),
                    graph,
                )),
                reserved_stack as i32,
            )
        } else {
            (None, 0)
        };

        (
            AllocationState {
                registers,
                stack,
                sp_displacement,
            },
            start_instruction,
        )
    }
}
