//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use'.

use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
};

use lazy_static::__Deref;

use crate::backend::arm::{
    arm_graph_utils::chain_two_chains,
    arm_repr::{FlexOffset, MemOp, MemOperand},
};

use super::{
    super::super::graph::Graph,
    arm_graph_utils::{is_shifted_8_bit, link_stats, simple_node, Chain},
    arm_repr::{ArmNode, Cond, ControlFlow, FlexOperand, Ident, RegOp, Register, Stat, Temporary},
    live_ranges::LiveRanges,
};

type Preserved = u128;
type Reserved = i128;

/// A type for each part of the registers and stack,
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Alloc {
    /// A temporary variable (in register/stack)
    Temp(Temporary),
    /// A value that must be preserved (register/stack)
    Preserve(Preserved),
    /// Indicates a position is free for use.
    Free,
    /// Indicates a stack word is reserved
    StackReserve,
    /// Space is protected, do not use! (register/stack)
    Protected,
}

/// Tracks the state of the registers, and the stack frame.
/// Stack frame is composed of:
///
/// |            | Stack Contents         |
/// |------------|------------------------|
/// |            | (Temp) Arg 4           |
/// |            | (Temp) Arg 5           |
/// |            | (Temp) Arg 6           |
/// |            | (Temp) Arg 7           |
/// |            | ...                    |
/// | SP at Call | Temps                  |
/// |            | ...                    |
/// |            | preserved register R4  |
/// |            | preserved register R5  |
/// |            | ...                    |
/// |            | preserved register R14 |
/// |            | reserved stack space   |
/// |            | reserved stack space   |
/// |            | ...                    |
/// |------------|------------------------|
#[derive(Debug, Clone)]
struct AllocationState {
    /// Represents the usable registers:
    /// ```text
    /// [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13 - SP, r14 - LR, r15 - PC]
    /// ```
    /// The following cannot be used:
    /// - R13 contains the stack pointer
    /// - R15 is the program counter
    registers: [Alloc; 16],

    /// Position in words from the end of the frame, to the allocated space for
    /// the identifier.
    alloc_map: HashMap<Alloc, i32>,

    /// The displacement of the stack pointer from the the start of the call.
    sp_displacement: i32,
}

impl AllocationState {
    /// Creates a stack frame and initial state
    fn create_stack_frame(
        args: &[Temporary],
        reserved_stack: u8,
        temps_set: &HashSet<Temporary>,
        graph: &mut Graph<ControlFlow>,
    ) -> (Self, Option<Chain>) {
        // allocate arguments on the registers r0-r3, then the stack. Reserved
        // space goes after the arguments, and immediately generates an
        // instruction to move the stack pointer offset accordingly

        // First we can calculate the frame size in words (4 bytes) as:
        // Number of temporaries (need a space on stack)
        // Number of protected registers (need a space if pushed)
        // Reserved stack space

        let mut alloc_map = HashMap::new();

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

        // place the first 3 arguments in registers.
        for reg in 0..(args.len().min(4)) {
            registers[reg] = Alloc::Temp(args[reg])
        }

        // now put all other arguments onto the stack. These will be just
        // before the stack pointer in reverse order (were pushed and popped)
        for (arg_ind, temp) in args.iter().enumerate().skip(4) {
            alloc_map.insert(Alloc::Temp(*temp), (args.len() - arg_ind) as i32);
        }

        // Now push all temporaries to the map to fill the frame. If it is
        // already in the map (it is a stack argument) we ignore.
        let mut sp_displacement = 0;
        for temp in temps_set {
            let temp_alloc = Alloc::Temp(*temp);
            if alloc_map.try_insert(temp_alloc, sp_displacement).is_ok() {
                sp_displacement += 1;
            }
        }

        // setup space for preserves
        for p in 0..9 {
            alloc_map.insert(Alloc::Preserve(p), sp_displacement);
            sp_displacement += 1;
        }

        // setup reserved stack space
        alloc_map.insert(Alloc::StackReserve, sp_displacement);
        sp_displacement += reserved_stack as i32;

        // generate stack pointer move:
        let nodes = if sp_displacement != 0 {
            let byte_displacement = sp_displacement * 4;
            if is_shifted_8_bit(byte_displacement) {
                Some(simple_node(
                    Stat::ApplyOp(
                        RegOp::Sub,
                        Cond::Al,
                        false,
                        Ident::Register(Register::Sp),
                        Ident::Register(Register::Sp),
                        FlexOperand::Imm(byte_displacement as u32),
                    ),
                    graph,
                ))
            } else {
                Some(link_stats(
                    vec![
                        Stat::Push(Cond::Al, vec![Ident::Register(Register::R4)]),
                        Stat::MemOp(
                            MemOp::Ldr,
                            Cond::Al,
                            false,
                            Ident::Register(Register::R4),
                            MemOperand::Expression(byte_displacement),
                        ),
                        Stat::ApplyOp(
                            RegOp::Sub,
                            Cond::Al,
                            false,
                            Ident::Register(Register::Sp),
                            Ident::Register(Register::Sp),
                            FlexOperand::ShiftReg(Ident::Register(Register::Sp), None),
                        ),
                    ],
                    graph,
                ))
                .expect("More than one statement")
            }
        } else {
            None
        };

        (
            Self {
                registers,
                alloc_map,
                sp_displacement,
            },
            nodes,
        )
    }

    /// Mark the space used for any variable not used as free, place all use
    /// temporaries in registers.
    /// Note: temps is sorted for 'wait longest till use' and the end.
    fn update_live(&mut self, temps: &[Temporary]) {
        for reg in self.registers.iter_mut() {
            if let Alloc::Temp(t) = reg {
                if !temps.contains(t) {
                    *reg = Alloc::Free
                }
            }
        }
    }

    /// Use the allocation map to get the displacement of the stack space for a
    /// given allocation.
    fn get_temporary_sp_offset(&self, alloc: &Alloc) -> i32 {
        (self.sp_displacement
            - self
                .alloc_map
                .get(alloc)
                .expect("all valid allocs are in the allocation map"))
            * 4
    }

    /// A utility function from moving data to and from the stack (offsets may
    /// be too large for immediate operand use)
    fn move_reg_stack(
        &self,
        dst_register: Register,
        alloc: &Alloc,
        memop: MemOp,
        mut free_registers: Vec<usize>,
        graph: &mut Graph<ControlFlow>,
    ) -> Chain {
        let sp_offset = self.get_temporary_sp_offset(alloc);
        if -4095 <= sp_offset && sp_offset <= 4095 {
            // we can use a stack pointer offset to get the temporary

            // LDR reg_ind, [Sp, #offset]

            simple_node(
                Stat::MemOp(
                    memop,
                    Cond::Al,
                    false,
                    Ident::Register(dst_register),
                    MemOperand::PreIndex(
                        Ident::Register(Register::Sp),
                        FlexOffset::Expr((sp_offset as i32).into()),
                        false,
                    ),
                ),
                graph,
            )
        } else if !free_registers.is_empty() {
            // the offset is too large, so we can load as a label into another (free) register, then use that to address
            let tmp_register = Ident::Register(Register::from_ind(
                free_registers
                    .pop()
                    .expect("vector of free registers is not empty"),
            ));

            // LDR tmp_reg, =offset
            // ADD tmp_reg, tmp_reg, SP
            // LDR reg_ind, [tmp_reg]

            link_stats(
                vec![
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        tmp_register,
                        MemOperand::Expression(sp_offset as i32),
                    ),
                    Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        false,
                        tmp_register,
                        tmp_register,
                        FlexOperand::ShiftReg(Ident::Register(Register::Sp), None),
                    ),
                    Stat::MemOp(
                        memop,
                        Cond::Al,
                        false,
                        Ident::Register(dst_register),
                        MemOperand::Zero(tmp_register),
                    ),
                ],
                graph,
            )
            .expect("More than one statement")
        } else {
            // choose a low usage register R12 to push and pop, we store the offset here
            let tmp_register = Ident::Register(Register::R12);

            // PUSH {tmp_reg}
            // LDR tmp_reg, =offset
            // ADD tmp_reg, tmp_reg, SP
            // LDR reg_ind, [tmp_reg]
            // POP {tmp_reg}

            link_stats(
                vec![
                    Stat::Push(Cond::Al, vec![tmp_register]),
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        tmp_register,
                        MemOperand::Expression(sp_offset as i32),
                    ),
                    Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        false,
                        tmp_register,
                        tmp_register,
                        FlexOperand::ShiftReg(Ident::Register(Register::Sp), None),
                    ),
                    Stat::MemOp(
                        memop,
                        Cond::Al,
                        false,
                        Ident::Register(dst_register),
                        MemOperand::Zero(tmp_register),
                    ),
                    Stat::Pop(Cond::Al, vec![tmp_register]),
                ],
                graph,
            )
            .expect("More than one statement")
        }
    }

    /// Move a temporary value into a register, without affecting the
    /// leave_alone temporaries, and considering the order of next uses for each
    /// live temporary.
    fn move_temp_into_reg(
        &mut self,
        temp: Temporary,
        leave_alone: &[Temporary],
        used: &[Temporary],
        graph: &mut Graph<ControlFlow>,
    ) -> (Register, Option<Chain>) {
        let mut free_registers = Vec::new();
        let mut preserve_registers = Vec::new();
        let mut temp_registers = HashMap::new();

        // iterate through all registers to get the free, preserved and the temporary registers
        for (reg_ind, alloc) in self.registers.iter_mut().enumerate() {
            match alloc {
                Alloc::Temp(t) => {
                    if *t == temp {
                        return (Register::from_ind(reg_ind), None);
                    } else if !leave_alone.contains(t) {
                        temp_registers.insert(*t, reg_ind);
                    }
                }
                Alloc::Preserve(p) => preserve_registers.push((*p, reg_ind)),
                Alloc::Free => free_registers.push(reg_ind),
                Alloc::Protected => (),
                Alloc::StackReserve => {
                    panic!("Should never have stack reserved space in registers")
                }
            }
        }

        // we define the allocation of the temporary variable
        let alloc = Alloc::Temp(temp);

        // Attempt to use free registers, registers with preserved values, and
        // registers containing temporaries to place the alloc in a register.
        if !free_registers.is_empty() {
            // Use a free register to store the temporary
            let dst_register = Register::from_ind(
                free_registers
                    .pop()
                    .expect("vector of free registers is not empty"),
            );
            (
                dst_register,
                Some(self.move_reg_stack(dst_register, &alloc, MemOp::Ldr, free_registers, graph)),
            )
        } else if !preserve_registers.is_empty() {
            // There are no free registers, however we can use one of the registers
            // containing a value which must be preserved. (place it on stack)
            let (pres, reg_ind) = preserve_registers
                .pop()
                .expect("vector of preserve registers is not empty");

            // create the destination register from the register's index
            let dst_register = Register::from_ind(reg_ind);

            // place the preserved value to the stack, and pull the temporary from the stack.
            let pres_to_stack = self.move_reg_stack(
                dst_register,
                &Alloc::Preserve(pres),
                MemOp::Str,
                free_registers,
                graph,
            );
            let temp_to_reg =
                self.move_reg_stack(dst_register, &alloc, MemOp::Ldr, Vec::new(), graph);

            // Set the register to contain the temporary we want in a register
            self.registers[reg_ind] = alloc;

            (
                dst_register,
                Some(chain_two_chains(pres_to_stack, temp_to_reg)),
            )
        } else {
            // Use current temporary (attempting to use one with the longest time till use)
            for old_temp in used {
                if let Some(reg_ind) = temp_registers.get(old_temp) {
                    let dst_register = Register::from_ind(*reg_ind);

                    let old_temp_to_stack = self.move_reg_stack(
                        dst_register,
                        &Alloc::Temp(*old_temp),
                        MemOp::Str,
                        Vec::new(),
                        graph,
                    );
                    let new_temp_to_reg = self.move_reg_stack(
                        dst_register,
                        &alloc,
                        MemOp::Ldr,
                        free_registers,
                        graph,
                    );

                    self.registers[*reg_ind] = alloc;

                    return (
                        dst_register,
                        Some(chain_two_chains(old_temp_to_stack, new_temp_to_reg)),
                    );
                }
            }

            panic!("There are no free registers, no preserve registers and no registers with temporaries, this means every register is in the leave_alone list.")
        }
    }

    /// Given another state, create the instructions required to conform
    fn match_state(&mut self, other_state: &Self) -> Option<Chain> {
        todo!()
    }
}

/// Starting from a given node, translate all temporaries to registers.
/// - Traverses from 'node', this must not have already been translated.
/// - state_map contains labels where a state already exists, and must be conformed to.
/// - alloc_state holds the current state of the stack and registers
/// - graph contains all arm nodes
fn translate_from_node(
    mut node: ArmNode,
    mut alloc_state: AllocationState,
    state_map: &mut HashMap<ArmNode, AllocationState>,
    live_ranges: &LiveRanges,
    graph: Graph<ControlFlow>,
) -> ArmNode {
    loop {
        // get the live in and live out
        let livein = live_ranges.get_livein(&node);
        let liveout = live_ranges.get_liveout(&node);

        // update the registers for current live_in
        alloc_state.update_live(&livein);

        let (next, chain_before, chain_after) = match node.clone().get_mut().deref_mut() {
            ControlFlow::Simple(_, stat, next) => {
                match stat {
                    Stat::ApplyOp(_, _, _, _, _, _) => todo!(),
                    Stat::Mul(_, _, _, _, _) => todo!(),
                    Stat::MulA(_, _, _, _, _, _) => todo!(),
                    Stat::MulOp(_, _, _, _, _, _, _) => todo!(),
                    Stat::Move(_, _, _, _, _) => todo!(),
                    Stat::Cmp(_, _, _, _) => todo!(),
                    Stat::SatOp(_, _, _, _, _) => todo!(),
                    Stat::ReadCPSR(_) => todo!(),
                    Stat::MemOp(_, _, _, _, _) => todo!(),
                    Stat::Push(_, _) => todo!(),
                    Stat::Pop(_, _) => todo!(),
                    Stat::Link(_, _) => todo!(),
                    Stat::Call(_, _, _, _) => todo!(),
                    Stat::AssignStackWord(_) => todo!(),
                };
            }
            ControlFlow::Branch(_, _, _, _) => todo!(),
            ControlFlow::Ltorg(_) => todo!(),
            ControlFlow::Return(_, _) => todo!(),
            ControlFlow::Multi(_, next) => {
                // this instruction has no temporaries, however we must keep
                // track that we have translated this (other nodes go here)
                state_map.insert(node, alloc_state.clone());

                // no other instructions are needed, so we simpy move to the next node.
                (next.clone(), None, None)
            }
            ControlFlow::Removed => todo!(),
        };

        alloc_state.update_live(&liveout);
        todo!()
    }
}

impl Register {
    /// get a register from the index in an AllocationState's register array.
    fn from_ind(ind: usize) -> Self {
        match ind {
            0 => Register::R0,
            1 => Register::R1,
            2 => Register::R2,
            3 => Register::R3,
            4 => Register::R4,
            5 => Register::R5,
            6 => Register::R6,
            7 => Register::R7,
            8 => Register::R8,
            9 => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::Sp,
            14 => Register::Lr,
            15 => Register::Pc,
            _ => panic!("Not a valid register index"),
        }
    }

    /// Get the AllocationState register array position of a register identifier
    fn to_ind(self) -> usize {
        self as usize
    }
}
