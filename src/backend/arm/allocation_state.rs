//! Defines the [allocation state](AllocationState) struct which keeps track of
//! all register usage, stack position, subroutine stack frame and the reserved
//! stack section.
//!
//! Generates instructions for moving allocations (temporary variables, or
//! values to preserve) between their stack frame locations and registers.
//!
//! Generates register mappings for temporaries on a per-instruction basis
//!
//! Uses live ranges to determine when to spill values from registers.

use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::graph::Graph;

use super::{
    arm_graph_utils::{
        is_shifted_8_bit, link_optional_chains, link_stats, link_two_chains, simple_node, Chain,
    },
    arm_repr::{
        ArmNode, Cond, ControlFlow, FlexOffset, FlexOperand, Ident, MemOp, MemOperand, MovOp,
        RegOp, Register, Stat, Temporary,
    },
};

/// Type alias for the preserved value identifiers (registers R4-R12 and R14)
type Preserved = u128;

/// A descriptor for the type of data held in a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Alloc {
    /// A temporary variable (in register/stack)
    Temp(Temporary),
    /// A value that must be preserved (register/stack)
    Preserve(Preserved),
    /// Indicates a position is free for use.
    Free,
    /// Space is protected, do not use! (register/stack)
    Protected,
}

/// Tracks the state of the registers, and the stack frame.
/// Stack frame is composed of:
/// |------------|------------------------|
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
pub struct AllocationState {
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

    /// The start of the stack reserve, distance in words from the sp at the
    /// start of the subroutine call
    stack_reserve: i32,

    /// The displacement of the stack pointer from the the start of the call.
    sp_displacement: i32,
}

impl AllocationState {
    /// Creates a stack frame, initial state and label all registers. Generate
    /// the first instructiosn for the call (moving the stack pointer).
    pub fn create_stack_frame(
        args: &[Temporary],
        reserved_stack: u8,
        temps_set: &HashSet<Temporary>,
        graph: &mut Graph<ControlFlow>,
    ) -> (Self, Chain) {
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
        println!("---------------------------------");
        println!("{:?}", args);

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
        for p in 0..=9 {
            alloc_map.insert(Alloc::Preserve(p), sp_displacement);
            sp_displacement += 1;
        }

        // setup reserved stack space
        let stack_reserve = sp_displacement;
        sp_displacement += reserved_stack as i32;

        let new_state = Self {
            registers,
            alloc_map,
            stack_reserve,
            sp_displacement,
        };

        // generate stack pointer move
        let chain = if new_state.sp_displacement != 0 {
            new_state.move_stack_pointer(-sp_displacement, graph)
        } else {
            simple_node(Stat::Nop, graph)
        };

        println!("alloc state after call \n{}", new_state);
        (new_state, chain)
    }

    /// Move the stack pointer by a number of words. Positive number means
    /// moving the stack pointer back, negative advancing it (descending arm
    /// stack).
    fn move_stack_pointer(&self, words: i32, graph: &mut Graph<ControlFlow>) -> Chain {
        let (op, byte_displacement) = if words > 0 {
            (RegOp::Add, words * 4)
        } else {
            (RegOp::Sub, words * -4)
        };

        if is_shifted_8_bit(byte_displacement) {
            simple_node(
                Stat::ApplyOp(
                    op,
                    Cond::Al,
                    false,
                    Ident::Reg(Register::Sp),
                    Ident::Reg(Register::Sp),
                    FlexOperand::Imm(byte_displacement as u32),
                ),
                graph,
            )
        } else {
            let free_regs = self.get_free_register_inds();

            // if there are free registers, we can use them as scratch space.
            if !free_regs.is_empty() {
                let temp_reg = Register::from(free_regs[0]);
                link_stats(
                    vec![
                        Stat::MemOp(
                            MemOp::Ldr,
                            Cond::Al,
                            false,
                            Ident::Reg(temp_reg),
                            MemOperand::Expression(byte_displacement),
                        ),
                        Stat::ApplyOp(
                            op,
                            Cond::Al,
                            false,
                            Ident::Reg(Register::Sp),
                            Ident::Reg(Register::Sp),
                            FlexOperand::ShiftReg(Ident::Reg(temp_reg), None),
                        ),
                    ],
                    graph,
                )
            } else {
                // Otherwise we just use R4 (most likely to be unused)
                link_stats(
                    vec![
                        Stat::Push(Cond::Al, Ident::Reg(Register::R4)),
                        Stat::MemOp(
                            MemOp::Ldr,
                            Cond::Al,
                            false,
                            Ident::Reg(Register::R4),
                            MemOperand::Expression(byte_displacement),
                        ),
                        Stat::ApplyOp(
                            op,
                            Cond::Al,
                            false,
                            Ident::Reg(Register::Sp),
                            Ident::Reg(Register::Sp),
                            FlexOperand::ShiftReg(Ident::Reg(Register::Sp), None),
                        ),
                        Stat::Pop(Cond::Al, Ident::Reg(Register::R4)),
                    ],
                    graph,
                )
            }
        }
    }

    /// Mark all registers containing dead temporaries as free.
    pub fn update_live(&mut self, temps: &[Temporary]) {
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
    fn get_alloc_sp_offset(&self, alloc: &Alloc) -> i32 {
        (self.sp_displacement
            - self
                .alloc_map
                .get(alloc)
                .expect("all valid allocs are in the allocation map"))
            * 4
    }

    /// Transfer data between a given register, and the space in the stack
    /// frame used to contain the allocation inside the register.
    fn register_stack_move(
        &self,
        dst_register: Register,
        alloc: &Alloc,
        memop: MemOp,
        mut free_registers: Vec<usize>,
        graph: &mut Graph<ControlFlow>,
    ) -> Chain {
        let sp_offset = self.get_alloc_sp_offset(alloc);

        if -4095 <= sp_offset && sp_offset <= 4095 {
            // we can use a stack pointer offset to get the temporary
            // LDR reg_ind, [Sp, #offset]
            simple_node(
                Stat::MemOp(
                    memop,
                    Cond::Al,
                    false,
                    Ident::Reg(dst_register),
                    MemOperand::PreIndex(
                        Ident::Reg(Register::Sp),
                        FlexOffset::Expr((sp_offset as i32).into()),
                    ),
                ),
                graph,
            )
        } else if !free_registers.is_empty() {
            // the offset is too large, so we can load as a label into another (free) register, then use that to address
            let tmp_register = Ident::Reg(Register::from(
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
                        FlexOperand::ShiftReg(Ident::Reg(Register::Sp), None),
                    ),
                    Stat::MemOp(
                        memop,
                        Cond::Al,
                        false,
                        Ident::Reg(dst_register),
                        MemOperand::Zero(tmp_register),
                    ),
                ],
                graph,
            )
        } else {
            // choose a low usage register R12 to push and pop, we store the offset here
            let tmp_register = Ident::Reg(Register::R12);

            // as we push, we must increase offset by 4

            // PUSH {tmp_reg}
            // LDR tmp_reg, =offset + 4
            // ADD tmp_reg, tmp_reg, SP
            // LDR reg_ind, [tmp_reg]
            // POP {tmp_reg}

            link_stats(
                vec![
                    Stat::Push(Cond::Al, tmp_register),
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        tmp_register,
                        MemOperand::Expression(sp_offset as i32 + 4),
                    ),
                    Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        false,
                        tmp_register,
                        tmp_register,
                        FlexOperand::ShiftReg(Ident::Reg(Register::Sp), None),
                    ),
                    Stat::MemOp(
                        memop,
                        Cond::Al,
                        false,
                        Ident::Reg(dst_register),
                        MemOperand::Zero(tmp_register),
                    ),
                    Stat::Pop(Cond::Al, tmp_register),
                ],
                graph,
            )
        }
    }

    /// Move a temporary value into a register, without affecting the
    /// leave_alone temporaries, and considering the order of next uses for each
    /// live temporary. If load_stack is true, loads the temporaries value from
    /// the stack.
    pub fn move_temp_into_reg(
        &mut self,
        temp: Temporary,
        load_stack: bool,
        leave_alone: &[Temporary],
        used: &[Temporary],
        graph: &mut Graph<ControlFlow>,
    ) -> (Register, Option<Chain>) {
        let mut free_registers = Vec::new();
        let mut preserve_registers = Vec::new();
        let mut temp_registers = HashMap::new();

        // iterate through all registers to get the free, preserved and the temporary registers
        for (reg_ind, alloc) in self.registers.iter_mut().enumerate().rev() {
            match alloc {
                Alloc::Temp(t) => {
                    if *t == temp {
                        return (Register::from(reg_ind), None);
                    } else if !leave_alone.contains(t) {
                        temp_registers.insert(*t, reg_ind);
                    }
                }
                Alloc::Preserve(p) => preserve_registers.push((*p, reg_ind)),
                Alloc::Free => free_registers.push(reg_ind),
                Alloc::Protected => (),
            }
        }

        // we define the allocation of the temporary variable
        let alloc = Alloc::Temp(temp);

        // Attempt to use free registers, registers with preserved values, and
        // registers containing temporaries to place the alloc in a register.
        if !free_registers.is_empty() {
            // Use a free register to store the temporary
            let dst_register = Register::from(
                free_registers
                    .pop()
                    .expect("vector of free registers is not empty"),
            );
            self.registers[dst_register as usize] = alloc;
            (
                dst_register,
                if load_stack {
                    // we must move it's value from the stack to a register
                    Some(self.register_stack_move(
                        dst_register,
                        &alloc,
                        MemOp::Ldr,
                        free_registers,
                        graph,
                    ))
                } else {
                    // not alive, no need to access stack
                    None
                },
            )
        } else if !preserve_registers.is_empty() {
            // There are no free registers, however we can use one of the registers
            // containing a value which must be preserved. (place it on stack)
            let (pres, reg_ind) = preserve_registers
                .pop()
                .expect("vector of preserve registers is not empty");

            // create the destination register from the register's index
            let dst_register = Register::from(reg_ind);

            // place the preserved value to the stack, and pull the temporary from the stack.
            let pres_to_stack = self.register_stack_move(
                dst_register,
                &Alloc::Preserve(pres),
                MemOp::Str,
                free_registers,
                graph,
            );

            // Set the register to contain the temporary we want in a register
            self.registers[reg_ind] = alloc;

            if load_stack {
                (
                    dst_register,
                    Some(link_two_chains(
                        pres_to_stack,
                        self.register_stack_move(dst_register, &alloc, MemOp::Ldr, vec![], graph),
                    )),
                )
            } else {
                // do not move from stack,
                (dst_register, Some(pres_to_stack))
            }
        } else {
            // Use current temporary (attempting to use one with the longest time till use)
            for old_temp in used {
                if let Some(reg_ind) = temp_registers.get(old_temp) {
                    let dst_register = Register::from(*reg_ind);

                    let old_temp_to_stack = self.register_stack_move(
                        dst_register,
                        &Alloc::Temp(*old_temp),
                        MemOp::Str,
                        vec![],
                        graph,
                    );

                    self.registers[*reg_ind] = alloc;

                    if load_stack {
                        return (
                            dst_register,
                            Some(link_two_chains(
                                old_temp_to_stack,
                                self.register_stack_move(
                                    dst_register,
                                    &alloc,
                                    MemOp::Ldr,
                                    free_registers,
                                    graph,
                                ),
                            )),
                        );
                    } else {
                        return (dst_register, Some(old_temp_to_stack));
                    }
                }
            }

            panic!("There are no free registers, no preserve registers and no registers with temporaries, this means every register is in the leave_alone list.")
        }
    }

    /// Get the indexes of every free register.
    fn get_free_register_inds(&self) -> Vec<usize> {
        let mut free_regs = vec![];

        for (reg_ind, alloc) in self.registers.iter().enumerate() {
            if alloc == &Alloc::Free {
                free_regs.push(reg_ind)
            }
        }

        free_regs
    }

    /// backup an allocation to the stack frame, if it is currently not in a
    /// register, it is already backed up.
    fn backup_alloc_to_frame(
        &mut self,
        alloc: Alloc,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        for (reg_ind, other_alloc) in self.registers.iter().enumerate() {
            if other_alloc == &alloc {
                return Some(self.register_stack_move(
                    Register::from(reg_ind),
                    &alloc,
                    MemOp::Str,
                    self.get_free_register_inds(),
                    graph,
                ));
            }
        }
        None
    }

    /// Mark a register as free, moving any contents to its place in the stack frame.
    fn free_register(
        &mut self,
        register: Register,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        let reg_ind = register as usize;

        match &self.registers[reg_ind] {
            alloc @ Alloc::Temp(_) | alloc @ Alloc::Preserve(_) => {
                let chain = self.register_stack_move(
                    register,
                    alloc,
                    MemOp::Str,
                    self.get_free_register_inds(),
                    graph,
                );
                self.registers[reg_ind] = Alloc::Free;
                Some(chain)
            }
            Alloc::Free => None,
            Alloc::Protected => panic!("Cannot free a protected register"),
        }
    }

    /// Moves an allocation to a specific register. Is allowed to effect
    /// protected registers.
    fn alloc_to_reg(
        &mut self,
        alloc_to_fetch: Alloc,
        dst_register: Register,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        let reg_ind = dst_register as usize;

        // check it it is already in the correct register
        let other_alloc = if self.registers[reg_ind] == alloc_to_fetch {
            return None;
        } else {
            self.registers[reg_ind]
        };

        // if the other_alloc was preserved or a temporary, place it back in its position in the stack frame.
        let put_to_stack =
            if matches!(other_alloc, Alloc::Temp(_)) || matches!(other_alloc, Alloc::Preserve(_)) {
                Some(self.register_stack_move(
                    dst_register,
                    &other_alloc,
                    MemOp::Str,
                    self.get_free_register_inds(),
                    graph,
                ))
            } else {
                None
            };

        // Search through all registers, if the alloc_to_fetch is in a register,
        // we can move it.
        for (other_reg_ind, reg_alloc) in self.registers.iter().enumerate() {
            if reg_alloc == &alloc_to_fetch {
                // move to the destination register
                let move_chain = simple_node(
                    Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Ident::Reg(dst_register),
                        FlexOperand::ShiftReg(Ident::Reg(Register::from(other_reg_ind)), None),
                    ),
                    graph,
                );

                // mark the register as containing the alloc_to_fetch
                self.registers[reg_ind] = alloc_to_fetch;
                self.registers[other_reg_ind] = Alloc::Free;

                return match put_to_stack {
                    Some(put_stack) => Some(link_two_chains(put_stack, move_chain)),
                    None => Some(move_chain),
                };
            }
        }

        // was not in a register, hence we must pull it from the stack.
        let pull_from_frame = self.register_stack_move(
            dst_register,
            &alloc_to_fetch,
            MemOp::Ldr,
            self.get_free_register_inds(),
            graph,
        );

        // mark the register as containing the alloc_to_fetch
        self.registers[reg_ind] = alloc_to_fetch;

        match put_to_stack {
            Some(put_other) => Some(link_two_chains(put_other, pull_from_frame)),
            None => Some(pull_from_frame),
        }
    }

    /// Creates the preamble to a branch link, by placing the contents of register
    /// 14 in an appropriate stack location.
    fn link(&mut self, graph: &mut Graph<ControlFlow>) -> Option<Chain> {
        let alloc = if let alloc @ Alloc::Temp(_) | alloc @ Alloc::Preserve(_) =
            &self.registers[Register::Lr as usize]
        {
            alloc
        } else {
            return None;
        };

        let chain = self.register_stack_move(Register::Lr, alloc, MemOp::Str, vec![], graph);
        self.registers[Register::Lr as usize] = Alloc::Free;
        Some(chain)
    }

    /// - Places arguments in r0-r3 and then on the stack.
    /// - Decrements the stack after the call, as well as getting the return.
    pub fn call(
        &mut self,
        fun: String,
        ret: &Option<Temporary>,
        args: &[Temporary],
        live_after: &[Temporary],
        graph: &mut Graph<ControlFlow>,
    ) -> Chain {
        let mut chains = vec![];

        // backup all args used after the call to their stack frame positions
        for arg in args {
            if live_after.contains(arg) {
                chains.push(self.backup_alloc_to_frame(Alloc::Temp(*arg), graph))
            }
        }

        // place arguments in registers
        if args.len() < 4 {
            for (reg_ind, arg) in args.iter().enumerate() {
                chains.push(self.alloc_to_reg(Alloc::Temp(*arg), Register::from(reg_ind), graph))
            }

            for reg_ind in args.len()..4 {
                chains.push(self.free_register(Register::from(reg_ind), graph))
            }
        } else {
            for (reg_ind, arg) in args.iter().enumerate().take(4) {
                chains.push(self.alloc_to_reg(Alloc::Temp(*arg), Register::from(reg_ind), graph))
            }

            for arg in args.iter().skip(4) {
                // note that registers R0-3 are protected as the first 4 args reside there and are to be 'left alone'
                let (reg, move_chain) =
                    self.move_temp_into_reg(*arg, true, &args[0..4], &[], graph);
                chains.push(move_chain);
                chains.push(Some(simple_node(
                    Stat::Push(Cond::Al, Ident::Reg(reg)),
                    graph,
                )));
            }
        }
        // now the sp_displacement is not valid (is off by max(0, args.len() - 4)

        // free link register
        chains.push(self.link(graph));

        chains.push(Some(simple_node(Stat::Link(Cond::Al, fun), graph)));

        if let Some(ret) = ret {
            // as a new value for ret is created, we must remove the old ret from the registers.
            for alloc in self.registers.iter_mut() {
                if alloc == &Alloc::Temp(*ret) {
                    *alloc = Alloc::Free;
                }
            }

            // set r0 as ret
            self.registers[0] = Alloc::Temp(*ret);
        } else {
            self.registers[0] = Alloc::Free;
        }

        // set r1-3 as free
        self.registers[1] = Alloc::Free;
        self.registers[2] = Alloc::Free;
        self.registers[3] = Alloc::Free;

        // decrement stack pointer by arguments placed on the stack
        if args.len() > 4 {
            chains.push(Some(
                self.move_stack_pointer(max(args.len() as i32 - 4, 0), graph),
            ));
        }

        link_optional_chains(chains).expect("Several statements are 'Some' so must have be a chain")
    }

    // Place the address of the reserved stack space in a register.
    pub fn assign_stack_reserved(&self, reg: Register, graph: &mut Graph<ControlFlow>) -> Chain {
        // given they are loading a stack reserve to a register.
        let sp_displacement = self.stack_reserve;
        if is_shifted_8_bit(sp_displacement) {
            // We use an immediate operand added to sp

            // ADD reg, sp, #sp_displacement

            simple_node(
                Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    Ident::Reg(reg),
                    Ident::Reg(Register::Sp),
                    FlexOperand::Imm(sp_displacement as u32),
                ),
                graph,
            )
        } else {
            // As we cannot store as an immediate, we place the displacement
            // in the register, then add the stack pointer.

            // LDR reg, =sp_displacement
            // ADD reg, reg, SP

            link_stats(
                vec![
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        Ident::Reg(reg),
                        MemOperand::Expression(sp_displacement),
                    ),
                    Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        false,
                        Ident::Reg(reg),
                        Ident::Reg(reg),
                        FlexOperand::ShiftReg(Ident::Reg(Register::Sp), None),
                    ),
                ],
                graph,
            )
        }
    }

    // Create clean up and return
    pub fn subroutine_return(
        &mut self,
        ret: &Option<Temporary>,
        graph: &mut Graph<ControlFlow>,
    ) -> ArmNode {
        // first we place the return value in R)
        let mut chains = vec![];

        if let Some(temp) = ret {
            chains.push(self.alloc_to_reg(Alloc::Temp(*temp), Register::R0, graph));
        }

        // place preserves back (callee saved).
        chains.push(self.alloc_to_reg(Alloc::Preserve(0), Register::R4, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(1), Register::R5, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(2), Register::R6, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(3), Register::R7, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(4), Register::R8, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(5), Register::R9, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(6), Register::R10, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(7), Register::R11, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(8), Register::R12, graph));
        chains.push(self.alloc_to_reg(Alloc::Preserve(9), Register::Lr, graph));
        // we place the saved link register, in the program counter. This instruction
        // will always result in a node as the PC is protected, and Preserve(9) cannot
        // therefore be in the PC yet.

        // now we must reset the stack to its position prior to the call.
        if self.sp_displacement != 0 {
            chains.push(Some(self.move_stack_pointer(self.sp_displacement, graph)));
        }

        chains.push(Some(simple_node(
            Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                Ident::Reg(Register::Pc),
                FlexOperand::ShiftReg(Ident::Reg(Register::Lr), None),
            ),
            graph,
        )));

        // link the statements together
        let Chain(start, mut end) = link_optional_chains(chains)
            .expect("Must place destination in PC, hence always has a chain");

        // Add a literal pool at the end for large functions
        end.set_successor(graph.new_node(ControlFlow::Ltorg(Some(end.clone()))));

        start
    }

    /// Given another state, create the instructions required to match (register
    /// entries must be the same)
    pub fn match_state(
        mut self,
        other_state: &Self,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        let mut chain = vec![];
        for (reg, conform, current) in other_state
            .registers
            .iter()
            .zip(self.registers.iter())
            .enumerate()
            .map(|(r, (conf, curr))| (Register::from(r), *conf, *curr))
            .collect::<Vec<_>>()
            .into_iter()
        {
            match (conform, current) {
                (Alloc::Free, Alloc::Free) => (),
                (Alloc::Protected, Alloc::Protected) => (),
                (Alloc::Protected, _) | (_, Alloc::Protected) => {
                    panic!("Protected do not match, so protected areas have been mangled")
                }
                (Alloc::Free, alloc) => chain.push(self.backup_alloc_to_frame(alloc, graph)),
                (alloc, Alloc::Free) => chain.push(self.alloc_to_reg(alloc, reg, graph)),
                (conf, curr) => {
                    if conf != curr {
                        chain.push(self.backup_alloc_to_frame(curr, graph));
                        chain.push(self.alloc_to_reg(conf, reg, graph))
                    }
                }
            }
        }

        link_optional_chains(chain)
    }

    pub fn push_sp(&mut self) {
        self.sp_displacement += 1
    }

    /// Pop the Allocation State's stack displacement (done when a pop operation
    /// occurs, ensures the offsets )
    pub fn pop_sp(&mut self) {
        self.sp_displacement -= 1
    }
}

impl From<usize> for Register {
    fn from(ind: usize) -> Self {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_from_usize_conversion_correct() {
        assert_eq!(Register::from(0), Register::R0,);
        assert_eq!(Register::from(1), Register::R1,);
        assert_eq!(Register::from(2), Register::R2,);
        assert_eq!(Register::from(3), Register::R3,);
        assert_eq!(Register::from(4), Register::R4,);
        assert_eq!(Register::from(5), Register::R5,);
        assert_eq!(Register::from(6), Register::R6,);
        assert_eq!(Register::from(7), Register::R7,);
        assert_eq!(Register::from(8), Register::R8,);
        assert_eq!(Register::from(9), Register::R9,);
        assert_eq!(Register::from(10), Register::R10,);
        assert_eq!(Register::from(11), Register::R11,);
        assert_eq!(Register::from(12), Register::R12,);
        assert_eq!(Register::from(13), Register::Sp,);
        assert_eq!(Register::from(14), Register::Lr,);
        assert_eq!(Register::from(15), Register::Pc,);
        assert_eq!(0, Register::R0 as usize);
        assert_eq!(1, Register::R1 as usize);
        assert_eq!(2, Register::R2 as usize);
        assert_eq!(3, Register::R3 as usize);
        assert_eq!(4, Register::R4 as usize);
        assert_eq!(5, Register::R5 as usize);
        assert_eq!(6, Register::R6 as usize);
        assert_eq!(7, Register::R7 as usize);
        assert_eq!(8, Register::R8 as usize);
        assert_eq!(9, Register::R9 as usize);
        assert_eq!(10, Register::R10 as usize);
        assert_eq!(11, Register::R11 as usize);
        assert_eq!(12, Register::R12 as usize);
        assert_eq!(13, Register::Sp as usize);
        assert_eq!(14, Register::Lr as usize);
        assert_eq!(15, Register::Pc as usize);
    }

    #[test]
    fn usize_from_register_casting_correct() {
        assert_eq!(Register::R0 as usize, 0);
        assert_eq!(Register::R1 as usize, 1);
        assert_eq!(Register::R2 as usize, 2);
        assert_eq!(Register::R3 as usize, 3);
        assert_eq!(Register::R4 as usize, 4);
        assert_eq!(Register::R5 as usize, 5);
        assert_eq!(Register::R6 as usize, 6);
        assert_eq!(Register::R7 as usize, 7);
        assert_eq!(Register::R8 as usize, 8);
        assert_eq!(Register::R9 as usize, 9);
        assert_eq!(Register::R10 as usize, 10);
        assert_eq!(Register::R11 as usize, 11);
        assert_eq!(Register::R12 as usize, 12);
        assert_eq!(Register::Sp as usize, 13);
        assert_eq!(Register::Lr as usize, 14);
        assert_eq!(Register::Pc as usize, 15);
    }
}

impl Display for Alloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Alloc::Temp(t) => write!(f, "T{}", t),
            Alloc::Preserve(p) => write!(f, "P{}", p),
            Alloc::Free => write!(f, "FREE"),
            Alloc::Protected => write!(f, "PROTECTED"),
        }
    }
}

impl Display for AllocationState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "REGISTERS: [{}]",
            self.registers
                .iter()
                .map(|alloc| alloc.to_string())
                .intersperse(", ".to_string())
                .collect::<String>()
        )?;
        writeln!(
            f,
            "{}",
            self.alloc_map
                .iter()
                .map(|(alloc, pos)| format!("{} -> {}", alloc, pos))
                .intersperse("\n".to_string())
                .collect::<String>()
        )?;
        writeln!(f, "STACK RESERVE: {}", self.stack_reserve)?;
        writeln!(f, "STACK DISPLACEMENT: {}", self.sp_displacement)
    }
}
