//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use'.

use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    ops::DerefMut,
};

use crate::backend::arm::{
    arm_graph_utils::{link_optional_chains, link_two_chains},
    arm_repr::{FlexOffset, MemOp, MemOperand, MovOp},
    live_ranges::get_live_ranges,
};

use super::{
    super::super::graph::Graph,
    arm_graph_utils::{is_shifted_8_bit, link_stats, simple_node, Chain},
    arm_repr::{
        ArmNode, Cond, ControlFlow, FlexOperand, Ident, Program, RegOp, Register, Stat, Subroutine,
        Temporary,
    },
    live_ranges::LiveRanges,
};

type Preserved = u128;
type Reserved = i128;

/// A type for each part of the registers and stack,
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

    /// The start of the stack reserve, distance in words from the sp at the
    /// start of the subroutine call
    stack_reserve: i32,

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

        (new_state, chain)
    }

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
                    Ident::Register(Register::Sp),
                    Ident::Register(Register::Sp),
                    FlexOperand::Imm(byte_displacement as u32),
                ),
                graph,
            )
        } else {
            let free_regs = self.get_free_register_inds();

            // if there are free registers, we can use them as scratch space.
            if !free_regs.is_empty() {
                let temp_reg = Register::from_ind(free_regs[0]);
                link_stats(
                    vec![
                        Stat::MemOp(
                            MemOp::Ldr,
                            Cond::Al,
                            false,
                            Ident::Register(temp_reg),
                            MemOperand::Expression(byte_displacement),
                        ),
                        Stat::ApplyOp(
                            op,
                            Cond::Al,
                            false,
                            Ident::Register(Register::Sp),
                            Ident::Register(Register::Sp),
                            FlexOperand::ShiftReg(Ident::Register(temp_reg), None),
                        ),
                    ],
                    graph,
                )
                .expect("There is more than one statement, so there is a chain")
            } else {
                // Otherwise we just use R4 (most likely to be unused)
                link_stats(
                    vec![
                        Stat::Push(Cond::Al, Ident::Register(Register::R4)),
                        Stat::MemOp(
                            MemOp::Ldr,
                            Cond::Al,
                            false,
                            Ident::Register(Register::R4),
                            MemOperand::Expression(byte_displacement),
                        ),
                        Stat::ApplyOp(
                            op,
                            Cond::Al,
                            false,
                            Ident::Register(Register::Sp),
                            Ident::Register(Register::Sp),
                            FlexOperand::ShiftReg(Ident::Register(Register::Sp), None),
                        ),
                        Stat::Pop(Cond::Al, Ident::Register(Register::R4)),
                    ],
                    graph,
                )
                .expect("There is more than one statement, so there is a chain")
            }
        }
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
                    Ident::Register(dst_register),
                    MemOperand::PreIndex(
                        Ident::Register(Register::Sp),
                        FlexOffset::Expr((sp_offset as i32).into()),
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
                    Stat::Push(Cond::Al, tmp_register),
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
                    Stat::Pop(Cond::Al, tmp_register),
                ],
                graph,
            )
            .expect("More than one statement")
        }
    }

    /// Move a temporary value into a register, without affecting the
    /// leave_alone temporaries, and considering the order of next uses for each
    /// live temporary. If load_stack is true, loads the temporaries value from
    /// the stack.
    fn move_temp_into_reg(
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
            let dst_register = Register::from_ind(reg_ind);

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
                        self.register_stack_move(
                            dst_register,
                            &alloc,
                            MemOp::Ldr,
                            Vec::new(),
                            graph,
                        ),
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
                    let dst_register = Register::from_ind(*reg_ind);

                    let old_temp_to_stack = self.register_stack_move(
                        dst_register,
                        &Alloc::Temp(*old_temp),
                        MemOp::Str,
                        Vec::new(),
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
        let mut free_regs = Vec::new();

        for (reg_ind, alloc) in self.registers.iter().enumerate() {
            if alloc == &Alloc::Free {
                free_regs.push(reg_ind)
            }
        }

        free_regs
    }

    /// backup an allocation to the stack frame, if it is currently not in a register, it is already backed up.
    fn backup_alloc_to_frame(
        &mut self,
        alloc: Alloc,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        for (reg_ind, other_alloc) in self.registers.iter().enumerate() {
            if other_alloc == &alloc {
                return Some(self.register_stack_move(
                    Register::from_ind(reg_ind),
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
        let reg_ind = register.to_ind();
        let alloc = self.registers[reg_ind];

        match &self.registers[register.to_ind()] {
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
        let reg_ind = dst_register.to_ind();

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
            if other_alloc == alloc_to_fetch {
                // move to the destination register
                let move_chain = simple_node(
                    Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Ident::Register(dst_register),
                        FlexOperand::ShiftReg(
                            Ident::Register(Register::from_ind(other_reg_ind)),
                            None,
                        ),
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
            &self.registers[Register::Lr.to_ind()]
        {
            alloc
        } else {
            return None;
        };

        let chain =
            self.register_stack_move(Register::Lr, alloc, MemOp::Str, Vec::new(), graph);
        self.registers[Register::Lr.to_ind()] = Alloc::Free;
        Some(chain)
    }

    /// - Places arguments in r0-r3 and then on the stack.
    /// - Decrements the stack after the call, as well as getting the return.
    fn call(
        &mut self,
        fun: String,
        ret: &Option<Temporary>,
        args: &[Temporary],
        live_after: &[Temporary],
        graph: &mut Graph<ControlFlow>,
    ) -> Chain {
        let mut chains = Vec::new();

        // backup all args used after the call to their stack frame positions
        for arg in args {
            if live_after.contains(arg) {
                chains.push(self.backup_alloc_to_frame(Alloc::Temp(*arg), graph))
            }
        }

        // place arguments in registers
        if args.len() < 4 {
            for (reg_ind, arg) in args.iter().enumerate() {
                chains.push(self.alloc_to_reg(
                    Alloc::Temp(*arg),
                    Register::from_ind(reg_ind),
                    graph,
                ))
            }

            for reg_ind in args.len()..4 {
                chains.push(self.free_register(Register::from_ind(reg_ind), graph))
            }
        } else {
            for (reg_ind, arg) in args.iter().enumerate().take(4) {
                chains.push(self.alloc_to_reg(
                    Alloc::Temp(*arg),
                    Register::from_ind(reg_ind),
                    graph,
                ))
            }

            for arg in args.iter().skip(4) {
                // note that registers R0-3 are protected as the first 4 args reside there and are to be 'left alone'
                let (reg, move_chain) =
                    self.move_temp_into_reg(*arg, true, &args[0..4], &[], graph);
                chains.push(move_chain);
                chains.push(Some(simple_node(
                    Stat::Push(Cond::Al, Ident::Register(reg)),
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
    fn assign_stack_reserved(
        &self,
        reg: Register,
        graph: &mut Graph<ControlFlow>,
    ) -> Option<Chain> {
        // given they are loading a stack reserve to a register.
        let sp_displacement = self.stack_reserve;
        if is_shifted_8_bit(sp_displacement) {
            // We use an immediate operand added to sp

            // ADD reg, sp, #sp_displacement

            Some(simple_node(
                Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    Ident::Register(reg),
                    Ident::Register(Register::Sp),
                    FlexOperand::Imm(sp_displacement as u32),
                ),
                graph,
            ))
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
                        Ident::Register(reg),
                        MemOperand::Expression(sp_displacement),
                    ),
                    Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        false,
                        Ident::Register(reg),
                        Ident::Register(reg),
                        FlexOperand::ShiftReg(Ident::Register(Register::Sp), None),
                    ),
                ],
                graph,
            )
        }
    }

    // Create clean up and return
    fn subroutine_return(
        mut self,
        ret: &Option<Temporary>,
        graph: &mut Graph<ControlFlow>,
    ) -> Chain {
        // first we place the return value in R)
        let mut chains = Vec::new();

        if let Some(temp) = ret {
            chains.push(self.alloc_to_reg(Alloc::Temp(*temp), Register::R0, graph));
        }

        // now we must reset the stack to its position prior to the call.
        if self.sp_displacement != 0 {
            chains.push(Some(self.move_stack_pointer(self.sp_displacement, graph)));
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

        // we place the saved link register, in the program counter. This instruction
        // will always result in a node as the PC is protected, and Preserve(9) cannot
        // therefore be in the PC yet.
        chains.push(self.alloc_to_reg(Alloc::Preserve(9), Register::Pc, graph));

        link_optional_chains(chains)
            .expect("Must place destination in PC, hence always has a chain")
    }

    /// Given another state, create the instructions required to match (register
    /// entries must be the same)
    fn match_state(mut self, other_state: &Self, graph: &mut Graph<ControlFlow>) -> Option<Chain> {
        let mut chain = Vec::new();
        for (reg, conform, current) in other_state
            .registers
            .iter()
            .zip(self.registers.iter())
            .enumerate()
            .map(|(r, (conf, curr))| (Register::from_ind(r), *conf, *curr))
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
                    chain.push(self.backup_alloc_to_frame(curr, graph));
                    chain.push(self.alloc_to_reg(conf, reg, graph))
                }
            }
        }

        link_optional_chains(chain)
    }

    fn push_sp(&mut self) {
        self.sp_displacement += 1
    }

    /// Pop the Allocation State's stack displacement (done when a pop operation
    /// occurs, ensures the offsets )
    fn pop_sp(&mut self) {
        self.sp_displacement -= 1
    }
}

impl Ident {
    pub fn get_temp(&self) -> Temporary {
        if let Ident::Temp(t) = self {
            *t
        } else {
            panic!("cannot get temporary id from a non-temporary id")
        }
    }
}

pub fn allocate_registers(program: Program) -> Program {
    // generate the live ranges for the program.
    let live_ranges = get_live_ranges(&program);

    // translate main program block:
    let Program {
        data,
        reserved_stack,
        main,
        temps,
        functions,
        mut cfg,
    } = program;

    Program {
        data,
        reserved_stack,
        main: allocate_for_routine(main, &[], reserved_stack, &temps, &live_ranges, &mut cfg),
        temps,
        functions: functions
            .into_iter()
            .map(
                |(
                    name,
                    Subroutine {
                        args,
                        start_node,
                        temps,
                        reserved_stack,
                    },
                )| {
                    (
                        name,
                        Subroutine {
                            args,
                            start_node: allocate_for_routine(
                                start_node,
                                &[],
                                reserved_stack,
                                &temps,
                                &live_ranges,
                                &mut cfg,
                            ),
                            temps,
                            reserved_stack,
                        },
                    )
                },
            )
            .collect::<HashMap<String, Subroutine>>(),
        cfg,
    }
}

fn allocate_for_routine(
    mut start_node: ArmNode,
    args: &[Temporary],
    reserved_stack: u8,
    temps_set: &HashSet<Temporary>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    let mut state_map = HashMap::new();
    let (alloc_state, (new_start, mut stack_setup_end)) =
        AllocationState::create_stack_frame(args, reserved_stack, temps_set, graph);

    // connect up the start:
    start_node.set_predecessor(stack_setup_end.clone());
    stack_setup_end.set_successor(start_node.clone());

    translate_from_node(
        start_node.clone(),
        alloc_state,
        &mut state_map,
        live_ranges,
        graph,
    );

    new_start
}

/// Starting from a given node, translate all temporaries to registers.
/// - Traverses from 'node', this must not have already been translated.
/// - state_map contains labels where a state already exists, and must be conformed to.
/// - alloc_state holds the current state of the stack and registers
/// - graph contains all arm nodes
/// - every node must have a predecessor.
fn translate_from_node(
    mut current_node: ArmNode,
    mut alloc_state: AllocationState,
    state_map: &mut HashMap<ArmNode, AllocationState>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) {
    loop {
        // get the live in and live out
        let livein = live_ranges.get_livein(&current_node.clone());
        let liveout = live_ranges.get_liveout(&current_node.clone());

        // update the registers for current live_in
        alloc_state.update_live(&livein);

        let mut next: ArmNode = match current_node.clone().get_mut().deref_mut() {
            ControlFlow::Simple(prev, Stat::Call(fun_name, ret, args), next) => {
                let (mut start, mut end) =
                    alloc_state.call(fun_name.clone(), ret, args, &liveout, graph);

                // as we entirely replace the 'call' node, we set the successors and
                // predecessors to point to the ends of our chain of statements
                if let Some(next_node) = next {
                    end.set_successor(next_node.clone());
                    next_node.replace_predecessor(current_node.clone(), end)
                }

                let mut prev_node = prev.clone().expect("Every node must have a predecessor");
                start.set_predecessor(prev_node.clone());
                prev_node.replace_successor(current_node.clone(), start);

                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Simple(prev_entry, stat, next_entry) => {
                let chain_before: Option<Chain> = match stat {
                    Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(
                            arg1_ident @ Ident::Temp(_),
                            FlexOffset::ShiftReg(_, arg2_ident @ Ident::Temp(_), _),
                        ),
                    )
                    | Stat::Mul(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    )
                    | Stat::ApplyOp(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg2_ident @ Ident::Temp(_), _),
                    )
                    | Stat::SatOp(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    ) => {
                        // Arguments can be the same and match the destination.

                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg1_ident.get_temp();

                        // ensure both arguments are in registers, and leave
                        // the arguments alone if they are already in a register.
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        // update the live temps, this means that if either argument
                        // dies after this statement, their register can be used as
                        // the destination register.
                        alloc_state.update_live(&liveout);

                        // Use any free register, if arg1 or arg2 are live after the instruction, we do not want to use their register.
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            false,
                            &[arg1_t, arg2_t],
                            &liveout,
                            graph,
                        );

                        // assign the register identifiers
                        *dst_ident = Ident::Register(dst_reg);
                        *arg1_ident = Ident::Register(arg1_reg);
                        *arg2_ident = Ident::Register(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, dst_chain])
                    }
                    Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::Zero(arg_ident @ Ident::Temp(_)),
                    )
                    | Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(arg_ident @ Ident::Temp(_), FlexOffset::Expr(_)),
                    )
                    | Stat::Move(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg_ident @ Ident::Temp(_), _),
                    )
                    | Stat::Cmp(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg_ident @ Ident::Temp(_), _),
                    )
                    | Stat::ApplyOp(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg_ident @ Ident::Temp(_),
                        _,
                    ) => {
                        // A destination and argument, these can be the same register.
                        let dst_t = dst_ident.get_temp();
                        let arg_t = arg_ident.get_temp();

                        // place the arg in a register
                        let (arg_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        // update the live temps, this means that if the argument
                        // dies after this statement, its register can be used.
                        alloc_state.update_live(&liveout);

                        // find and place the dst in a register
                        let (dst_reg, dst_chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[arg_t], &liveout, graph);

                        *arg_ident = Ident::Register(arg_reg);
                        *dst_ident = Ident::Register(dst_reg);

                        link_optional_chains(vec![arg_chain, dst_chain])
                    }
                    Stat::MulA(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                        arg3_ident @ Ident::Temp(_),
                    ) => {
                        // three arguments and one destination
                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();
                        let arg3_t = arg2_ident.get_temp();

                        // ensure both arguments are in registers, and leave
                        // the arguments alone if they are already in a register.
                        let (arg1_reg, arg1_chain) = alloc_state.move_temp_into_reg(
                            arg1_t,
                            true,
                            &[arg2_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg2_reg, arg2_chain) = alloc_state.move_temp_into_reg(
                            arg2_t,
                            true,
                            &[arg1_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg3_reg, arg3_chain) = alloc_state.move_temp_into_reg(
                            arg3_t,
                            true,
                            &[arg3_t, arg2_t],
                            &livein,
                            graph,
                        );

                        // update with the liveout range
                        alloc_state.update_live(&liveout);

                        // find a destination, assign register
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            false,
                            &[arg1_t, arg2_t, arg3_t],
                            &liveout,
                            graph,
                        );

                        *dst_ident = Ident::Register(dst_reg);
                        *arg1_ident = Ident::Register(arg1_reg);
                        *arg2_ident = Ident::Register(arg2_reg);
                        *arg3_ident = Ident::Register(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, arg3_chain, dst_chain])
                    }
                    Stat::MulOp(
                        _,
                        _,
                        _,
                        dst1_ident @ Ident::Temp(_),
                        dst2_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    ) => {
                        // two destinations and two arguments

                        let dst1_t = dst1_ident.get_temp();
                        let dst2_t = dst2_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        // update with the liveout range
                        alloc_state.update_live(&liveout);

                        // assign the argument registers
                        let (dst1_reg, dst1_chain) = alloc_state.move_temp_into_reg(
                            dst1_t,
                            false,
                            &[arg1_t, arg2_t],
                            &livein,
                            graph,
                        );
                        let (dst2_reg, dst2_chain) = alloc_state.move_temp_into_reg(
                            dst2_t,
                            false,
                            &[arg1_t, arg2_t, dst1_t],
                            &livein,
                            graph,
                        );

                        *dst1_ident = Ident::Register(dst1_reg);
                        *dst2_ident = Ident::Register(dst2_reg);
                        *arg1_ident = Ident::Register(arg1_reg);
                        *arg2_ident = Ident::Register(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, dst1_chain, dst2_chain])
                    }

                    Stat::Move(_, _, _, dst_ident @ Ident::Temp(_), _)
                    | Stat::Cmp(_, _, dst_ident @ Ident::Temp(_), _)
                    | Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    )
                    | Stat::ReadCPSR(dst_ident @ Ident::Temp(_)) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        // kill unused variables before determining destination
                        alloc_state.update_live(&liveout);

                        let (reg, chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[], &[], graph);
                        *dst_ident = Ident::Register(reg);
                        chain
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(arg2_ident @ Ident::Temp(_), FlexOffset::Expr(_)),
                    )
                    | Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::Zero(arg2_ident @ Ident::Temp(_)),
                    ) => {
                        // two arguments
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        *arg1_ident = Ident::Register(arg1_reg);
                        *arg2_ident = Ident::Register(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(
                            arg2_ident @ Ident::Temp(_),
                            FlexOffset::ShiftReg(_, arg3_ident @ Ident::Temp(_), _),
                        ),
                    ) => {
                        // two arguments
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();
                        let arg3_t = arg3_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) = alloc_state.move_temp_into_reg(
                            arg1_t,
                            true,
                            &[arg2_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg2_reg, arg2_chain) = alloc_state.move_temp_into_reg(
                            arg2_t,
                            true,
                            &[arg1_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg3_reg, arg3_chain) = alloc_state.move_temp_into_reg(
                            arg3_t,
                            true,
                            &[arg1_t, arg2_t],
                            &livein,
                            graph,
                        );

                        *arg1_ident = Ident::Register(arg1_reg);
                        *arg2_ident = Ident::Register(arg2_reg);
                        *arg3_ident = Ident::Register(arg3_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, arg3_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg_ident @ Ident::Temp(_),
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    ) => {
                        // single argument used (a store)
                        let arg_t = arg_ident.get_temp();

                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Register(arg1_reg);

                        arg_chain
                    }

                    Stat::Push(_, arg_ident @ Ident::Temp(_)) => {
                        // push to the stack
                        let arg_t = arg_ident.get_temp();
                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Register(arg1_reg);

                        alloc_state.push_sp();

                        arg_chain
                    }
                    Stat::Pop(_, arg_ident @ Ident::Temp(_)) => {
                        // push to the stack
                        let arg_t = arg_ident.get_temp();
                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Register(arg1_reg);

                        alloc_state.pop_sp();

                        arg_chain
                    }
                    Stat::Link(_, _) => None,
                    Stat::AssignStackWord(dst_ident @ Ident::Temp(_)) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        let (reg, get_reg_chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[], &[], graph);
                        let set_address_chain = alloc_state.assign_stack_reserved(reg, graph);

                        *dst_ident = Ident::Register(reg);

                        link_optional_chains(vec![get_reg_chain, set_address_chain])
                    }
                    _ => None, // No temporaries, so no chain
                };

                // if new chains were created, then make them the predecessors to this node.
                if let Some((mut before_start, mut before_end)) = chain_before {
                    let mut prev_node =
                        prev_entry.clone().expect("All nodes must have a successor");
                    // connect prev <-> before_start <-> before_end <-> this node
                    prev_node.replace_successor(current_node.clone(), before_start.clone());
                    before_start.set_predecessor(prev_node);

                    *prev_entry = Some(before_end.clone());
                    before_end.set_successor(current_node.clone())
                }

                if let Some(next_node) = next_entry {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Branch(prev, branch_next, _, next) => {
                // check if the current node has had its registers allocated already
                if let Some(conform_alloc_state) = state_map.get(branch_next) {
                    if let Some((mut chain_start, mut chain_end)) =
                        alloc_state.match_state(conform_alloc_state, graph)
                    {
                        let mut next_node = branch_next.clone();

                        chain_start.set_predecessor(current_node.clone());
                        *branch_next = chain_start;

                        chain_end.set_successor(branch_next.clone());
                        next_node.replace_predecessor(current_node, chain_end)
                    }

                    return;
                }

                translate_from_node(
                    branch_next.clone(),
                    alloc_state.clone(),
                    state_map,
                    live_ranges,
                    graph,
                );

                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Ltorg(_) => return,
            ControlFlow::Return(prev, ret) => {
                let (start, end) = alloc_state.subroutine_return(ret, graph);

                let mut prev_node = prev.clone().expect("Every node must have a predecessor");
                prev_node.replace_successor(current_node.clone(), start);

                return;
            }
            ControlFlow::Multi(_, next) => {
                // this instruction has no temporaries, however we must keep
                // track that we have translated this (other nodes go here)
                state_map.insert(current_node.clone(), alloc_state.clone());

                // no other instructions are needed, so we simpy move to the next node.
                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Removed => {
                panic!("Removed nodes must not be present in the traversable part of the graph")
            }
        };

        // check if the current node has had its registers allocated already
        if let Some(conform_alloc_state) = state_map.get(&next) {
            if let Some((mut chain_start, mut chain_end)) =
                alloc_state.match_state(conform_alloc_state, graph)
            {
                chain_start.set_predecessor(current_node.clone());
                current_node.replace_successor(next.clone(), chain_start);

                chain_end.set_successor(next.clone());
                next.replace_predecessor(current_node, chain_end);
            }

            return;
        }

        current_node = next;
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
