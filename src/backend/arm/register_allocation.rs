//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use'.

use std::{cmp::min, collections::HashMap, ops::DerefMut};

use lazy_static::__Deref;

use crate::backend::arm::arm_repr::{MemOperand, FlexOffset, MemOp};

use super::{
    super::super::graph::Graph,
    arm_graph_utils::{simple_node, Chain, link_stats},
    arm_repr::{
        Cond, ControlFlow, FlexOperand, Ident, RegOp, Register, Stat, Temporary, ArmNode,
    },
};

type Preserved = u128;
type Reserved = i128;

/// A type for each part of the registers and stack,
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct AllocationState {
    /// Represents the usable registers:
    /// ```text
    /// [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13 - SP, r14 - LR, r15 - PC]
    /// ```
    /// The following cannot be used:
    /// - R13 contains the stack pointer
    /// - R15 is the program counter
    registers: [Alloc; 16],

    /// Represents the stack, the end of the vector is the current position of
    /// the stack pointer.
    stack: Vec<Alloc>,

    /// Represents the displacement of the stack pointer from when the function
    /// was called in words (1 word = 4 bytes)
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
                // reserve reserved_stack number of words (4 bytes each) on the stack
                Some(simple_node(
                    Stat::ApplyOp(
                        RegOp::Sub,
                        Cond::Al,
                        false,
                        Ident::Register(Register::Sp),
                        Ident::Register(Register::Sp),
                        FlexOperand::Imm((reserved_stack * 4) as u32),
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

    /// Mark the space used for any variable not used as free, place all use 
    /// temporaries in registers.
    /// Note: temps is sorted for 'wait longest till use' and the end.
    fn update_live(&mut self, temps: &Vec<Temporary>, graph: &mut Graph<ControlFlow>) -> Option<Chain> {

        // First mark all dead temporaries as free in registers. Build a map of 
        // temporaries to registers & all free registers.
        for (ind, allocation) in self.registers.iter_mut().enumerate() {
            if let Alloc::Temp(temp) = &allocation {
                if !temps.contains(&temp) {
                    *allocation = Alloc::Free
                }
            }
        }

        // Mark all dead temporaries as free in the stack
        for allocation in self.stack.iter_mut() {
            if let Alloc::Temp(temp) = &allocation {
                if !temps.contains(&temp) {
                    *allocation = Alloc::Free
                }
            }
        }

        let mut stats = Vec::new();

        // clean up stack by removing free at the end

        while self.stack.last() == Some(&Alloc::Free) {
            stats.push(Stat::ApplyOp(RegOp::Add, Cond::Al, false, Ident::Register(Register::Sp), Ident::Register(Register::Sp), FlexOperand::Imm(4)));
            self.sp_displacement -= 1;
            self.stack.pop();
        }

        if !stats.is_empty() {
            link_stats(stats, graph)
        } else {
            None
        }
    }

    /// Move a temporary into a register, if it is on the stack, set that space 
    /// to free, accordingly. Accounts for very large (>4095 byte) offsets from 
    /// SP to temp stack position.
    fn move_from_stack_to_reg(&mut self, temp: Temporary, reg_ind: usize, graph: &mut Graph<ControlFlow>) -> (Register, Option<Chain>) {
        let stack_len = self.stack.len();
        self.registers[reg_ind] = Alloc::Temp(temp);
            for (stack_ind, stack_alloc) in self.stack.iter_mut().enumerate() {
                if &Alloc::Temp(temp) == stack_alloc {
                    *stack_alloc = Alloc::Free;
                    let offset = (stack_len - stack_ind) * 4;
                    let instrs_chain = if offset <= 4096 {
                        // LDR reg_ind, [Sp, #offset]
                        simple_node(Stat::MemOp(MemOp::Ldr, Cond::Al, false, Ident::Register(Register::from_ind(reg_ind)), MemOperand::PreIndex(Ident::Register(Register::Sp), FlexOffset::Expr((offset as i32).into()), false)), graph)
                    } else {
                        // PUSH {R12}
                        // LDR R12, =offset
                        // ADD R12, R12, SP
                        // LDR reg_ind, [R12]
                        // PUSH R12
                        link_stats(vec![
                            Stat::Push(Cond::Al, vec![Ident::Register(Register::R12)]),
                            Stat::MemOp(MemOp::Ldr, Cond::Al, false, Ident::Register(Register::R12), MemOperand::Expression(offset as i32)),
                            Stat::ApplyOp(RegOp::Add, Cond::Al, false, Ident::Register(Register::R12), Ident::Register(Register::R12), FlexOperand::ShiftReg(Ident::Register(Register::Sp), None)),
                            Stat::MemOp(MemOp::Ldr, Cond::Al, false, Ident::Register(Register::from_ind(reg_ind)), MemOperand::Zero(Ident::Register(Register::R12))),
                            Stat::Pop(Cond::Al, vec![Ident::Register(Register::R12)]),
                        ], graph).expect("More than one statement")
                    };
                    return (Register::from_ind(reg_ind), Some(instrs_chain))
                }
            }
            return (Register::from_ind(reg_ind), None)
    }

    fn move_from_reg_to_stack(&mut self, register_ind: usize, graph: &mut Graph<ControlFlow>) -> Chain {
        // set register as free
        let alloc = self.registers[register_ind];
        self.registers[register_ind] = Alloc::Free;

        // find a space on the stack, we try to keep stack size small by 

    }

    /// Move a temporary to a register. If it is already in a register, no 
    /// changes are made.
    fn move_to_reg(&mut self, temp: Temporary, leave_alone: &Vec<Temporary>, usefulness: &Vec<Temporary>, graph: &mut Graph<ControlFlow>) -> (Register, Option<Chain>) {

        let mut reg_temps = HashMap::new();
        let mut free_reg = None;
        let mut reg_pres = None;

        for (ind, alloc) in self.registers.iter().enumerate() {
            match alloc {
                Alloc::Temp(t) => {
                    if t == &temp {
                        return (Register::from_ind(ind), None)
                    } else if !leave_alone.contains(t) {
                        reg_temps.insert(*t, ind);
                    }
                },
                Alloc::Preserve(p) => reg_pres = Some((*p, ind)),
                Alloc::Free => free_reg = Some(ind),
                Alloc::StackReserve | 
                Alloc::Protected => (),
            }
        }

        let stack_len = self.stack.len();

        if let Some(reg_ind) = free_reg {
            // move from stack to the register
            return self.move_from_stack_to_reg(temp, reg_ind, graph);
        }

        if let Some((pres_id, reg_ind)) = reg_pres {
            // move the preserve to the stack



        }




        todo!()

    }

    fn translate_node(&mut self, node: ArmNode, livein: Vec<Temporary>, liveout: Vec<Temporary>,  graph: &mut Graph<ControlFlow>) {
        self.update_live(&livein, graph);




        match node.get_mut().deref_mut() {
            ControlFlow::Simple(_, stat, _) => todo!(),
            ControlFlow::Branch(_, _, _, _) => todo!(),
            ControlFlow::Return(_, _) => todo!(),
            ControlFlow::Multi(_, _) => todo!(),
            ControlFlow::Ltorg(_) |
            ControlFlow::Removed => (),
        };




        self.update_live(&liveout, graph);
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
            _ => panic!("Not a valid register index")
        }
    }

    /// Get the AllocationState register array position of a register identifier
    fn to_ind(self) -> usize {
        self as usize
    }
}