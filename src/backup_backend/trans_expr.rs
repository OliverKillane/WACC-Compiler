use super::arm_repr::*;
use crate::ast;
use crate::ast::{Expr, Type};
use std::collections::HashMap;

/// Returns the the size in bytes of the given AST Type
pub(super) fn get_type_size(t: &Type) -> i32 {
    match t {
        Type::Bool | Type::Char => 1,
        Type::Int | Type::String | Type::Pair(_, _) | Type::Array(_, _) => 4,
        Type::Any | Type::Generic(_) => {
            panic!("Any type should never show up after semantic analysis")
        }
    }
}

/// Returns the appropriate operand for an offseted variable
pub(super) fn get_variable_operand(offset: i32) -> MemOperand {
    if offset == 0 {
        MemOperand::Zero(Register::Sp)
    } else {
        MemOperand::PreIndex(Register::Sp, FlexOffset::Expr(offset.into()), false)
    }
}

pub(super) const ARG_REGS: [Register; 4] = [Register::R0, Register::R1, Register::R2, Register::R3];
pub(super) const RET_REG: Register = Register::R0;
pub(super) const PUSH_SIZE: i32 = 4;

/// Translates AST expressions to the corresponding IR ARM expressions
/// and pushes the ARM expressions to the provided Vector of Statements
pub(super) fn trans_expr(
    arm_stats: &mut Vec<Stat>,
    expr: &Expr<Option<Type>, usize>,
    expr_type: &Type,
    dest_reg: Register,
    registers: &[Register],
    stack_offset: i32,
    symbol_table: &HashMap<usize, ast::Type>,
    id_map: &HashMap<usize, i32>,
    string_literals: &mut HashMap<String, usize>,
) {
    match expr {
        /// Null translation
        /// Loads 0 into the destination register
        Expr::Null => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(0),
            ));
        }
        /// Int translation
        /// Loads the value of the given integer into the destination register
        Expr::Int(i) => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(*i),
            ));
        }
        /// Bool translation
        /// Loads the corresponding integer value of the given boolean into the destination register
        ///
        Expr::Bool(b) => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(if *b { 1 } else { 0 }),
            ));
        }
        /// Char translation
        Expr::Char(c) => {
            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                dest_reg,
                FlexOperand::Char((*c as u8 as i32).into()),
            ));
        }
        /// String translation
        Expr::String(s) => {
            let str_id = string_literals.len();
            let str_id = string_literals.entry(s.clone()).or_insert(str_id);
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Label(format!("msg_{}", str_id)),
            ));
        }
        /// Variable translation
        /// Decic
        Expr::Var(id) => {
            let ldr_instr = if get_type_size(&symbol_table[id]) == 1 {
                MemOp::Ldrb
            } else {
                MemOp::Ldr
            };
            arm_stats.push(Stat::MemOp(
                ldr_instr,
                Cond::Al,
                false,
                dest_reg,
                get_variable_operand(&id_map[id] + stack_offset),
            ));
        }
        Expr::ArrayElem(id, exprs) => {
            let id_offset = id_map[id] + stack_offset;
            arm_stats.push(Stat::ApplyOp(
                RegOp::Add,
                Cond::Al,
                false,
                dest_reg,
                Register::Sp,
                FlexOperand::Imm(id_offset as u32),
            ));

            for (expr_idx, ast::ASTWrapper(t, expr)) in exprs.iter().enumerate() {
                let do_register_fallback = registers.len() == 1;
                let is_last_expr = expr_idx == exprs.len() - 1;

                let array_index_reg = if do_register_fallback {
                    arm_stats.push(Stat::Push(Cond::Al, vec![dest_reg]));
                    dest_reg
                } else {
                    registers[0]
                };
                trans_expr(
                    arm_stats,
                    expr,
                    t.as_ref().unwrap(),
                    array_index_reg,
                    registers,
                    stack_offset,
                    symbol_table,
                    id_map,
                    string_literals,
                );
                let array_ptr_reg = if do_register_fallback {
                    arm_stats.push(Stat::Pop(Cond::Al, vec![registers[0]]));
                    registers[0]
                } else {
                    dest_reg
                };
                arm_stats.push(Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    array_ptr_reg,
                    MemOperand::Zero(array_ptr_reg),
                ));
                arm_stats.push(Stat::Move(
                    MovOp::Mov,
                    Cond::Al,
                    false,
                    ARG_REGS[0],
                    FlexOperand::ShiftReg(array_index_reg, None),
                ));
                arm_stats.push(Stat::Move(
                    MovOp::Mov,
                    Cond::Al,
                    false,
                    ARG_REGS[1],
                    FlexOperand::ShiftReg(array_ptr_reg, None),
                ));
                arm_stats.push(Stat::Branch(
                    BranchOp::Bl,
                    Cond::Al,
                    "p_check_array_bounds".to_string(),
                ));
                arm_stats.push(Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    array_ptr_reg,
                    array_ptr_reg,
                    FlexOperand::Imm(4),
                ));
                arm_stats.push(Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    dest_reg,
                    array_ptr_reg,
                    FlexOperand::ShiftReg(
                        array_index_reg,
                        if is_last_expr && get_type_size(expr_type) == 4 {
                            Some(Shift::Lsl(2.into()))
                        } else {
                            None
                        },
                    ),
                ));
            }

            arm_stats.push(Stat::MemOp(
                if get_type_size(expr_type) == 1 {
                    MemOp::Ldrb
                } else {
                    MemOp::Ldr
                },
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Zero(dest_reg),
            ))
        }
        ast::Expr::UnOp(un_op, box ast::ASTWrapper(t, e)) => {
            trans_expr(
                arm_stats,
                e,
                t.as_ref().unwrap(),
                dest_reg,
                registers,
                stack_offset,
                symbol_table,
                id_map,
                string_literals,
            );

            match un_op {
                ast::UnOp::Minus => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Rsb,
                        Cond::Al,
                        true,
                        dest_reg,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Vs,
                        "p_throw_overflow_error".to_string(),
                    ));
                }
                ast::UnOp::Neg => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Eor,
                        Cond::Al,
                        false,
                        dest_reg,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                }
                ast::UnOp::Len => arm_stats.push(Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    true,
                    dest_reg,
                    MemOperand::Zero(dest_reg),
                )),
                ast::UnOp::Ord => {}
                ast::UnOp::Chr => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::And,
                        Cond::Al,
                        false,
                        dest_reg,
                        dest_reg,
                        FlexOperand::Imm(0xFF),
                    ));
                }
                ast::UnOp::Fst => {
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        ARG_REGS[0],
                        FlexOperand::ShiftReg(dest_reg, None),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "p_check_null_pointer".to_string(),
                    ));
                    arm_stats.push(Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        dest_reg,
                        MemOperand::Zero(dest_reg),
                    ));
                    let ldr_instr = if let Type::Pair(box fst, _) = t.as_ref().unwrap() {
                        if get_type_size(fst) == 1 {
                            MemOp::Ldrb
                        } else {
                            MemOp::Ldr
                        }
                    } else {
                        panic!("Fst expression should only have type pair")
                    };
                    arm_stats.push(Stat::MemOp(
                        ldr_instr,
                        Cond::Al,
                        false,
                        dest_reg,
                        MemOperand::Zero(dest_reg),
                    ));
                }
                ast::UnOp::Snd => {
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        ARG_REGS[0],
                        FlexOperand::ShiftReg(dest_reg, None),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "p_check_null_pointer".to_string(),
                    ));
                    arm_stats.push(Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        dest_reg,
                        MemOperand::PreIndex(dest_reg, FlexOffset::Expr(4.into()), false),
                    ));
                    let ldr_instr = if let Type::Pair(_, box snd) = t.as_ref().unwrap() {
                        if get_type_size(snd) == 1 {
                            MemOp::Ldrb
                        } else {
                            MemOp::Ldr
                        }
                    } else {
                        panic!("Fst expression should only have type pair")
                    };
                    arm_stats.push(Stat::MemOp(
                        ldr_instr,
                        Cond::Al,
                        false,
                        dest_reg,
                        MemOperand::Zero(dest_reg),
                    ));
                }
            }
        }
        Expr::BinOp(
            box ast::ASTWrapper(t1, e1),
            ast::BinOp::Newpair,
            box ast::ASTWrapper(t2, e2),
        ) => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                ARG_REGS[0],
                MemOperand::Expression(8),
            ));
            arm_stats.push(Stat::Branch(BranchOp::Bl, Cond::Al, "malloc".to_string()));
            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                dest_reg,
                FlexOperand::ShiftReg(RET_REG, None),
            ));

            assert!(registers.len() > 1);
            let sub_expr_dest = registers[0];

            // translate e1
            trans_expr(
                arm_stats,
                e1,
                t1.as_ref().unwrap(),
                sub_expr_dest,
                &registers[1..],
                stack_offset,
                symbol_table,
                id_map,
                string_literals,
            );

            // allocate space for e1
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                ARG_REGS[0],
                MemOperand::Expression(get_type_size(t1.as_ref().unwrap())),
            ));
            arm_stats.push(Stat::Branch(BranchOp::Bl, Cond::Al, "malloc".to_string()));

            // store e1 into its allocated space
            arm_stats.push(Stat::MemOp(
                if get_type_size(t1.as_ref().unwrap()) == 1 {
                    MemOp::Strb
                } else {
                    MemOp::Str
                },
                Cond::Al,
                false,
                sub_expr_dest,
                MemOperand::Zero(RET_REG),
            ));
            arm_stats.push(Stat::MemOp(
                MemOp::Str,
                Cond::Al,
                false,
                RET_REG,
                MemOperand::Zero(dest_reg),
            ));

            // translate e2
            trans_expr(
                arm_stats,
                e2,
                t2.as_ref().unwrap(),
                sub_expr_dest,
                registers,
                stack_offset,
                symbol_table,
                id_map,
                string_literals,
            );

            // allocate space for e2
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                ARG_REGS[0],
                MemOperand::Expression(get_type_size(t2.as_ref().unwrap())),
            ));
            arm_stats.push(Stat::Branch(BranchOp::Bl, Cond::Al, "malloc".to_string()));

            // store e2 into its allocated space
            arm_stats.push(Stat::MemOp(
                if get_type_size(t2.as_ref().unwrap()) == 1 {
                    MemOp::Strb
                } else {
                    MemOp::Str
                },
                Cond::Al,
                false,
                sub_expr_dest,
                MemOperand::Zero(RET_REG),
            ));
            arm_stats.push(Stat::MemOp(
                MemOp::Str,
                Cond::Al,
                false,
                RET_REG,
                MemOperand::PreIndex(dest_reg, FlexOffset::Expr(4.into()), false),
            ));
        }
        Expr::BinOp(box ast::ASTWrapper(t1, e1), bin_op, box ast::ASTWrapper(t2, e2)) => {
            trans_expr(
                arm_stats,
                e1,
                t1.as_ref().unwrap(),
                dest_reg,
                registers,
                stack_offset,
                symbol_table,
                id_map,
                string_literals,
            );

            let do_register_fallback = registers.len() == 1;

            let e2_dest = if do_register_fallback {
                arm_stats.push(Stat::Push(Cond::Al, vec![dest_reg]));
                dest_reg
            } else {
                registers[0]
            };

            trans_expr(
                arm_stats,
                e2,
                t2.as_ref().unwrap(),
                e2_dest,
                if do_register_fallback {
                    registers
                } else {
                    &registers[1..]
                },
                stack_offset + (do_register_fallback as i32) * PUSH_SIZE,
                symbol_table,
                id_map,
                string_literals,
            );

            let e1_dest = if do_register_fallback {
                arm_stats.push(Stat::Pop(Cond::Al, vec![registers[0]]));
                registers[0]
            } else {
                dest_reg
            };

            match bin_op {
                ast::BinOp::Add => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        true,
                        dest_reg,
                        e1_dest,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Vs,
                        "p_throw_overflow_error".to_string(),
                    ));
                }
                ast::BinOp::Sub => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Sub,
                        Cond::Al,
                        true,
                        dest_reg,
                        e1_dest,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Vs,
                        "p_throw_overflow_error".to_string(),
                    ));
                }
                ast::BinOp::Mul => {
                    arm_stats.push(Stat::MulOp(
                        MulOp::SMulL,
                        Cond::Al,
                        true,
                        dest_reg,
                        registers[0],
                        e1_dest,
                        e2_dest,
                    ));
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        registers[0],
                        FlexOperand::ShiftReg(dest_reg, Some(Shift::Asr(31.into()))),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Ne,
                        "p_throw_overflow_error".to_string(),
                    ));
                }
                ast::BinOp::Div => {
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Register::R0,
                        FlexOperand::ShiftReg(e1_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Register::R1,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));

                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "p_check_divide_by_zero".to_string(),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "__aeabi_idiv".to_string(),
                    ));

                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        dest_reg,
                        FlexOperand::ShiftReg(Register::R0, None),
                    ));
                }
                ast::BinOp::Mod => {
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Register::R0,
                        FlexOperand::ShiftReg(e1_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Register::R1,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));

                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "p_check_divide_by_zero".to_string(),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Al,
                        "__aeabi_idivmod".to_string(),
                    ));

                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        dest_reg,
                        FlexOperand::ShiftReg(Register::R1, None),
                    ));
                }
                ast::BinOp::Gt => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Gt,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Le,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::Gte => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Ge,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Lt,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::Lt => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Lt,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Ge,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::Lte => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Le,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Gt,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::Eq => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Eq,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Ne,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::Ne => {
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Ne,
                        false,
                        dest_reg,
                        FlexOperand::Imm(1),
                    ));
                    arm_stats.push(Stat::Move(
                        MovOp::Mov,
                        Cond::Eq,
                        false,
                        dest_reg,
                        FlexOperand::Imm(0),
                    ));
                }
                ast::BinOp::And => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::And,
                        Cond::Al,
                        false,
                        dest_reg,
                        e1_dest,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                }
                ast::BinOp::Or => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Orr,
                        Cond::Al,
                        false,
                        dest_reg,
                        e1_dest,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                }
                ast::BinOp::Newpair => unreachable!(),
            }
        }
    };
}
