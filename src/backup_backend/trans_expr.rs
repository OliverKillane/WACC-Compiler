use super::arm_repr::*;
use crate::ast;
use crate::ast::{Expr, Type};
use std::collections::HashMap;

fn get_type_size(t: &Type) -> i32 {
    match t {
        Type::Bool | Type::Char => 1,
        Type::Int | Type::String | Type::Pair(_, _) | Type::Array(_, _) => 4,
        Type::Any | Type::Generic(_) => {
            panic!("Any type should never show up after semantic analysis")
        }
    }
}

const RET_REG: Register = Register::R0;
const PUSH_SIZE: i32 = 4;
fn trans_expr(
    arm_stats: &mut Vec<Stat>,
    expr: &Expr<Option<Type>, usize>,
    dest_reg: Register,
    mut registers: &[Register],
    push_count: i32,
    symbol_table: &HashMap<usize, ast::Type>,
    id_map: &HashMap<usize, i32>,
    string_literals: &mut HashMap<String, usize>,
) {
    match expr {
        Expr::Null => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(0),
            ));
        }
        Expr::Int(i) => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(*i),
            ));
        }
        Expr::Bool(b) => {
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Expression(if *b { 1 } else { 0 }),
            ));
        }
        Expr::Char(c) => {
            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                dest_reg,
                FlexOperand::Char((*c as u8 as i32).into()),
            ));
        }
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
        Expr::Var(id) => {
            let ldr_instr = if get_type_size(&symbol_table[id]) == 1 {
                MemOp::Ldrb
            } else {
                MemOp::Ldr
            };
            let id_offset = id_map[id] + push_count * PUSH_SIZE;
            if let 4095..=4095 = id_offset {
                arm_stats.push(Stat::MemOp(
                    ldr_instr,
                    Cond::Al,
                    false,
                    dest_reg,
                    MemOperand::PreIndex(Register::Sp, FlexOffset::Expr(id_offset.into()), false),
                ));
            } else {
                arm_stats.push(Stat::MemOp(
                    ldr_instr,
                    Cond::Al,
                    false,
                    dest_reg,
                    MemOperand::Expression(id_offset),
                ));
                arm_stats.push(Stat::MemOp(
                    ldr_instr,
                    Cond::Al,
                    false,
                    dest_reg,
                    MemOperand::PreIndex(
                        Register::Sp,
                        FlexOffset::ShiftReg(false, dest_reg, None),
                        false,
                    ),
                ));
            }
        }
        Expr::ArrayElem(id, exprs) => {
            // let array_index_tmp = registers.pop().unwrap();
            // let id_offset = id_map[id] + push_count * PUSH_SIZE;
            // arm_stats.push(Stat::ApplyOp(RegOp::Add, Cond::Al, false, ))
            // writeln!(f, "\tADD {}, sp, #{}", array_index_tmp,)?;
            // let mut non_final_exprs = exprs.clone();
            // let ASTWrapper(t, final_expr) = non_final_exprs.pop().unwrap();
            // for ASTWrapper(_, expr) in non_final_exprs.iter() {
            //     trans_expr(
            //         f,
            //         expr,
            //         array_index_tmp,
            //         registers.clone(),
            //         symbol_table,
            //         id_map,
            //         string_literals,
            //     )?;
            //     writeln!(f, "\tLDR {} [{}]", dest_reg, dest_reg)?;

            //     writeln!(f, "\tMOV R0 {}", array_index_tmp)?;
            //     writeln!(f, "\tMOV R1 {}", dest_reg)?;

            //     writeln!(f, "\tBL p_check_array_bounds")?;
            //     writeln!(f, "\tADD {}, {}, #4", dest_reg, dest_reg)?;
            //     writeln!(
            //         f,
            //         "\tADD {}, {}, {}, LSL #2",
            //         dest_reg, dest_reg, array_index_tmp
            //     )?;
            // }

            // trans_expr(
            //     f,
            //     &final_expr,
            //     array_index_tmp,
            //     registers.clone(),
            //     symbol_table,
            //     id_map,
            //     string_literals,
            // )?;
            // writeln!(f, "\tLDR {} [{}]", dest_reg, dest_reg)?;

            // writeln!(f, "\tMOV R0 {}", array_index_tmp)?;
            // writeln!(f, "\tMOV R1 {}", dest_reg)?;

            // writeln!(f, "\tBL p_check_array_bounds")?;
            // writeln!(f, "\tADD {}, {}, #4", dest_reg, dest_reg)?;
            // writeln!(f, "\tADD {}, {}, {}", dest_reg, dest_reg, array_index_tmp)?;

            // writeln!(
            //     f,
            //     "\tLDR{} {} [{}]",
            //     get_type_suffix(&t.unwrap()),
            //     dest_reg,
            //     dest_reg
            // )?;
        }
        ast::Expr::UnOp(un_op, box ast::ASTWrapper(t, e)) => {
            trans_expr(
                arm_stats,
                e,
                dest_reg,
                registers,
                push_count,
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
                        RET_REG,
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
                        RET_REG,
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
                RET_REG,
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
                sub_expr_dest,
                &registers[1..],
                push_count,
                symbol_table,
                id_map,
                string_literals,
            );

            // allocate space for e1
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                RET_REG,
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
                sub_expr_dest,
                registers,
                push_count,
                symbol_table,
                id_map,
                string_literals,
            );

            // allocate space for e2
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                RET_REG,
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
        Expr::BinOp(box ast::ASTWrapper(_, e1), bin_op, box ast::ASTWrapper(_, e2)) => {
            trans_expr(
                arm_stats,
                e1,
                dest_reg,
                registers,
                push_count,
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
                e2_dest,
                if do_register_fallback {
                    registers
                } else {
                    &registers[1..]
                },
                push_count + (do_register_fallback as i32),
                symbol_table,
                id_map,
                string_literals,
            );

            if do_register_fallback {
                arm_stats.push(Stat::Pop(Cond::Al, vec![registers[0]]));
            }

            match bin_op {
                ast::BinOp::Add => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Add,
                        Cond::Al,
                        true,
                        dest_reg,
                        dest_reg,
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
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                    arm_stats.push(Stat::Branch(
                        BranchOp::Bl,
                        Cond::Vs,
                        "p_throw_overflow_error".to_string(),
                    ));
                }
                ast::BinOp::Mul => {
                    // writeln!(
                    //     f,
                    //     "\tSMULL {}, {}, {}, {}",
                    //     dest_reg, e2_dest, dest_reg, e2_dest
                    // )?;
                    // writeln!(f, "\tCMP {}, {}, ASR #31", e2_dest, dest_reg)?;
                    // writeln!(f, "\tBLNE p_throw_overflow_error")?;
                    arm_stats.push(Stat::MulOp(
                        MulOp::SMulL,
                        Cond::Al,
                        true,
                        dest_reg,
                        e2_dest,
                        dest_reg,
                        e2_dest,
                    ));
                    arm_stats.push(Stat::Cmp(
                        CmpOp::Cmp,
                        Cond::Al,
                        e2_dest,
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
                        FlexOperand::ShiftReg(dest_reg, None),
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
                        FlexOperand::ShiftReg(dest_reg, None),
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
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                }
                ast::BinOp::Or => {
                    arm_stats.push(Stat::ApplyOp(
                        RegOp::Orr,
                        Cond::Al,
                        false,
                        dest_reg,
                        dest_reg,
                        FlexOperand::ShiftReg(e2_dest, None),
                    ));
                }
                ast::BinOp::Newpair => unreachable!(),
            }
        }
    };
}
