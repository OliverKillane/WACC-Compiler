use super::arm_repr::*;
use super::trans_expr::{
    is_flex_imm, trans_expr, translate_variable_get, ARG_REGS, PUSH_SIZE, RET_REG,
};
use crate::ast;
use crate::ast::{ASTWrapper, AssignLhs, AssignRhs, Expr, StatWrap, Type};
use crate::frontend::semantic::symbol_table::VariableSymbolTable;
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

fn trans_rhs(
    arm_stats: &mut Vec<Stat>,
    rhs: &AssignRhs<Option<Type>, usize>,
    dest_reg: Register,
    registers: &[Register],
    symbol_table: &HashMap<usize, ast::Type>,
    id_map: &HashMap<usize, i32>,
    string_literals: &mut HashMap<String, usize>,
) {
    match rhs {
        AssignRhs::Expr(ASTWrapper(Some(expr_type), expr)) => {
            trans_expr(
                arm_stats,
                expr,
                expr_type,
                dest_reg,
                registers,
                0,
                symbol_table,
                id_map,
                string_literals,
            );
        }
        AssignRhs::Array(ASTWrapper(Some(Type::Array(box t, _)), exprs)) => {
            // writeln!(f, "\tLDR R0, ={}", get_type_size(t) * exprs.len() + 4)?;
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                Register::R0,
                MemOperand::Expression(get_type_size(t) * (exprs.len() as i32) + 4),
            ));
            // writeln!(f, "\tBL malloc")?;
            arm_stats.push(Stat::Branch(BranchOp::Bl, Cond::Al, "malloc".to_string()));
            // writeln!(f, "\tMOV {}, R0", dest_reg)?;
            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                dest_reg,
                FlexOperand::ShiftReg(Register::R0, None),
            ));

            let tmp_reg = registers.get(0).unwrap();

            for (i, ASTWrapper(Some(expr_type), expr)) in exprs.iter().enumerate() {
                trans_expr(
                    arm_stats,
                    expr,
                    expr_type,
                    *tmp_reg,
                    registers,
                    0,
                    symbol_table,
                    id_map,
                    string_literals,
                );
                // take a look at this, probs wrong
                arm_stats.push(Stat::MemOp(
                    if get_type_size(expr_type) == 1 {
                        MemOp::Strb
                    } else {
                        MemOp::Str
                    },
                    Cond::Al,
                    false,
                    *tmp_reg,
                    MemOperand::Expression(get_type_size(expr_type) * (i as i32) + 4),
                ));
                // writeln!(
                //     f,
                //     "\tSTR{} {}, [{}, #{}]",
                //     get_type_suffix(t),
                //     tmp_reg,
                //     dest_reg,
                //     get_type_size(t) * i + 4
                // )?;
            }

            // writeln!(f, "\tLDR {}, ={}", tmp_reg, exprs.len())?;
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                *tmp_reg,
                MemOperand::Expression(exprs.len() as i32),
            ));

            // writeln!(f, "\tSTR {}, [{}]", tmp_reg, dest_reg)?;
            arm_stats.push(Stat::MemOp(
                MemOp::Str,
                Cond::Al,
                false,
                *tmp_reg,
                MemOperand::Zero(dest_reg),
            ));
        }
        AssignRhs::Call(_, _) => todo!(),
        _ => panic!("Illegal rhs found: {:?}", rhs),
    };
}

fn trans_lhs(
    arm_stats: &mut Vec<Stat>,
    lhs: &ast::AssignLhs<Option<Type>, usize>,
    dest_reg: Register,
    registers: &[Register],
    symbol_table: &HashMap<usize, ast::Type>,
    id_map: &HashMap<usize, i32>,
    string_literals: &mut HashMap<String, usize>,
) -> bool {
    match lhs {
        AssignLhs::ArrayElem(id, exprs) => {
            let expr_type = if let Type::Array(box expr_type, _) = symbol_table.get(id).unwrap() {
                expr_type
            } else {
                panic!("Variable indexed should be an array")
            };
            let id_offset = id_map[id];
            if is_flex_imm(id_offset) {
                arm_stats.push(Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    dest_reg,
                    Register::Sp,
                    FlexOperand::Imm(id_offset as u32),
                ));
            } else {
                arm_stats.push(Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    dest_reg,
                    MemOperand::Expression(id_offset),
                ));
                arm_stats.push(Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    dest_reg,
                    Register::Sp,
                    FlexOperand::ShiftReg(dest_reg, None),
                ));
            }

            let mut non_final_exprs = &exprs[..exprs.len() - 1];
            for (expr_idx, ast::ASTWrapper(t, expr)) in exprs.iter().enumerate() {
                let is_last_expr = expr_idx == exprs.len() - 1;

                let array_index_reg = registers[0];
                trans_expr(
                    arm_stats,
                    expr,
                    t.as_ref().unwrap(),
                    array_index_reg,
                    registers,
                    0,
                    symbol_table,
                    id_map,
                    string_literals,
                );
                let array_ptr_reg = dest_reg;
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
            get_type_size(expr_type) == 4
        }
        AssignLhs::PairFst(ASTWrapper(Some(t @ Type::Pair(box lt, _)), e)) => {
            trans_expr(
                arm_stats,
                e,
                t,
                dest_reg,
                registers,
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            // check if pair ptr is not null.
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

            // deref the first ptr of pair.
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::Zero(dest_reg),
            ));

            get_type_size(lt) == 4
        }
        AssignLhs::PairSnd(ASTWrapper(Some(t @ Type::Pair(_, box rt)), e)) => {
            trans_expr(
                arm_stats,
                e,
                t,
                dest_reg,
                registers,
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            // check if pair ptr is not null.
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

            // deref the second ptr of pair.
            arm_stats.push(Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::PreIndex(dest_reg, FlexOffset::Expr(4.into()), false),
            ));

            get_type_size(rt) == 4
        }
        _ => panic!("Expected pointer-calculation enum variants"),
    }
}

const REGISTERS: [Register; 9] = [
    Register::R4,
    Register::R5,
    Register::R6,
    Register::R7,
    Register::R8,
    Register::R9,
    Register::R10,
    Register::R11,
    Register::R12,
];

fn trans_stats(
    arm_stats: &mut Vec<Stat>,
    stats: &Vec<StatWrap<Option<Type>, usize>>,
    symbol_table: &VariableSymbolTable,
    string_literals: &mut HashMap<String, usize>,
) {
    let mut used_stack_space = 0;
    let mut id_space: Vec<(_, _)> = symbol_table
        .0
        .clone()
        .into_iter()
        .map(|(id, typ)| (id, get_type_size(&typ)))
        .collect();
    id_space.sort_by_key(|(id, _)| *id);
    id_space.reverse();

    for (id, space) in id_space.iter_mut() {
        let tmp = used_stack_space;
        used_stack_space += *space;
        *space = tmp;
    }

    let id_map: HashMap<usize, i32> = id_space.into_iter().collect();

    arm_stats.push(Stat::ApplyOp(
        RegOp::Sub,
        Cond::Al,
        false,
        Register::Sp,
        Register::Sp,
        FlexOperand::Imm(used_stack_space as u32),
    ));

    for ASTWrapper(_, stat) in stats {}

    arm_stats.push(Stat::ApplyOp(
        RegOp::Sub,
        Cond::Al,
        false,
        Register::Sp,
        Register::Sp,
        FlexOperand::Imm(used_stack_space as u32),
    ));
}

fn trans_stat(
    arm_stats: &mut Vec<Stat>,
    stat: &ast::Stat<Option<Type>, usize>,
    symbol_table: &HashMap<usize, ast::Type>,
    id_map: &HashMap<usize, i32>,
    string_literals: &mut HashMap<String, usize>,
    label_counter: &mut usize,
) {
    match stat {
        ast::Stat::Skip => {}
        ast::Stat::Def(_, id, rhs) | ast::Stat::Assign(AssignLhs::Var(id), rhs) => {
            let dest_reg = REGISTERS[0];
            trans_rhs(
                arm_stats,
                &rhs,
                REGISTERS[0],
                &REGISTERS[1..],
                symbol_table,
                id_map,
                string_literals,
            );

            arm_stats.push(Stat::MemOp(
                MemOp::Strb,
                Cond::Al,
                false,
                dest_reg,
                MemOperand::PreIndex(Register::Sp, FlexOffset::Expr(id_map[&id].into()), false),
            ));
        }
        ast::Stat::Assign(lhs, rhs) => {
            let expression_reg = REGISTERS[0];
            trans_rhs(
                arm_stats,
                &rhs,
                expression_reg,
                &REGISTERS[1..],
                symbol_table,
                id_map,
                string_literals,
            );
            let ptr_reg = REGISTERS[1];
            let str_instr = if trans_lhs(
                arm_stats,
                &lhs,
                ptr_reg,
                &REGISTERS[2..],
                symbol_table,
                id_map,
                string_literals,
            ) {
                MemOp::Str
            } else {
                MemOp::Strb };for ASTWrapper(_, s) in body_stats {
                    trans_stat(
                        arm_stats,
                        s,
                        symbol_table,
                        id_map,
                        string_literals,
                        label_counter,
                    );
                } for ASTWrapper(_, s) in body_stats {
                trans_stat(
                    arm_stats,
                    s,
                    symbol_table,
                    id_map,
                    string_literals,
                    label_counter,
                );
            }
            };
            arm_stats.push(Stat::MemOp(
                str_instr,
                Cond::Al,
                false,
                expression_reg,
                MemOperand::Zero(ptr_reg),
            ));
        }
        ast::Stat::Read(AssignLhs::Var(id)) => {
            todo!()
        }
        ast::Stat::Free(_) => todo!(),
        ast::Stat::Return(ASTWrapper(Some(t),  expr)) => {
            
        },
        ast::Stat::Exit(ASTWrapper(_, expr)) => {
            let dest_reg = REGISTERS[0];

            trans_expr(
                arm_stats,
                &expr,
                &ast::Type::Int,
                dest_reg,
                &REGISTERS[1..],
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                Register::R0,
                FlexOperand::ShiftReg(dest_reg, None),
            ));
            arm_stats.push(Stat::Branch(BranchOp::Bl, Cond::Al, "exit".to_string()))
        }
        ast::Stat::Print(ASTWrapper(Some(t), expr)) => {
            let dest_reg = REGISTERS[0];

            trans_expr(
                arm_stats,
                &expr,
                &t,
                dest_reg,
                &REGISTERS[1..],
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                Register::R0,
                FlexOperand::ShiftReg(dest_reg, None),
            ));

            arm_stats.push(Stat::Branch(
                BranchOp::Bl,
                Cond::Al,
                format!(
                    "p_print_{}",
                    match t {
                        Type::Int => "int",
                        Type::Bool => "bool",
                        Type::Char => "char",
                        Type::String => "string",
                        Type::Pair(_, _) => "reference",
                        Type::Array(_, _) => "reference",
                        t =>
                            panic!("Only allowd prints are int, bool, char, string, or reference!"),
                    }
                ),
            ));
        }
        ast::Stat::PrintLn(ASTWrapper(Some(t), expr)) => {
            let dest_reg = REGISTERS[0];

            trans_expr(
                arm_stats,
                &expr,
                &t,
                dest_reg,
                &REGISTERS[1..],
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            arm_stats.push(Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                Register::R0,
                FlexOperand::ShiftReg(dest_reg, None),
            ));
            arm_stats.push(Stat::Branch(
                BranchOp::Bl,
                Cond::Al,
                format!(
                    "p_print_{}",
                    match t {
                        Type::Int => "int",
                        Type::Bool => "bool",
                        Type::Char => "char",
                        Type::String => "string",
                        Type::Pair(_, _) => "reference",
                        Type::Array(_, _) => "reference",
                        t =>
                            panic!("Only allowd prints are int, bool, char, string, or reference!"),
                    }
                ),
            ));
            arm_stats.push(Stat::Branch(
                BranchOp::Bl,
                Cond::Al,
                "p_println".to_string(),
            ));
        }
        ast::Stat::If(ASTWrapper(Some(t), expr), true_stats, false_stats) => {
            let dest_reg = REGISTERS[0];

            trans_expr(
                arm_stats,
                &expr,
                &t,
                dest_reg,
                &REGISTERS[1..],
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            arm_stats.push(Stat::Cmp(
                CmpOp::Cmp,
                Cond::Al,
                dest_reg,
                FlexOperand::Imm(0),
            ));

            let false_label = *label_counter;
            *label_counter += 1;

            let true_label = *label_counter;
            *label_counter += 1;

            arm_stats.push(Stat::Branch(
                BranchOp::B,
                Cond::Eq,
                format!("L{}", false_label),
            ));

            // If true statements
            for ASTWrapper(_, s) in true_stats {
                trans_stat(
                    arm_stats,
                    s,
                    symbol_table,
                    id_map,
                    string_literals,
                    label_counter,
                );
            }

            // Branch to true label
            arm_stats.push(Stat::Branch(
                BranchOp::B,
                Cond::Al,
                format!("L{}", true_label),
            ));

            // If false label
            arm_stats.push(Stat::Label(format!("L{}", false_label)));

            // If false statements
            for ASTWrapper(_, s) in false_stats {
                trans_stat(
                    arm_stats,
                    s,
                    symbol_table,
                    id_map,
                    string_literals,
                    label_counter,
                );
            }

            // If true label
            arm_stats.push(Stat::Label(format!("L{}", true_label)));
        }
        ast::Stat::While(ASTWrapper(Some(t), expr), body_stats) => {
            // Get body label and compare label

            let body_label = *label_counter;
            *label_counter += 1;

            let compare_label = *label_counter;
            *label_counter += 1;

            // Jump to compare label
            arm_stats.push(Stat::Branch(
                BranchOp::B,
                Cond::Al,
                format!("L{}", compare_label),
            ));

            // Body label
            arm_stats.push(Stat::Label(format!("L{}", body_label)));

            // Body statements
            for ASTWrapper(_, s) in body_stats {
                trans_stat(
                    arm_stats,
                    s,
                    symbol_table,
                    id_map,
                    string_literals,
                    label_counter,
                );
            }

            // Compare label
            arm_stats.push(Stat::Label(format!("L{}", compare_label)));

            // Compare expression

            let dest_reg = REGISTERS[0];

            trans_expr(
                arm_stats,
                &expr,
                &t,
                dest_reg,
                &REGISTERS[1..],
                0,
                symbol_table,
                id_map,
                string_literals,
            );

            // Cond. Jump to body label
            arm_stats.push(Stat::Cmp(
                CmpOp::Cmp,
                Cond::Al,
                dest_reg,
                FlexOperand::Imm(1),
            ));

            arm_stats.push(Stat::Branch(
                BranchOp::B,
                Cond::Eq,
                format!("L{}", body_label),
            ));
        }
        ast::Stat::Block(body_stats) => {
            for ASTWrapper(_, s) in body_stats {
                trans_stat(
                    arm_stats,
                    s,
                    symbol_table,
                    id_map,
                    string_literals,
                    label_counter,
                );
            }
        }
        s => panic!("Unexpected statement: {:?}", s),
    }
}
