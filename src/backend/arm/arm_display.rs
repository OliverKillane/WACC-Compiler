//! Converts [ARM Representation](Program) into a string format, suitable for
//! assembly.

use lazy_static::__Deref;

use super::arm_repr::*;
use std::{collections::HashMap, fmt::Display};

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ident::Temp(t) => write!(f, "T{}", t),
            Ident::Reg(r) => write!(f, "{}", r),
        }
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Cond::Eq => "EQ",
                Cond::Ne => "NE",
                Cond::Hs => "HS",
                Cond::Lo => "LO",
                Cond::Mi => "MI",
                Cond::Pl => "PL",
                Cond::Vs => "VS",
                Cond::Vc => "VC",
                Cond::Hi => "HI",
                Cond::Ls => "LS",
                Cond::Ge => "GE",
                Cond::Lt => "LT",
                Cond::Gt => "GT",
                Cond::Le => "LE",
                Cond::Al => "",
            }
        )
    }
}
impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Register::R0 => "R0",
                Register::R1 => "R1",
                Register::R2 => "R2",
                Register::R3 => "R3",
                Register::R4 => "R4",
                Register::R5 => "R5",
                Register::R6 => "R6",
                Register::R7 => "R7",
                Register::R8 => "R8",
                Register::R9 => "R9",
                Register::R10 => "R10",
                Register::R11 => "R11",
                Register::R12 => "R12",
                Register::Sp => "sp",
                Register::Lr => "lr",
                Register::Pc => "pc",
            }
        )
    }
}

impl Display for Shift {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Shift::Asr(n) => format!("ASR #{}", i32::from(*n)),
                Shift::Lsl(n) => format!("LSL #{}", i32::from(*n)),
                Shift::Lsr(n) => format!("LSR #{}", i32::from(*n)),
                Shift::Ror(n) => format!("ROR #{}", i32::from(*n)),
                Shift::Rrx => "RRX".into(),
            }
        )
    }
}

impl Display for FlexOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FlexOperand::Imm(n) => format!("#{}", n),
                FlexOperand::ShiftReg(reg, Some(shift)) => format!("{},\t{}", reg, shift),
                FlexOperand::ShiftReg(reg, None) => format!("{}", reg),
            }
        )
    }
}

impl Display for RegOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RegOp::Add => "ADD",
                RegOp::Sub => "SUB",
                RegOp::Rsb => "RSB",
                RegOp::Adc => "ADC",
                RegOp::Sbc => "SBC",
                RegOp::Rsc => "RSC",
                RegOp::And => "AND",
                RegOp::Orr => "ORR",
                RegOp::Eor => "EOR",
                RegOp::Bic => "BIC",
            }
        )
    }
}

impl Display for MovOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MovOp::Mov => "MOV",
                MovOp::Mvn => "MVN",
            }
        )
    }
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CmpOp::Cmp => "CMP",
                CmpOp::Cmn => "CMN",
                CmpOp::Tst => "TST",
                CmpOp::Teq => "TEQ",
            }
        )
    }
}

impl Display for SatOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SatOp::Add => "QADD",
                SatOp::Sub => "QSUB",
                SatOp::DAdd => "QDADD",
                SatOp::DSub => "QDSUB",
            }
        )
    }
}

impl Display for MulOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MulOp::UMulL => "UMULL",
                MulOp::UMlAL => "UMLAL",
                MulOp::SMulL => "SMULL",
                MulOp::SMlAL => "SMLAL",
            }
        )
    }
}

impl Display for MemOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MemOp::Ldr => "LDR",
                MemOp::Str => "STR",
            }
        )
    }
}

impl Display for MemOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MemOperand::Zero(reg) => format!("[{}]", reg),
                MemOperand::PreIndex(reg, flex) => format!("[{}, {}]", reg, flex),
                MemOperand::Label(label) => format!("={}", display_data_ref(*label)),
                MemOperand::Expression(expr) => format!("={}", expr),
            }
        )
    }
}

impl Display for FlexOffset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FlexOffset::Expr(expr) => format!("#{}", i32::from(*expr)),
                FlexOffset::ShiftReg(minus, reg, shift) => format!(
                    "{}{}{}",
                    if *minus { "-" } else { "" },
                    reg,
                    shift.map_or_else(String::new, |shift| format!(", {}", shift))
                ),
            }
        )
    }
}

impl Display for Stat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let conv = |s: &bool| if *s { "S".to_string() } else { "".to_string() };
        match self {
            Stat::ApplyOp(op, cond, s, dest, operand, operand2) => write!(
                f,
                "\t{}{}{}\t{},\t{},\t{}",
                op,
                cond,
                conv(s),
                dest,
                operand,
                operand2
            ),
            Stat::Mul(cond, s, dest, operand, operand2) => write!(
                f,
                "\tMUL{}{}\t{},\t{},\t{}",
                cond,
                conv(s),
                dest,
                operand,
                operand2
            ),
            Stat::MulA(cond, s, dest, operand, operand2, operand3) => write!(
                f,
                "\tMULA{}{}\t{},\t{},\t{},\t{}",
                cond,
                conv(s),
                dest,
                operand,
                operand2,
                operand3
            ),
            Stat::MulOp(op, cond, s, hi, lo, operand, operand2) => write!(
                f,
                "\t{}{}{}\t{},\t{},\t{},\t{}",
                op,
                cond,
                conv(s),
                hi,
                lo,
                operand,
                operand2
            ),
            Stat::Move(op, cond, s, ident, operand) => {
                write!(f, "\t{}{}{}\t{},\t{}", op, cond, conv(s), ident, operand)
            }
            Stat::Cmp(op, cond, ident, operand) => {
                write!(f, "\t{}{}\t{},\t{}", op, cond, ident, operand)
            }
            Stat::SatOp(op, cond, dest, operand, operand2) => {
                write!(f, "\t{}{}\t{},\t{},\t{}", op, cond, dest, operand, operand2)
            }
            Stat::ReadCPSR(reg) => write!(f, "\tMSR\t{},\tCPSR", reg),
            Stat::MemOp(op, cond, s, ident, operand) => {
                write!(
                    f,
                    "\t{}{}{}\t{},\t{}",
                    op,
                    cond,
                    if *s { "B" } else { "" },
                    ident,
                    operand
                )
            }
            Stat::Push(cond, ident) => write!(f, "\tPUSH{}\t{{{}}}", cond, ident,),
            Stat::Pop(cond, ident) => write!(f, "\tPOP{}\t{{{}}}", cond, ident,),
            Stat::Link(cond, link_to) => write!(f, "\tBL{}\t{}", cond, link_to),
            Stat::Call(fun_name, ret_temp, arg_temps) => write!(
                f,
                "\tINTERNAL OPERATION: CALL\t{}\t{}, ARGS({})",
                fun_name,
                match ret_temp {
                    Some(t) => format!("T{}", t),
                    None => "No Return".to_string(),
                },
                arg_temps
                    .iter()
                    .map(|t| format!("T{}", t))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Stat::AssignStackWord(ident) => {
                write!(f, "\tINTERNAL OPERATION: ASSIGN WORD OF STACK TO {}", ident)
            }
            Stat::Nop => write!(f, "\tNOP"),
        }
    }
}

fn display_routine(
    start_node: &ArmNode,
    name: &str,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    // Used to store label numbers based on the nodes connected, and a
    // boolean describing if their code has been generated yet.
    let mut label_map: HashMap<ArmNode, (usize, bool)> = HashMap::new();

    // Literal pools are already placed after unconditional jumps, we add in
    // extra literal pools where required using unconditional jumps.
    //
    // This is done by placing a pool every 2000 instructions (literal pools always <= 4000 bytes away).
    // Starts at 1000 since there are no literal pools at the start of the program.
    let mut since_lit = 1000;
    let mut lit_branch_ident = 0;

    // Label identifier conversion to strings.
    let label_conv = |id: &usize| format!("b_{}_{}", name, id);

    let lit_label_conv = |id: &usize| format!("blit_{}_{}", name, id);

    let mut current = start_node.clone();

    writeln!(f, "{}:", name)?;

    loop {
        loop {
            if since_lit > 2000 {
                writeln!(f, "\tB\t{}", lit_label_conv(&lit_branch_ident))?;
                writeln!(f, "\t.ltorg")?;
                writeln!(f, "{}:", lit_label_conv(&lit_branch_ident))?;
                lit_branch_ident += 1;
                since_lit = 0;
            }

            match current.clone().get().deref() {
                ControlFlow::Simple(_, stat, next) => {
                    since_lit += 1;
                    // write the statement
                    writeln!(f, "{}", stat)?;

                    // check next:
                    if let Some(next_node) = next {
                        if let Some((id, _)) = label_map.get(next_node) {
                            // next node already translated, so place a branch break
                            writeln!(f, "\tB\t{}", label_conv(id))?;
                            break;
                        } else {
                            current = next_node.clone();
                        }
                    } else {
                        break;
                    }
                }
                ControlFlow::Branch(_, branch_next, cond, next) => {
                    since_lit += 1;
                    if let Some((id, _)) = label_map.get(branch_next) {
                        writeln!(f, "\tB{}\t{}", cond, label_conv(id))?;
                    } else {
                        let new_id = label_map.len();
                        label_map.insert(branch_next.clone(), (new_id, false));
                        writeln!(f, "\tB{}\t{}", cond, label_conv(&new_id))?;
                    }

                    // check next:
                    if let Some(next_node) = next {
                        if let Some((id, _)) = label_map.get(next_node) {
                            // next node already translated, so place a branch break
                            writeln!(f, "\tB\t{}", label_conv(id))?;
                            break;
                        } else {
                            current = next_node.clone();
                        }
                    } else {
                        break;
                    }
                }
                ControlFlow::Ltorg(_) => {
                    since_lit = 0;
                    writeln!(f, "\t.ltorg")?;
                    break;
                }
                ControlFlow::Return(_, ret) => {
                    writeln!(
                        f,
                        "\tINTERNAL OPERATION: RETURN\t{}",
                        if let Some(ret_temp) = ret {
                            format! {"T{}", ret_temp}
                        } else {
                            "no value returned".to_string()
                        }
                    )?;
                    break;
                }
                ControlFlow::Multi(_, next) => {
                    match label_map.get_mut(&current) {
                        Some((id, t @ false)) => {
                            writeln!(f, "{}:", label_conv(id))?;
                            *t = true;
                        }
                        Some((_, true)) => break,
                        None => {
                            let new_id = label_map.len();
                            label_map.insert(current.clone(), (new_id, true));
                            writeln!(f, "{}:", label_conv(&new_id))?;
                        }
                    }

                    // check next:
                    if let Some(next_node) = next {
                        if let Some((id, _)) = label_map.get(next_node) {
                            // next node already translated, so place a branch break
                            writeln!(f, "\tB\t{}", label_conv(id))?;
                            break;
                        } else {
                            current = next_node.clone();
                        }
                    } else {
                        break;
                    }
                }
                ControlFlow::Removed => panic!("Cannot display removed nodes"),
            }
        }

        // find the next node to write from, and place label
        current = {
            let mut next_node = None;
            for (node, (id, created)) in label_map.iter_mut() {
                if !*created {
                    *created = true;
                    writeln!(f, "{}:", label_conv(id))?;
                    next_node = Some(node.clone());
                    break;
                }
            }

            if let Some(node) = next_node {
                node
            } else {
                break;
            }
        };
    }

    writeln!(f)
}

/// Converts a data reference into its string representation
fn display_data_ref(ident: DataIdent) -> String {
    format!("d_ref_{}", ident)
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t")?;
        match self {
            DataType::Ascii(s) => {
                write!(f, ".ascii \"")?;
                for char in s.chars() {
                    match char {
                        '\0' => write!(f, "\\0")?,
                        '\n' => write!(f, "\\n")?,
                        c => write!(f, "{}", c)?,
                    }
                }
                writeln!(f, "\"")
            }
            DataType::Word(w) => writeln!(f, ".word {}", w),
            DataType::HalfWord(h) => writeln!(f, ".hword {}", h),
            DataType::Byte(b) => writeln!(f, ".byte {}", b),
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\t{}:\n{}",
            display_data_ref(self.0),
            self.1
                .iter()
                .map(|d| format!("\t{}", d))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

impl Display for ArmCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ArmCode {
            data,
            reserved_stack: _,
            temps: _,
            main,
            subroutines: functions,
            cfg: _,
        } = self;
        if !data.is_empty() {
            writeln!(f, ".data")?;
            for dataref in data {
                write!(f, "{}", dataref)?;
            }
        };

        writeln!(f, ".text\n.global main")?;
        display_routine(main, "main", f)?;

        for (
            fun_name,
            Subroutine {
                args: _,
                start_node,
                temps: _,
                reserved_stack: _,
            },
        ) in functions
        {
            display_routine(start_node, fun_name, f)?;
        }

        writeln!(f)
    }
}
