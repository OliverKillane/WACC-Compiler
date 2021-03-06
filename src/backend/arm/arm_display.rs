//! Converts [ARM Representation](ArmCode) into a string format, suitable for
//! assembly.
//!
//! Traverses along a linear chain of the graph, adding label entires for every
//! branch from the chain, and every node in the chain that has multiple
//! predecessors.
//!
//! Can then continually translate un-translated entries (potentially adding more
//! entires to the label map) in the label map until all are translated.

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
                MemOperand::Label(label, offset) => format!(
                    "={}{}",
                    display_data_ref(*label),
                    if *offset == 0 {
                        String::new()
                    } else {
                        format!(" + {}", offset)
                    }
                ),
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
                "\tCALL\t{} ({}) {} (INTERNAL OPERATION)",
                fun_name,
                arg_temps
                    .iter()
                    .map(|t| format!("T{}", t))
                    .collect::<Vec<_>>()
                    .join(","),
                if let Some(ret) = ret_temp {
                    format!("-> T{}", ret)
                } else {
                    "".to_string()
                }
            ),
            Stat::AssignStackWord(ident) => {
                write!(f, "\tASSIGN STACK WORD {} (INTERNAL OPERATION)", ident)
            }
            Stat::Nop => write!(f, "\tNOP"),
        }
    }
}

fn get_next_node(label_map: &HashMap<ArmNode, (usize, bool)>) -> Option<ArmNode> {
    for (node, (_, translated)) in label_map {
        if !*translated {
            return Some(node.clone());
        }
    }
    None
}

/// Display from a start node:
/// - Create the subroutine label
/// - Ensures all instructions are in range of a literal pool
/// - Determines the
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
    let mut since_lit = 500;
    let mut lit_branch_ident = 0;

    // Label identifier conversion to strings.
    let label_conv = |id: &usize| format!("b_{}_{}", name, id);

    let lit_label_conv = |id: &usize| format!("blit_{}_{}", name, id);

    let mut next = Some(start_node.clone());

    writeln!(f, "{}:", name)?;

    while next.is_some() {
        while let Some(current) = next {
            if since_lit >= 1000 {
                writeln!(f, "\tB\t{}", lit_label_conv(&lit_branch_ident))?;
                writeln!(f, "\t.ltorg")?;
                writeln!(f, "{}:", lit_label_conv(&lit_branch_ident))?;
                lit_branch_ident += 1;
                since_lit = 0;
            }

            match label_map.get_mut(&current) {
                Some((id, true)) => {
                    writeln!(f, "\tB\t{}\n\t.ltorg", label_conv(id))?;
                    since_lit = 0;
                    break;
                }
                Some((id, l)) => {
                    writeln!(f, "{}:", label_conv(id))?;
                    *l = true
                }
                None => (),
            }

            since_lit += 1;

            next = match current.get().deref() {
                ControlFlow::Simple(_, stat, next) => {
                    writeln!(f, "{}", stat)?;
                    next.clone()
                }
                ControlFlow::Branch(_, branch_true, cond, branch_false) => {
                    let id = if let Some((id, _)) = label_map.get(branch_true) {
                        *id
                    } else {
                        let new_id = label_map.len();
                        label_map.insert(branch_true.clone(), (new_id, false));
                        new_id
                    };

                    writeln!(f, "\tB{}\t{}", cond, label_conv(&id))?;

                    branch_false.clone()
                }
                ControlFlow::Ltorg(_) => {
                    writeln!(f, "\t.ltorg")?;
                    since_lit = 0;
                    None
                }
                ControlFlow::Return(_, ret) => {
                    writeln!(
                        f,
                        "\tRETURN\t{}(INTERNAL OPERATION)",
                        if let Some(ret_temp) = ret {
                            format! {"T{} ", ret_temp}
                        } else {
                            "".to_string()
                        }
                    )?;
                    None
                }
                ControlFlow::Multi(_, next) => {
                    if label_map.get(&current).is_none() {
                        let new_id = label_map.len();
                        label_map.insert(current.clone(), (new_id, true));
                        writeln!(f, "{}:", label_conv(&new_id))?;
                    }
                    next.clone()
                }
                ControlFlow::Removed => panic!("There should be no removed nodes"),
            };
        }
        next = get_next_node(&label_map);
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
                        '\t' => write!(f, "\\t")?,
                        '"' => write!(f, "\\\"")?,
                        '\r' => write!(f, "\\r")?,
                        '\u{0008}' => write!(f, "\\b")?,
                        '\u{0012}' => write!(f, "\\f")?,
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
                cfg: _,
            },
        ) in functions
        {
            display_routine(start_node, fun_name, f)?;
        }

        writeln!(f)
    }
}
