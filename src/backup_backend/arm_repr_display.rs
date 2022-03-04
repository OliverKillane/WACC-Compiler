//! Converts [ARM Representation](Program) into a string format, suitable for
//! assembly.

use super::arm_repr::*;
use std::fmt::Display;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}\n{}{}\n",
            if !self.0.is_empty() { ".data\n\n" } else { "" },
            self.0
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<_>>()
                .join("\n"),
            if !self.1.is_empty() { ".text\n\n" } else { "" },
            self.1
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Cond::Eq => "eq",
                Cond::Ne => "ne",
                Cond::Hs => "hs",
                Cond::Lo => "lo",
                Cond::Mi => "mi",
                Cond::Pl => "pl",
                Cond::Vs => "vs",
                Cond::Vc => "vc",
                Cond::Hi => "hi",
                Cond::Ls => "ls",
                Cond::Ge => "ge",
                Cond::Lt => "lt",
                Cond::Gt => "gt",
                Cond::Le => "le",
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
                Register::Sp => "SP",
                Register::Lr => "LR",
                Register::Pc => "PC",
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
                Shift::Asr(n) => format!("asr #{}", i32::from(*n)),
                Shift::Lsl(n) => format!("lsl #{}", i32::from(*n)),
                Shift::Lsr(n) => format!("lsr #{}", i32::from(*n)),
                Shift::Ror(n) => format!("ror #{}", i32::from(*n)),
                Shift::Rrx => "rrx".into(),
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
                FlexOperand::Char(c) => format!("#'{}'", (i32::from(*c) as u8 as char)),
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
                RegOp::Add => "add",
                RegOp::Sub => "sub",
                RegOp::Rsb => "rsb",
                RegOp::Adc => "adc",
                RegOp::Sbc => "sbc",
                RegOp::Rsc => "rsc",
                RegOp::And => "and",
                RegOp::Orr => "orr",
                RegOp::Eor => "eor",
                RegOp::Bic => "bic",
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
                MovOp::Mov => "mov",
                MovOp::Mvn => "mvn",
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
                CmpOp::Cmp => "cmp",
                CmpOp::Cmn => "cmn",
                CmpOp::Tst => "tst",
                CmpOp::Teq => "teq",
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
                SatOp::Add => "qadd",
                SatOp::Sub => "qsub",
                SatOp::DAdd => "qdadd",
                SatOp::DSub => "qdsub",
            }
        )
    }
}

impl Display for BranchOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BranchOp::B => "b",
                BranchOp::Bl => "bl",
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
                MulOp::UMulL => "umull",
                MulOp::UMlAL => "umlal",
                MulOp::SMulL => "smull",
                MulOp::SMlAL => "smlal",
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
                MemOp::Ldr => "ldr",
                MemOp::Str => "str",
                MemOp::Ldrb => "ldrb",
                MemOp::Strb => "strb",
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
                MemOperand::PreIndex(reg, flex, bang) =>
                    format!("[{}, {}]{}", reg, flex, if *bang { "!" } else { "" }),
                MemOperand::Label(label) => format!("={}", label),
                MemOperand::Expression(expr) => format!("={}", expr),
                MemOperand::PostIndex(reg, flex) => format!("[{}], {}", reg, flex),
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
        let conv = |s: &bool| if *s { "S".to_owned() } else { String::from("") };
        write!(
            f,
            "{}",
            match self {
                Stat::ApplyOp(op, cond, s, dest, operand, operand2) => format!(
                    "\t{}{}{}\t{},\t{},\t{}",
                    op,
                    cond,
                    conv(s),
                    dest,
                    operand,
                    operand2
                ),
                Stat::Mul(cond, s, dest, operand, operand2) => format!(
                    "\tMUL{}{}\t{},\t{},\t{}",
                    cond,
                    conv(s),
                    dest,
                    operand,
                    operand2
                ),
                Stat::MulA(cond, s, dest, operand, operand2, operand3) => format!(
                    "\tMULA{}{}\t{},\t{},\t{},\t{}",
                    cond,
                    conv(s),
                    dest,
                    operand,
                    operand2,
                    operand3
                ),
                Stat::MulOp(op, cond, s, hi, lo, operand, operand2) => format!(
                    "\t{}{}{}\t{},\t{},\t{},\t{}",
                    op,
                    cond,
                    conv(s),
                    hi,
                    lo,
                    operand,
                    operand2
                ),
                Stat::Move(op, cond, s, register, operand) =>
                    format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), register, operand),
                Stat::Cmp(op, cond, register, operand) =>
                    format!("\t{}{}\t{},\t{}", op, cond, register, operand),
                Stat::SatOp(op, cond, dest, operand, operand2) =>
                    format!("\t{}{}\t{},\t{},\t{}", op, cond, dest, operand, operand2),
                Stat::ReadCPSR(reg) => format!("\tmsr\t{},\tcpsr", reg),
                Stat::Branch(op, cond, label) => format!("\t{}{}\t{}", op, cond, label),
                Stat::MemOp(op, cond, s, register, operand) =>
                    format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), register, operand),
                Stat::Push(cond, reglist) => format!(
                    "\tpush{}\t{{{}}}",
                    cond,
                    reglist
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",")
                ),
                Stat::Pop(cond, reglist) => format!(
                    "\tpop{}\t{{{}}}",
                    cond,
                    reglist
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",")
                ),
                Stat::LiteralPool => "\t.ltorg".to_owned(),
                Stat::Global(label) => format!(".global\t{}", label),
                Stat::Label(label) => format!("{}:", label),
            }
        )
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "\t{}",
            match self {
                DataType::Ascii(s) => format!(".ascii \"{}\"", s),
                DataType::Word(w) => format!(".word {}", w),
                DataType::HalfWord(h) => format!(".hword {}", h),
                DataType::Byte(b) => format!(".byte {}", b),
            }
        )
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Data(ref_name, data_types) = self;
        write!(
            f,
            "\t{}:\n{}",
            ref_name,
            data_types
                .iter()
                .map(|d| format!("\t{}", d))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn this_is_not_a_test() {
        println!(
            "{}",
            Program(
                vec![],
                vec![
                    Stat::Global(String::from("main")),
                    Stat::Label(String::from("main")),
                    Stat::Push(Cond::Al, vec![Register::Lr]),
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        Register::R4,
                        MemOperand::Expression(-1)
                    ),
                    Stat::Move(
                        MovOp::Mov,
                        Cond::Al,
                        false,
                        Register::R0,
                        FlexOperand::ShiftReg(Register::R4, None)
                    ),
                    Stat::Branch(BranchOp::Bl, Cond::Al, String::from("exit")),
                    Stat::MemOp(
                        MemOp::Ldr,
                        Cond::Al,
                        false,
                        Register::R0,
                        MemOperand::Expression(0)
                    ),
                    Stat::Pop(Cond::Al, vec![Register::Pc]),
                    Stat::LiteralPool,
                ]
            )
        );
    }
}
