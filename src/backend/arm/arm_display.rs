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
                Shift::Rxx => "RXX".into(),
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
                FlexOperand::Imm(n) => n.to_string(),
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

impl Display for BranchOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BranchOp::B => "B",
                BranchOp::Bl => "BL",
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
        let conv = |s: &bool| if *s {"S".to_owned()} else {String::from("")};
        write!(f, "{}", match self {
            Stat::ApplyOp(op, cond, s, dest, operand, operand2) => format!("\t{}{}{}\t{},\t{},\t{}", op, cond, conv(s), dest, operand, operand2),
            Stat::Mul(cond, s, dest, operand, operand2) => format!("\tMUL{}{}\t{},\t{},\t{}", cond, conv(s), dest, operand, operand2),
            Stat::MulA(cond, s, dest, operand, operand2, operand3) => format!("\tMULA{}{}\t{},\t{},\t{},\t{}", cond, conv(s), dest, operand, operand2, operand3),
            Stat::MulOp(op, cond, s, hi, lo, operand, operand2) => format!("\t{}{}{}\t{},\t{},\t{},\t{}", op, cond, conv(s), hi, lo, operand, operand2),
            Stat::Move(op, cond, s, register, operand) => format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), register, operand),
            Stat::Cmp(op, cond, register, operand) => format!("\t{}{}\t{},\t{}", op, cond, register, operand),
            Stat::SatOp(op, cond, dest, operand, operand2) => format!("\t{}{}\t{},\t{},\t{}", op, cond, dest, operand, operand2),
            Stat::ReadCPSR(reg) => format!("\tMSR\t{},\tCPSR", reg),
            Stat::Branch(op, cond, label) => format!("\t{}{}\t{}", op, cond, label),
            Stat::MemOp(op, cond, s, register, operand) => format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), register, operand),
            Stat::Push(cond, reglist) => format!("\tPUSH{}\t{{{}}}", cond, reglist.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(",")),
            Stat::Pop(cond, reglist) => format!("\tPOP{}\t{{{}}}", cond, reglist.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join(",")),
            Stat::LiteralPool => "\t.ltorg".to_owned(),
            Stat::Global(label) => format!(".global\t{}", label),
            Stat::Label(label) => format!("{}:", label),
        })
    }
}

impl Display for DataKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            DataKind::Ascii(string) => format!("\t\t.word {}\n\t\t.ascii \"{}\"", string.len(), string),
        })
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t{}:\n{}", self.0, self.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn this_is_not_a_test() {
        println!("{}", Program(vec![], vec![
            Stat::Global(String::from("main")),
            Stat::Label(String::from("main")),
            Stat::Push(Cond::Al, vec![Register::Lr]),
            Stat::MemOp(MemOp::Ldr, Cond::Al, false, Register::R4, MemOperand::Expression(-1)),
            Stat::Move(MovOp::Mov, Cond::Al, false, Register::R0, FlexOperand::ShiftReg(Register::R4, None)),
            Stat::Branch(BranchOp::Bl, Cond::Al, String::from("exit")),
            Stat::MemOp(MemOp::Ldr, Cond::Al, false, Register::R0, MemOperand::Expression(0)),
            Stat::Pop(Cond::Al, vec![Register::Pc]),
            Stat::LiteralPool,
        ]));
    }
}