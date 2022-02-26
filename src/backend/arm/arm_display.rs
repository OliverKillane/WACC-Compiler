//! Converts [ARM Representation](Program) into a string format, suitable for
//! assembly.

use lazy_static::__Deref;

use super::arm_repr::*;
use std::{collections::HashMap, fmt::Display};

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

impl<T: Display> Display for FlexOperand<T> {
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

impl<T: Display> Display for MemOperand<T> {
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

impl<T: Display> Display for Stat<T> {
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
                Stat::Move(op, cond, s, ident, operand) =>
                    format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), ident, operand),
                Stat::Cmp(op, cond, ident, operand) =>
                    format!("\t{}{}\t{},\t{}", op, cond, ident, operand),
                Stat::SatOp(op, cond, dest, operand, operand2) =>
                    format!("\t{}{}\t{},\t{},\t{}", op, cond, dest, operand, operand2),
                Stat::ReadCPSR(reg) => format!("\tMSR\t{},\tCPSR", reg),
                Stat::MemOp(op, cond, s, ident, operand) =>
                    format!("\t{}{}{}\t{},\t{}", op, cond, conv(s), ident, operand),
                Stat::Push(cond, identlist) => format!(
                    "\tPUSH{}\t{{{}}}",
                    cond,
                    identlist
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",")
                ),
                Stat::Pop(cond, identlist) => format!(
                    "\tPOP{}\t{{{}}}",
                    cond,
                    identlist
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",")
                ),
            }
        )
    }
}

impl<T: Display> Display for ArmNode<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Used to store label numbers based on the nodes connected, and a
        // boolean describing if their code has been generated yet.
        let mut label_map: HashMap<ArmNode<T>, (usize, bool)> = HashMap::new();

        // Label identifier conversion tpo strings.
        let label_conv = |id: &usize| format!("b_label_{}", id);

        let mut next_node = Some(self.clone());

        loop {
            while let Some(current_node) = next_node {
                label_map
                    .entry(current_node.clone())
                    .and_modify(|label_passed| label_passed.1 = true);

                next_node = match current_node.get().deref() {
                    ControlFlow::Start(next) => Some(next.clone()),
                    ControlFlow::Simple(_, stat, next) => {
                        writeln!(f, "{}", stat)?;
                        Some(next.clone())
                    }
                    ControlFlow::Branching(_, branch_to, Some((cond, next))) => {
                        // For a conditional branch, branch with the condition, and continue to the (condition failed) next statements.

                        match label_map.get(branch_to) {
                            Some((id, _)) => writeln!(f, "\t B{}\t{}", cond, label_conv(id))?,
                            None => {
                                let id = label_map.len();
                                label_map.insert(branch_to.clone(), (id, false));
                                writeln!(f, "\tB{}\t{}", cond, label_conv(&id))?
                            }
                        }

                        Some(next.clone())
                    }
                    ControlFlow::Branching(_, branch_to, None) => {
                        match label_map.get(branch_to) {
                            Some((id, _)) => {
                                // IF the label already exists, branch to it
                                writeln!(f, "\tB\t{}", label_conv(id))?;
                            }
                            None => {
                                let id = label_map.len();
                                writeln!(f, "\tB\t{}", label_conv(&id))?;
                                label_map.insert(branch_to.clone(), (id, false));
                            }
                        };
                        None
                    }
                    ControlFlow::Call(_, fun_name, None) => {
                        writeln!(f, "\tBL\t{}", fun_name)?;
                        None
                    }
                    ControlFlow::Call(_, fun_name, Some((cond, next))) => {
                        writeln!(f, "\tBL{}\t{}", cond, fun_name)?;
                        Some(next.clone())
                    }
                    ControlFlow::Multi(_, next) => {
                        let id = label_map.len();
                        let id = label_map
                            .entry(current_node.clone())
                            .and_modify(|data| {
                                data.1 = true;
                            })
                            .or_insert((id, true))
                            .0;

                        writeln!(f, "{}:", label_conv(&id))?;

                        Some(next.clone())
                    }
                    ControlFlow::End(_) => None,
                    ControlFlow::Removed => {
                        unreachable!("Removed should not be found when displaying ARM code")
                    }
                };
            }

            let next_node = label_map
                .iter()
                .find(|(node, (_, passed))| !passed)
                .map(|res| res.0);
        }
    }
}

impl Display for DataKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DataKind::Ascii(string) =>
                    format!("\t\t.word {}\n\t\t.ascii \"{}\"", string.len(), string),
            }
        )
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t{}:\n{}", self.0, self.1)
    }
}

impl<T: Display> Display for Program<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Program {
            data,
            main,
            functions,
            cfg,
        } = self;
        if !data.is_empty() {
            writeln!(f, ".data")?;
            for dataref in data {
                write!(f, "{}", dataref)?;
            }
        };

        writeln!(f, ".text\n.global main\nmain:\n{}", main)?;

        for (fun_name, Function { args, start_node }) in functions {
            write!(f, "{}:\n{}", fun_name, start_node)?;
        }

        writeln!(f)
    }
}
