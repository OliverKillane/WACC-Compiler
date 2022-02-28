//! A structure representing a subset of arm assembly, to be generated as the
//! final stage of the compiler, and written to a file (formatted)
use super::{
    super::super::graph::{Deleted, Graph, NodeRef},
    int_constraints::ConstrainedInt,
};
use std::collections::HashMap;

/// The temporary type (used before register allocation)
pub type Temporary = u32;
pub type DataIdent = i64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ident {
    Temp(Temporary),
    Register(Register),
}

/// All general purpose register accessible in user mode (registers are
/// allocated in a second pass).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    Sp,
    Lr,
    Pc,
}

/// Condition suffixes to be used in conditionally executing instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cond {
    Eq,
    Ne,
    Hs,
    Lo,
    Mi,
    Pl,
    Vs,
    Vc,
    Hi,
    Ls,
    Ge,
    Lt,
    Gt,
    Le,
    Al,
}

/// The shifts that can be used for operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Shift {
    /// Arithmetic Right Shift, copies the sign bit for two complement to fill
    /// shifted in region. Must shift by `1 <= n <= 32`
    Asr(ConstrainedInt<1, 32>),
    /// Logical Left Shift, shift bits left. Must shift by `0 <= n <= 31`
    Lsl(ConstrainedInt<0, 31>),
    /// Logical shift right, shift bits right. Must shift `1 <= n <= 32`
    Lsr(ConstrainedInt<1, 32>),
    /// Rotate right shift. Must shift `1 <= n <= 31`
    Ror(ConstrainedInt<1, 31>),
    /// Rotate right by one bit, with extension.
    Rxx,
}

/// The allowed flexible second operands for arm.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlexOperand {
    /// Use an immediate operand, must be a shifted 8-bit pattern (shifted by even number of bits)
    Imm(u32),
    /// Shift register to get operand value
    ///
    /// Note:
    /// - Register cannot be `R15`/`PC`
    ShiftReg(Ident, Option<Shift>),
}

/// Regular operations, two operands and a destination.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegOp {
    /// Arithmetic addition.
    Add,
    /// Arithmetic subtraction.
    Sub,
    /// Reverse Subtract .
    /// ```text
    /// SUB Rd, Rn, Op2: Rd = Op2 - Rn
    /// ```
    Rsb,
    /// Add with carry (addition include the carry flag)
    Adc,
    /// Subtract with carry.
    Sbc,
    /// Reverse Subtract with carry.
    Rsc,
    /// Bitwise and.
    And,
    /// Bitwise or.
    Orr,
    /// Bitwise exclusive or (XOR).
    Eor,
    /// Clears bits in the first, based on the second operand.
    /// ```text
    /// BIC Rd, Rn, Operand2: Rd = Rn & ~Operand2
    /// ```
    Bic,
}

/// Move a flexible operand to a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MovOp {
    /// Move value from one register to another.
    Mov,
    /// Move and bitwise negation.
    Mvn,
}

/// Compare a register and flexible operand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    /// Sets conditions bits as set by SUBS.
    Cmp,
    /// Sets conditions bits as set by ADDS.
    Cmn,
    /// Sets condition bits based on bitwise and
    Tst,
    /// Sets condition bits based on exclusive or.
    Teq,
}

/// Sets but does not clear the Q flag for overflows, sets accordingly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SatOp {
    /// Arithmetic addition.
    Add,
    /// Arithmetic subtraction.
    Sub,
    /// Double second argument, then add (if either saturates, set flag).
    DAdd,
    /// Double second argument, then subtract (if either saturates, set flag).
    DSub,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MulOp {
    /// Unsigned multiplication.
    UMulL,
    /// Unsigned multiplication, but the result it added to what is contained
    /// in the Hi and Lo result registers.
    UMlAL,
    /// Signed multiplication.
    SMulL,
    /// Signed multiplication that adds to existing values in Hi and Lo result
    /// registers.
    SMlAL,
}

/// Memory operation type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemOp {
    /// Load-register instruction.
    Ldr,
    /// Store-register instruction
    Str,
}

/// Memory operand is the second operand of the [memory operand](MemOp).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemOperand {
    /// Indirect memory location stored in an identifier.
    Zero(Ident),
    /// Indirect memory location stored in an identifier with offset [FlexOffset].
    /// If [bool] is true then we update the value in the register with offset
    /// before accessing the memory location.
    PreIndex(Ident, FlexOffset, bool),
    /// Direct from the label in the data section indicated by [String].
    Label(DataIdent),
    /// Immediate 32-bit integer value.
    Expression(i32),
    /// Indirect memory location stored in the identifier, updated by
    /// [FlexOffset] after the memory access has taken place.
    PostIndex(Ident, FlexOffset),
}

/// Offset for LDR/STR instructions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlexOffset {
    /// Constant offset between -4095 and 4095 inclusive.
    Expr(ConstrainedInt<-4095, 4095>),
    /// [bool] represents if this is a negative offset. [Register] is the
    /// the value we are offsetting by, shifted by [Shift].
    ShiftReg(bool, Ident, Option<Shift>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stat {
    /// A normal register operation of form:
    /// ```text
    /// (Operation, Condition, Set Condition Bits?, Register Destination, Register Operand, Flexible Second Operand)
    /// ```
    ///
    /// Note:
    /// - `R15`/`PC` should not be used with the S suffix (set condition bits), as
    ///  this is undefined behaviour, and the assembler will not warn.
    ApplyOp(RegOp, Cond, bool, Ident, Ident, FlexOperand),

    /// Basic multiplication taking only the 32 least significant bits
    Mul(Cond, bool, Ident, Ident, Ident),

    /// Basic Multiplication with addition added. Takes 32 least significant
    /// bits.
    MulA(Cond, bool, Ident, Ident, Ident, Ident),

    /// A multiply operation using 4 register.
    /// ```text
    /// (Multiply Operation, Condition, Set Condition Bits?, Result Hight, Result Low, Register Operand, Second Register Operand)
    /// ```
    MulOp(MulOp, Cond, bool, Ident, Ident, Ident, Ident),

    /// A move instruction from register to register of form:
    /// ```text
    /// (Move Operation, Condition, Set Condition Bits?, Register Destination, Flexible Second Operand)
    /// ```
    Move(MovOp, Cond, bool, Ident, FlexOperand),

    /// A comparison instruction to set the condition bits.
    /// ```text
    /// (Comparison Operation, Condition, Register Operand, Flexible Second Operand)
    /// ```
    Cmp(CmpOp, Cond, Ident, FlexOperand),

    /// A saturating operation, checks for overflows/underflows.
    /// ```text
    /// (Saturating operation, Register Desination, Register Operand, Second Register Operand)
    /// ```
    SatOp(SatOp, Cond, Ident, Ident, Ident),

    /// Read the CSPR register into a general purpose register.
    /// ```text
    /// Flag:  N  |Z  |C  |V  |Q
    ///  Bit: 31 |30 |29 |28 |27
    /// ```
    ReadCPSR(Ident),

    /// Conditional branch to a label.
    /// ```text
    /// (Type of branch: B/BL, Condition, Label to branch to)
    /// ```

    /// Memory Operation (a load or store).
    MemOp(MemOp, Cond, bool, Ident, MemOperand),

    /// Push thumb instruction.
    Push(Cond, Vec<Ident>),

    /// Pop thumb instruction, R15/PC can only be used as the last register to
    /// pop (prevent jump before popping other registers).
    Pop(Cond, Vec<Ident>),

    /// Branch link to a string identifier (calls are converted into these)
    Link(Cond, String),

    /// A dummy node for calls, expanded by register allocation.
    Call(Cond, String, Option<Ident>, Vec<Ident>),
}

pub type ArmNode = NodeRef<ControlFlow>;

/// Structure of the control flow of the arm graph. Note that when a no more
/// statements are present, a return is presumed (setting the ).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlFlow {
    /// Simple Control Flow
    Simple(Option<ArmNode>, Stat, Option<ArmNode>),

    /// Branch to a label.
    /// ```text
    /// (Type of branch: B/BL, Condition, Label to branch to)
    /// ```
    Branch(Option<ArmNode>, ArmNode, Cond, Option<ArmNode>),

    /// Return statement, returning a temporary. This is a dummy node, which is
    /// replaced by the relevant register/stack management when allocating registers
    Return(Option<ArmNode>, Option<Ident>),

    /// A position that can be jumped to from multiple locations/can have multiple predecessors.
    Multi(Vec<ArmNode>, Option<ArmNode>),

    /// Used to remove references when dropping elements of the graph
    Removed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataKind {
    /// An ascii value
    Ascii(String),
}

/// All types of data that can be kept in the binary.
pub struct Data(pub DataIdent, pub DataKind);

pub struct Subroutine {
    pub args: Vec<Temporary>,
    pub start_node: ArmNode,
}

/// The main program containing text ([instructions](Stat)) and [data](Data).
pub struct Program {
    pub data: Vec<Data>,
    pub main: ArmNode,
    pub functions: HashMap<String, Subroutine>,
    pub cfg: Graph<ControlFlow>,
}

impl Deleted for ControlFlow {
    fn deleted() -> Self {
        Self::Removed
    }
}
