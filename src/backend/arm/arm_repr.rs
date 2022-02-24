///! A structure representing a subset of arm assembly, to be generated as the final stage of the compiler, and written to a file (formatted)

/// Condition suffixes to be used in conditionally executing instructions.
enum Cond {
    EQ,
    NE,
    HS,
    LO,
    MI,
    PL,
    VS,
    VC,
    HI,
    LS,
    GE,
    LT,
    GT,
    LE,
    AL,
}

/// All general purpose register accessible in user mode.
enum Register {
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
    SP,
    LR,
    PC,
}

enum Shift {
    /// Arithmetic Right Shift, copies the sign bit for two complement to fill 
    /// shifted in region. Must shift by `1 <= n <= 32`
    ASR,
    /// Logical Left Shift, shift bits left. Must shift by `0 <= n <= 31`
    LSL,
    /// Logical shift right, shift bits right. Must shift `1 <= n <= 32`
    LSR,
    /// Rotate right shift. Must shift `1 <= n <= 31`
    ROR,
}

enum FlexOperand{ 
    /// Use an immediate operand, must be a shifted 8-bit pattern
    Imm(u32),
    /// Shift register to get operand value
    /// 
    /// Note:
    /// - Register cannot be `R15`/`PC` 
    ShiftReg(Register, Shift, u8),
}

/// Regular operations, two operands and a destination.
enum RegOp {
    /// Arithmetic addition.
    ADD,
    /// Arithmetic subtraction.
    SUB,
    /// Reverse Subtract .
    /// ```text
    /// SUB Rd, Rn, Op2: Rd = Op2 - Rn
    /// ```
    RSB,
    /// Add with carry (addition include the carry flag)
    ADC,
    /// Subtract with carry.
    SBC,
    /// Reverse Subtract with carry.
    RSC,
    /// Bitwise and.
    AND,
    /// Bitwise or.
    ORR,
    /// Bitwise exclusive or (XOR).
    EOR,
    /// Clears bits in the first, based on the second operand.
    /// ```text
    /// BIC Rd, Rn, Operand2: Rd = Rn & ~Operand2
    /// ```
    BIC
}

/// Move a flexible operand to a register.
enum MovOp {
    /// Move value from one register to another.
    MOV,
    /// Move and bitwise negation.
    MVN,
}

/// Compare a register and flexible operand. 
enum CmpOp {
    /// Sets conditions bits as set by SUBS.
    CMP,
    /// Sets conditions bits as set by ADDS.
    CMN,
    /// Sets condition bits based on bitwise and
    TST,
    /// Sets condition bits based on exclusive or.
    TEQ,
}

/// Sets but does not clear the Q flag for overflows, sets accordingly.
enum SatOp {
    /// Arithmetic addition.
    QADD,
    /// Arithmetic subtraction.
    QSUB,
    /// Double second argument, then add (if either saturates, set flag).
    QDADD,
    /// Double second argument, then subtract (if either saturates, set flag).
    QDSUB,
}

enum BranchOp {
    /// Branch (go to a new label).
    B,
    /// Branch and link (set the R14/LR to the next instruction).
    BL,
}

enum MulOp {
    /// Unsigned multiplication.
    UMULL,
    /// Unsigned multlipication, but the result it added to what is contained 
    /// in the Hi and Lo result registers.
    UMLAL,
    /// Signed multiplication.
    SMULL,
    /// Signed multlipication that adds to existing values in Hi and Lo result 
    /// registers.
    SMLAL,
}

enum Stat {
    /// A normal register operation of form:
    /// ```text
    /// (Operation, Condition, Set Condition Bits?, Register Destination, Register Operand, Flexible Second Operand)
    /// ```
    /// 
    /// Note:
    /// - `R15`/`PC` should not be used with the S suffix (set condition bits), as
    ///  this is undefined behaviour, and the assembler will not warn.
    /// - 
    ApplyOp(RegOp, Cond, bool, Register, Register, FlexOperand),

    /// Basic multiplication taking only the 32 least significant bits
    MUL(Cond, bool, Register, Register, Register),
    /// Basic Multiplication with addition added. Takes 32 least significant 
    /// bits.
    MULA(Cond, bool, Register, Register, Register, Register),

    /// A multiply operation using 4 register.
    /// ```text
    /// (Multiply Operation, Condition, Set Condition Bits?, Result Hight, Result Low, Register Operand, Second Register Operand)
    /// ```
    MulOp(MulOp, Cond, bool, Register, Register, Register, Register),

    /// A move instruction from register to register of form:
    /// ```text
    /// (Move Operation, Condition, Set Condition Bits?, Register Destination, Flexible Second Operand)
    /// ```
    Move(MovOp, Cond, bool, Register, FlexOperand),

    /// A comparison instruction to set the condition bits.
    /// ```text
    /// (Comparison Operation, Condition, Register Operand, Flexible Second Operand)
    /// ```
    Cmp(CmpOp, Cond, Register, FlexOperand),

    /// A saturating operation, checks for overflows/underflows.
    /// ```text
    /// (Saturating operation, Register Desination, Register Operand, Second Register Operand)
    /// ```
    SatOp(SatOp, Register, Register, Register),

    /// Read the CSPR register into a general purpose register.
    /// ```text
    /// Flag:  N  |Z  |C  |V  |Q
    ///  Bit: 31 |30 |29 |28 |27
    /// ```
    ReadPSR(Register),

    /// Branch to a label.
    /// ```text
    /// (Type of branch: B/BL, Condition, Label to branch to)
    /// ```
    Branch(BranchOp, Cond, String),

    /// A literal pool assembler directive, allowing the compiler to place parts 
    /// of the data section in the pool so they are in range of the instructions
    /// that use them.
    LiteralPool,
}

