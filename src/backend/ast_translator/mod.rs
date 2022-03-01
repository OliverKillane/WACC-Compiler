use crate::frontend::{ast::*, semantic::symbol_table::*};
use std::fmt::Write;
use std::{collections::HashMap, fmt::Display};

#[derive(Copy, Clone)]
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
                Register::SP => "SP",
                Register::LR => "LR",
                Register::PC => "PC",
            }
        )
    }
}

struct AnalProgram<'a> {
    body: Vec<StatWrap<Option<Type>, usize>>,
    symbol_table: VariableSymbolTable,
    functions: HashMap<&'a str, (FunWrap<Option<Type>, usize>, VariableSymbolTable)>,
}

fn get_type_size(t: &Type) -> usize {
    match t {
        Type::Int => 4,
        Type::Bool => 1,
        Type::Char => 1,
        Type::String => 4,
        Type::Any => panic!("Any type should never show up after semantic analysis"),
        Type::Generic(_) => panic!("Generic type should never show up after semantic analysis"),
        Type::Pair(_, _) => 4,
        Type::Array(_, _) => 4,
    }
}

fn get_type_suffix(t: &Type) -> &'static str {
    match t {
        Type::Int => "",
        Type::Bool => "B",
        Type::Char => "B",
        Type::String => "",
        Type::Any => panic!("Any type should never show up after semantic analysis"),
        Type::Generic(_) => panic!("Generic type should never show up after semantic analysis"),
        Type::Pair(_, _) => "",
        Type::Array(_, _) => "",
    }
}

fn trans_expr(
    f: &mut String,
    expr: &Expr<Option<Type>, usize>,
    dest_reg: Register,
    mut registers: Vec<Register>,
    symbol_table: &VariableSymbolTable,
    id_map: &HashMap<usize, usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    match expr {
        Expr::Null => {
            writeln!(f, "\tLDR {}, ={}", dest_reg, 0)?;
        }
        Expr::Int(i) => {
            writeln!(f, "\tLDR {}, ={}", dest_reg, i)?;
        }
        Expr::Bool(b) => {
            writeln!(f, "\tMOV {}, #{}", dest_reg, if *b { 1 } else { 0 })?;
        }
        Expr::Char(c) => {
            writeln!(f, "\tMOV {}, #'{}'", dest_reg, c)?;
        }
        Expr::String(_) => {}
        Expr::Var(id) => {
            writeln!(
                f,
                "\tLDR{} {}, [sp, #{}]",
                get_type_suffix(&symbol_table.0[id]),
                dest_reg,
                id_map[id]
            )?;
        }
        Expr::ArrayElem(_, _) => todo!(),
        Expr::UnOp(un_op, box ASTWrapper(t, e)) => {
            trans_expr(f, e, dest_reg, registers.clone(), symbol_table, id_map)?;

            match un_op {
                UnOp::Minus => {
                    writeln!(f, "\tRSBS {}, {}, #0", dest_reg, dest_reg)?;
                    writeln!(f, "\tBLVS p_throw_overflow_error")?;
                }
                UnOp::Neg => {
                    writeln!(f, "\tEOR {}, {}, #1", dest_reg, dest_reg)?;
                }
                UnOp::Len => todo!(),
                UnOp::Ord => {}
                UnOp::Chr => {}
                UnOp::Fst => {
                    writeln!(f, "\tMOV R0, {}", dest_reg)?;
                    writeln!(f, "\tBL p_check_null_pointer")?;
                    writeln!(f, "\tLDR {}, [{}]", dest_reg, dest_reg)?;
                    writeln!(
                        f,
                        "\tLDR{} {}, [{}]",
                        get_type_suffix(match &t.clone().unwrap() {
                            Type::Pair(fst, _) => fst,
                            _ => panic!("Fst expression should only have type pair"),
                        }),
                        dest_reg,
                        dest_reg
                    )?;
                }
                UnOp::Snd => {
                    writeln!(f, "\tMOV R0, {}", dest_reg)?;
                    writeln!(f, "\tBL p_check_null_pointer")?;
                    writeln!(f, "\tLDR {}, [{}, #4]", dest_reg, dest_reg)?;
                    writeln!(
                        f,
                        "\tLDR{} {}, [{}]",
                        get_type_suffix(match &t.clone().unwrap() {
                            Type::Pair(_, snd) => snd,
                            _ => panic!("Fst expression should only have type pair"),
                        }),
                        dest_reg,
                        dest_reg
                    )?;
                }
            }
        }
        Expr::BinOp(box ASTWrapper(t1, e1), BinOp::Newpair, box ASTWrapper(t2, e2)) => {
            writeln!(f, "\tLDR R0, =8")?;
            writeln!(f, "\tBL malloc")?;
            writeln!(f, "\tMOV {}, R0", dest_reg)?;

            let sub_expr_dest = registers.pop().unwrap();

            // translate e1
            trans_expr(
                f,
                e1,
                sub_expr_dest,
                registers.clone(),
                symbol_table,
                id_map,
            )?;

            // allocate space for e1
            writeln!(f, "\tLDR R0, ={}", get_type_size(&t1.clone().unwrap()))?;
            writeln!(f, "\tBL malloc")?;

            // store e1 into its allocated space
            writeln!(
                f,
                "\tSTR{} {}, [R0]",
                get_type_suffix(&t1.clone().unwrap()),
                sub_expr_dest
            )?;
            writeln!(f, "\tSTR R0, [{}]", dest_reg)?;

            // translate e2
            trans_expr(
                f,
                e2,
                sub_expr_dest,
                registers.clone(),
                symbol_table,
                id_map,
            )?;

            // allocate space for e2
            writeln!(f, "\tLDR R0, ={}", get_type_size(&t2.clone().unwrap()))?;
            writeln!(f, "\tBL malloc")?;

            // store e3 into its allocated space
            writeln!(
                f,
                "\tSTR{} {}, [R0]",
                get_type_suffix(&t2.clone().unwrap()),
                sub_expr_dest
            )?;
            writeln!(f, "\tSTR R0, [{}, #4]", dest_reg)?;
        }
        Expr::BinOp(box ASTWrapper(_, e1), bin_op, box ASTWrapper(_, e2)) => {
            trans_expr(f, e1, dest_reg, registers.clone(), symbol_table, id_map)?;

            let e2_dest = registers.pop().unwrap();

            trans_expr(f, e2, e2_dest, registers.clone(), symbol_table, id_map)?;

            match bin_op {
                BinOp::Add => {
                    writeln!(f, "\tADDS {}, {}, {}", dest_reg, dest_reg, e2_dest)?;
                    writeln!(f, "\tBLVS p_throw_overflow_error")?;
                }
                BinOp::Sub => {
                    writeln!(f, "\tSUBS {}, {}, {}", dest_reg, dest_reg, e2_dest)?;
                    writeln!(f, "\tBLVS p_throw_overflow_error")?;
                }
                BinOp::Mul => {
                    writeln!(
                        f,
                        "\tSMULL {}, {}, {}, {}",
                        dest_reg, e2_dest, dest_reg, e2_dest
                    )?;
                    writeln!(f, "\tCMP {}, {}, ASR #31", e2_dest, dest_reg)?;
                    writeln!(f, "\tBLNE p_throw_overflow_error")?;
                }
                BinOp::Div => {
                    writeln!(f, "\tMOV R0, {}", dest_reg)?;
                    writeln!(f, "\tMOV R1, {}", e2_dest)?;
                    writeln!(f, "\tBL p_check_divide_by_zero")?;
                    writeln!(f, "\tBL __aeabi_idiv")?;
                    writeln!(f, "\tMOV {}, R0", dest_reg)?;
                }
                BinOp::Mod => {
                    writeln!(f, "\tMOV R0, {}", dest_reg)?;
                    writeln!(f, "\tMOV R1, {}", e2_dest)?;
                    writeln!(f, "\tBL p_check_divide_by_zero")?;
                    writeln!(f, "\tBL __aeabi_idivmod")?;
                    writeln!(f, "\tMOV {}, R1", dest_reg)?;
                }
                BinOp::Gt => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVGT {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVLE {}, #0", dest_reg)?;
                }
                BinOp::Gte => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVGE {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVLT {}, #0", dest_reg)?;
                }
                BinOp::Lt => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVLT {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVGE {}, #0", dest_reg)?;
                }
                BinOp::Lte => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVLE {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVGT {}, #0", dest_reg)?;
                }
                BinOp::Eq => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVEQ {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVNE {}, #0", dest_reg)?;
                }
                BinOp::Ne => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVNE {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVEQ {}, #0", dest_reg)?;
                }
                BinOp::And => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest)?;
                    writeln!(f, "\tMOVNE {}, #1", dest_reg)?;
                    writeln!(f, "\tMOVEQ {}, #0", dest_reg)?;
                }
                BinOp::Or => {
                    writeln!(f, "\tORR {}, {}, {}", dest_reg, dest_reg, e2_dest)?;
                }
                BinOp::Newpair => {}
            }
        }
    };

    Ok(())
}

fn trans_rhs(
    f: &mut String,
    rhs: &AssignRhs<Option<Type>, usize>,
    dest_reg: Register,
    registers: Vec<Register>,
    symbol_table: &VariableSymbolTable,
    id_map: &HashMap<usize, usize>,
) -> Result<(), Box<dyn std::error::Error>> {
    match rhs {
        AssignRhs::Expr(ASTWrapper(_, expr)) => {
            trans_expr(f, expr, dest_reg, registers, symbol_table, id_map)?;
        }
        AssignRhs::Array(_) => todo!(),
        AssignRhs::Call(_, _) => todo!(),
    };
    Ok(())
}

fn trans_stats(
    stats: &Vec<StatWrap<Option<Type>, usize>>,
    symbol_table: &VariableSymbolTable,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut f = String::new();
    let registers = vec![
        Register::R12,
        Register::R11,
        Register::R10,
        Register::R9,
        Register::R8,
        Register::R7,
        Register::R6,
        Register::R5,
        Register::R4,
    ];
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

    dbg!(&id_space, used_stack_space);

    let id_map = id_space.into_iter().collect();

    writeln!(f, "\tSUB sp, sp, #{}", used_stack_space)?;

    for ASTWrapper(_, stat) in stats {
        match stat {
            Stat::Skip => {}
            Stat::Def(t, id, rhs) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_rhs(&mut f, rhs, dest_reg, new_regs, symbol_table, &id_map)?;

                writeln!(
                    f,
                    "\tSTR{} {}, [sp, #{}]",
                    get_type_suffix(t),
                    dest_reg,
                    id_map[id]
                )?;
            }
            Stat::Assign(lhs, rhs) => {
                let mut new_regs = registers.clone();
                let expression_reg = new_regs.pop().unwrap();

                trans_rhs(&mut f, rhs, expression_reg, new_regs.clone(), symbol_table, &id_map)?;

                match lhs {
                    AssignLhs::Var(id) => {
                        writeln!(
                            f,
                            "\tSTR{} {}, [sp, #{}]",
                            get_type_suffix(&symbol_table.0[id]),
                            expression_reg,
                            id_map[id]
                        )?;
                    }
                    AssignLhs::ArrayElem(_, _) => todo!(),
                    AssignLhs::PairFst(ASTWrapper(
                        Some(Type::Pair(box lt, _)),
                        Expr::Var(id),
                    )) => {
                        let pair_reg = new_regs.pop().unwrap();
                        // load the pair ptr into a register.
                        writeln!(f, "\tLDR {}, [sp, #{}]", pair_reg, id_map[id])?;

                        // check if pair ptr is not null.
                        writeln!(f, "\tMOV R0, {}", pair_reg)?;
                        writeln!(f, "\tBL p_check_null_pointer")?;

                        // deref the first ptr of pair.
                        writeln!(f, "\tLDR {}, [{}]", pair_reg, pair_reg)?;

                        // store the expression into the first ptr of pair
                        writeln!(f, "\tSTR{} {}, [{}]", get_type_suffix(lt), expression_reg, pair_reg)?;
                    }
                    AssignLhs::PairSnd(ASTWrapper(
                        Some(Type::Pair(box lt, box rt)),
                        Expr::Var(id),
                    )) => {
                        let pair_reg = new_regs.pop().unwrap();
                        // load the pair ptr into a register.
                        writeln!(f, "\tLDR {}, [sp, #{}]", pair_reg, id_map[id])?;

                        // check if pair ptr is not null.
                        writeln!(f, "\tMOV R0, {}", pair_reg)?;
                        writeln!(f, "\tBL p_check_null_pointer")?;

                        // deref the second ptr of pair.
                        writeln!(f, "\tLDR {}, [{}, #{}]", pair_reg, pair_reg, get_type_size(lt))?;

                        // store the expression into the second ptr of pair
                        writeln!(f, "\tSTR{} {}, [{}]", get_type_suffix(rt), expression_reg, pair_reg)?;
                    }
                    _ => panic!("Illegal rhs found: {:?}", lhs),
                }
            }
            Stat::Read(_) => todo!(),
            Stat::Free(_) => todo!(),
            Stat::Return(_) => todo!(),
            Stat::Exit(ASTWrapper(_, expr)) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_expr(&mut f, expr, dest_reg, new_regs, symbol_table, &id_map)?;

                writeln!(f, "\tMOV R0, {}", dest_reg)?;
                writeln!(f, "\tBL exit")?;
            }
            Stat::Print(ASTWrapper(t, expr)) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_expr(&mut f, expr, dest_reg, new_regs, symbol_table, &id_map)?;

                writeln!(f, "\tMOV R0, {}", dest_reg)?;
                writeln!(
                    f,
                    "\tBL p_print_{}",
                    match t.clone().unwrap() {
                        Type::Int => "int",
                        Type::Bool => "bool",
                        Type::Char => "char",
                        Type::String => "string",
                        Type::Any => todo!(),
                        Type::Generic(_) => todo!(),
                        Type::Pair(_, _) => "reference",
                        Type::Array(_, _) => "reference",
                    }
                )?;
            }
            Stat::PrintLn(ASTWrapper(t, expr)) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_expr(&mut f, expr, dest_reg, new_regs, symbol_table, &id_map)?;

                writeln!(f, "\tMOV R0, {}", dest_reg)?;
                writeln!(
                    f,
                    "\tBL p_print_{}",
                    match t.clone().unwrap() {
                        Type::Int => "int",
                        Type::Bool => "bool",
                        Type::Char => "char",
                        Type::String => "string",
                        Type::Any => todo!(),
                        Type::Generic(_) => todo!(),
                        Type::Pair(_, _) => "reference",
                        Type::Array(_, _) => "reference",
                    }
                )?;
                writeln!(f, "\tBL p_println")?;
            }
            Stat::If(_, _, _) => todo!(),
            Stat::While(_, _) => todo!(),
            Stat::Block(_) => todo!(),
        }
    }

    writeln!(f, "\tADD sp, sp, #{}", used_stack_space)?;
    Ok(f)
}

impl Display for Expr<Option<Type>, usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for AssignLhs<Option<Type>, usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Display for AssignRhs<Option<Type>, usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a> Display for AnalProgram<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".text")?;
        writeln!(f, ".global main")?;
        writeln!(f, "main:")?;

        writeln!(f, "\tPUSH {{LR}}")?;

        write!(
            f,
            "{}",
            trans_stats(&self.body, &self.symbol_table).unwrap()
        )?;

        writeln!(f, "\tLDR R0, =0")?;
        writeln!(f, "\tPOP {{PC}}")?;
        writeln!(f, "\t.ltorg")
    }
}

#[cfg(test)]

mod tests {

    use crate::frontend::{parser::parse, semantic::analyse_semantics};

    use super::*;

    #[test]
    fn test_skip() {
        let ast = parse(include_str!(
            "../../frontend/tests/valid/pairs/writeSnd.wacc"
        ))
        .unwrap();
        let (a, b, c) = analyse_semantics(ast, "").unwrap();
        println!(
            "{}",
            AnalProgram {
                body: a,
                symbol_table: b,
                functions: c
            }
        )
    }
}
