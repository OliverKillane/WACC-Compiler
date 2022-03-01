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

fn trans_expr(
    f: &mut String,
    expr: &Expr<Option<Type>, usize>,
    dest_reg: Register,
    mut registers: Vec<Register>,
    id_map: &HashMap<usize, usize>,
) {
    match expr {
        Expr::Null => todo!(),
        Expr::Int(i) => {
            writeln!(f, "\tLDR {}, ={}", dest_reg, i);
        }
        Expr::Bool(_) => todo!(),
        Expr::Char(_) => todo!(),
        Expr::String(_) => todo!(),
        Expr::Var(id) => {
            writeln!(f, "\tLDR {}, [sp, #{}]", dest_reg, 4 * id_map[id]);
        }
        Expr::ArrayElem(_, _) => todo!(),
        Expr::UnOp(_, _) => todo!(),
        Expr::BinOp(box ASTWrapper(_, e1), bin_op, box ASTWrapper(_, e2)) => {
            trans_expr(f, e1, dest_reg, registers.clone(), id_map);

            let e2_dest = registers.pop().unwrap();

            trans_expr(f, e2, e2_dest, registers.clone(), id_map);

            match bin_op {
                BinOp::Add => {
                    writeln!(f, "\tADDS {}, {}, {}", dest_reg, dest_reg, e2_dest);
                    writeln!(f, "\tBLVS p_throw_overflow_error");
                }
                BinOp::Sub => {
                    writeln!(f, "\tSUBS {}, {}, {}", dest_reg, dest_reg, e2_dest);
                    writeln!(f, "\tBLVS p_throw_overflow_error");
                }
                BinOp::Mul => {
                    writeln!(
                        f,
                        "\tSMULL {}, {}, {}, {}",
                        dest_reg, e2_dest, dest_reg, e2_dest
                    );
                    writeln!(f, "\tCMP {}, {}, ASR #31", e2_dest, dest_reg);
                    writeln!(f, "\tBLNE p_throw_overflow_error");
                }
                BinOp::Div => {
                    writeln!(f, "\tMOV R0, {}", dest_reg);
                    writeln!(f, "\tMOV R1, {}", e2_dest);
                    writeln!(f, "\tBL p_check_divide_by_zero");
                    writeln!(f, "\tBL __aeabi_idiv");
                    writeln!(f, "\tMOV {}, R0", dest_reg);
                }
                BinOp::Mod => {
                    writeln!(f, "\tMOV R0, {}", dest_reg);
                    writeln!(f, "\tMOV R1, {}", e2_dest);
                    writeln!(f, "\tBL p_check_divide_by_zero");
                    writeln!(f, "\tBL __aeabi_idivmod");
                    writeln!(f, "\tMOV {}, R1", dest_reg);
                }
                BinOp::Gt => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVGT {}, #1", dest_reg);
                    writeln!(f, "\tMOVLE {}, #0", dest_reg);
                },
                BinOp::Gte => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVGE {}, #1", dest_reg);
                    writeln!(f, "\tMOVLT {}, #0", dest_reg);
                },
                BinOp::Lt => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVLT {}, #1", dest_reg);
                    writeln!(f, "\tMOVGE {}, #0", dest_reg);
                },
                BinOp::Lte => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVLE {}, #1", dest_reg);
                    writeln!(f, "\tMOVGT {}, #0", dest_reg);
                },
                BinOp::Eq => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVEQ {}, #1", dest_reg);
                    writeln!(f, "\tMOVNE {}, #0", dest_reg);
                },
                BinOp::Ne => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVNE {}, #1", dest_reg);
                    writeln!(f, "\tMOVEQ {}, #0", dest_reg);
                },
                BinOp::And => {
                    writeln!(f, "\tCMP {}, {}", dest_reg, e2_dest);
                    writeln!(f, "\tMOVNE {}, #1", dest_reg);
                    writeln!(f, "\tMOVEQ {}, #0", dest_reg);
                },
                BinOp::Or => todo!(),
                BinOp::Newpair => todo!(),
            }
        }
    }
}

fn trans_rhs(
    f: &mut String,
    rhs: &AssignRhs<Option<Type>, usize>,
    dest_reg: Register,
    registers: Vec<Register>,
    id_map: &HashMap<usize, usize>,
) {
    match rhs {
        AssignRhs::Expr(ASTWrapper(_, expr)) => trans_expr(f, expr, dest_reg, registers, id_map),
        AssignRhs::Array(_) => todo!(),
        AssignRhs::Call(_, _) => todo!(),
    }
}

fn trans_stats(stats: &Vec<StatWrap<Option<Type>, usize>>, symbol_table: &VariableSymbolTable) -> String {
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
    let types: Vec<(usize, Type)> = symbol_table.0.into_iter().collect();
    let mut id_map = types.iter().map(|(id, typ)| (id, match typ {
        Type::Int => 4,
        Type::Bool => 1,
        Type::Char => 1,
        Type::String => 4,
        Type::Any => panic!("Any type should never show up after semantic analysis"),
        Type::Generic(_) => panic!("Generic type should never show up after semantic analysis"),
        Type::Pair(_, _) => todo!(),
        Type::Array(_, _) => todo!(),
    }));

    for ASTWrapper(_, stat) in stats {
        match stat {
            Stat::Skip => {}
            Stat::Def(t, id, rhs) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_rhs(&mut f, rhs, dest_reg, new_regs, &id_map);

                id_map.insert(*id, symbol_table.0.len() - id_map.len() - 1);

                writeln!(f, "\tSTR {}, [sp, #{}]", dest_reg, id_map[id]);
            }
            Stat::Assign(_, _) => todo!(),
            Stat::Read(_) => todo!(),
            Stat::Free(_) => todo!(),
            Stat::Return(_) => todo!(),
            Stat::Exit(ASTWrapper(_, expr)) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_expr(&mut f, expr, dest_reg, new_regs, &id_map);

                writeln!(f, "\tMOV R0, {}", dest_reg);
                writeln!(f, "\tBL exit");
            }
            Stat::Print(_) => todo!(),
            Stat::PrintLn(ASTWrapper(_, expr)) => {
                let mut new_regs = registers.clone();
                let dest_reg = new_regs.pop().unwrap();

                trans_expr(&mut f, expr, dest_reg, new_regs, &id_map);

                writeln!(f, "\tMOV R0, {}", dest_reg);
                writeln!(f, "\tBL p_print_int");
                writeln!(f, "\tBL p_println");
            }
            Stat::If(_, _, _) => todo!(),
            Stat::While(_, _) => todo!(),
            Stat::Block(_) => todo!(),
        }
    }
    format!(
        "\tSUB sp, sp, #{}
    {}\tADD sp, sp, #{}\n",
        id_map.len() * 4,
        f,
        id_map.len() * 4
    )
    // self.iter().map(|a| " ").collect::<Vec<_>>().join("\n")
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
        // writeln!(f, ".data")?;

        writeln!(f, ".text")?;
        writeln!(f, ".global main")?;
        writeln!(f, "main:")?;

        writeln!(f, "\tPUSH {{LR}}")?;

        write!(f, "{}", trans_stats(&self.body, &self.symbol_table))?;

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
            "../../frontend/tests/valid/expressions/greaterExpr.wacc"
        ))
        .unwrap();
        let (a, b, c) = analyse_semantics(
            ast,
            include_str!("../../frontend/tests/valid/basic/skip/skip.wacc"),
        )
        .unwrap();
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
