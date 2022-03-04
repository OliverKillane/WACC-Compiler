mod arm_repr;
mod arm_repr_display;
mod int_constraints;
mod trans_expr;
mod trans_stats;

use crate::ast;
use crate::frontend::semantic::symbol_table::VariableSymbolTable;
use arm_repr::*;
use std::collections::HashMap;
use trans_stats::trans_stats;

fn trans_program(
    body: &Vec<ast::ASTWrapper<Option<ast::Type>, ast::Stat<Option<ast::Type>, usize>>>,
    symbol_table: &VariableSymbolTable,
    functions_symbol_tables: &HashMap<
        &str,
        (ast::FunWrap<Option<ast::Type>, usize>, VariableSymbolTable),
    >,
) -> Program {
    let mut string_literals = HashMap::<String, usize>::new();

    let mut label_counter = 0;

    // writeln!(f, ".text")?;
    // writeln!(f, ".global main")?;
    // writeln!(f, "main:")?;

    // writeln!(f, "\tPUSH {{LR}}")?;

    // write!(
    //     f,
    //     "{}",
    //     trans_stats(&self.body, &self.symbol_table, &mut string_literals).unwrap()
    // )?;

    // writeln!(f, "\tLDR R0, =0")?;
    // writeln!(f, "\tPOP {{PC}}")?;
    // writeln!(f, "\t.ltorg")

    let mut program_stats: Vec<Stat> = vec![];
    trans_stats(
        &mut program_stats,
        body,
        symbol_table,
        &mut string_literals,
        &mut label_counter,
    );

    let mut function_stats: Vec<Stat> = vec![];

    let mut data: Vec<Data> = vec![];
    for (string, name) in string_literals {
        data.push(Data(
            format!("msg_{}", name),
            vec![DataType::Word(string.len() as i32), DataType::Ascii(string)],
        ))
    }

    Program(data, program_stats)
}
