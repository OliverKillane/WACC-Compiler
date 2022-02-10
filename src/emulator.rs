use atoi::FromRadix10SignedChecked;

use std::collections::HashMap;

use crate::intermediate::*;

enum Var {
    DWord(i32),
    Word(i16),
    Byte(i8),
    Bool(bool),
    Ptr(DataRef),
}

enum ExitStatus {
    Code(u32),
    Fault(String),
}

struct ProgramState {
    vars: HashMap<VarRepr, Var>,
    memory: HashMap<DataRef, Var>,
    stdin_data: Vec<u8>,
    stdin_ptr: usize,
    stdout: String,
}

fn eval_ptrexpr(expr: &PtrExpr, state: &mut ProgramState) -> Result<DataRef, ExitStatus> {
    todo!()
}

fn eval_boolexpr(expr: &BoolExpr, state: &mut ProgramState) -> Result<bool, ExitStatus> {
    todo!()
}

fn eval_numexpr(expr: &NumExpr, state: &mut ProgramState) -> Result<u32, ExitStatus> {
    todo!()
}

fn eval_expr(expr: &Expr, state: &mut ProgramState) -> Result<Var, ExitStatus> {
    todo!()
}

fn set_variable(var: &mut Var, val: Var) {
    match (val, var) {
        (Var::DWord(val), Var::DWord(ref mut var)) => *var = val,
        (Var::Word(val), Var::Word(ref mut var)) => *var = val,
        (Var::Byte(val), Var::Byte(ref mut var)) => *var = val,
        (Var::Bool(val), Var::Bool(ref mut var)) => *var = val,
        (Var::Ptr(val), Var::Ptr(ref mut var)) => *var = val,
        _ => panic!("Mismatched variable type"),
    }
}

fn deref_memory<'l>(
    ptr_expr: &PtrExpr,
    state: &'l mut ProgramState,
) -> Result<&'l mut Var, ExitStatus> {
    let mem_location = eval_ptrexpr(ptr_expr, state)?;
    state
        .memory
        .get_mut(&mem_location)
        .ok_or(ExitStatus::Fault("Segmentation fault".to_string()))
}

fn get_var<'l>(var: &VarRepr, vars: &'l mut HashMap<VarRepr, Var>) -> &'l mut Var {
    vars.get_mut(var)
        .expect(&format!("Variable {} not found", var))
}

fn run_statement(stat: &Stat, state: &mut ProgramState) -> Result<(), ExitStatus> {
    match stat {
        Stat::AssignVar(ref var, ref expr) => {
            let expr_val = eval_expr(expr, state)?;
            set_variable(get_var(var, &mut state.vars), expr_val);
        }
        Stat::AssignPtr(ref ptr_expr, ref expr) => {
            let expr_val = eval_expr(expr, state)?;
            set_variable(deref_memory(ptr_expr, state)?, expr_val);
        }
        Stat::ReadIntVar(ref var) => {
            let (read_val, bytes_read) =
                i32::from_radix_10_signed_checked(&state.stdin_data[state.stdin_ptr..]);
            state.stdin_ptr += bytes_read;
            if let Some(read_val) = read_val && bytes_read > 0 {
                set_variable(get_var(var, &mut state.vars), Var::DWord(read_val));
            }
        }
        Stat::ReadIntPtr(ref ptr_expr) => {
            let (read_val, bytes_read) =
                i32::from_radix_10_signed_checked(&state.stdin_data[state.stdin_ptr..]);
            state.stdin_ptr += bytes_read;
            if let Some(read_val) = read_val && bytes_read > 0 {
                set_variable(deref_memory(ptr_expr, state)?, Var::DWord(read_val));
            }
        }
        Stat::ReadCharVar(ref var) => {
            if state.stdin_ptr < state.stdin_data.len() {
                let c = state.stdin_data[state.stdin_ptr] as i8;
                state.stdin_ptr += 1;
                set_variable(get_var(var, &mut state.vars), Var::Byte(c));
            }
        }
        Stat::ReadCharPtr(ref ptr_expr) => {
            if state.stdin_ptr < state.stdin_data.len() {
                let c = state.stdin_data[state.stdin_ptr] as i8;
                state.stdin_ptr += 1;
                set_variable(deref_memory(ptr_expr, state)?, Var::Byte(c));
            }
        }
        Stat::Free(ref ptr_expr, ref len_expr) => {}
        _ => todo!(),
    };
    Ok(())
}

#[test]
fn test() {
    println!(
        "{:?}",
        i32::from_radix_10_signed_checked("123abcdef".as_bytes())
    );
}

fn run_graph(graph: &BlockGraph, state: &mut ProgramState) -> Result<Var, ExitStatus> {
    let mut curr_block_num = 0;
    loop {
        let Block(_, ref stats, ref ending) = graph[curr_block_num];
        for stat in stats {
            run_statement(stat, state)?;
        }
        match ending {
            BlockEnding::CondJumps(ref conds, ref else_block_num) => {
                let mut jumped = false;
                for (cond, jump_block_num) in conds {
                    if eval_boolexpr(cond, state)? {
                        curr_block_num = *jump_block_num;
                        jumped = true;
                        break;
                    }
                }
                if !jumped {
                    curr_block_num = *else_block_num;
                }
            }
            BlockEnding::Exit(expr) => {
                return Err(ExitStatus::Code(eval_numexpr(expr, state)?));
            }
            BlockEnding::Return(expr) => return eval_expr(expr, state),
        }
    }
}

fn run(program: &Program) {
    todo!()
}
