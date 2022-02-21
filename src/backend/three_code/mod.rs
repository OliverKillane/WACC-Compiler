mod expr;

use self::expr::{
    bool::{propagate_bool_const, translate_bool_expr},
    num::{propagate_num_const, translate_num_expr},
    ptr::{propagate_ptr_const, translate_ptr_expr},
    translate_expr, ExprTranslationData,
};
use super::{Options, PropagationOpt};
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use std::{cell::RefMut, collections::HashMap, slice::SliceIndex};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Type of the source operand for an operation
pub(super) enum OpSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable with a given [id](VarRepr)
    Var(VarRepr),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Size of the load/store operations
pub(super) enum Size {
    /// 1 byte
    Byte,
    /// 2 bytes
    Word,
    /// 4 bytes
    DWord,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// Binary operation code
pub(super) enum BinOp {
    /// Addition (+)
    Add,
    /// Subtraction (-)
    Sub,
    /// Signed multiplication (*)
    Mul,
    /// Division (/)
    Div,
    /// Modulo (%)
    Mod,

    /// Equality (==)
    Eq,
    /// Not equality (!=)
    Ne,
    /// Greater than (>)
    Gt,
    /// Greater than or equal (>=)
    Gte,
    /// Less than (<)
    Lt,
    /// Less than or equal (<=)
    Lte,

    /// Logical And (&&)
    And,
    /// Logical Or (||)
    Or,
    /// Logical Xor (^)
    Xor,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(super) enum PtrSrc {
    DataRef(DataRef, i32),
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// Statement type
pub(super) enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc),
    /// Load from a reference to static data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadImm(VarRepr, PtrSrc, Size),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    LoadVar(VarRepr, VarRepr, Size),
    /// Store to a static data reference. The number of bytes stored is signified
    /// by the [size](Size) field.
    StoreImm(PtrSrc, VarRepr, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    StoreVar(VarRepr, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
    /// A call to a function that ommits .
    ///
    VoidCall(String, Vec<VarRepr>),
}

#[derive(Debug, Clone)]

pub(super) enum StatType {
    Deleted,
    Simple(StatCode, NodeRef<StatNode>),
    Final(StatCode),
    Branch(VarRepr, NodeRef<StatNode>, NodeRef<StatNode>),
}

#[derive(Debug, Clone)]
pub(super) struct StatNode {
    pub(super) incoming: Vec<NodeRef<StatNode>>, 
    pub(super) stat_type: StatType
}

impl StatNode {
    fn new_final(stat_code: StatCode) -> Self {
        StatNode {
            incoming: Vec::new(),
            stat_type: StatType::Final(stat_code)
        }
    }

    fn new_simple(stat_code: StatCode, next: NodeRef<StatNode>) -> Self {
        StatNode {
            incoming: Vec::new(),
            stat_type: StatType::Simple(stat_code, next)
        }
    }

    fn new_branch(cond: VarRepr, if_true: NodeRef<StatNode>, if_false: NodeRef<StatNode>) -> Self {
        StatNode { incoming: Vec::new(), stat_type: StatType::Branch(cond, if_true, if_false) }
    }
}

impl Deleted for StatNode {
    fn deleted() -> Self {
        StatNode{incoming: Vec::new(), stat_type: StatType::Deleted}
    }
}

/// Graph of statements. The index of a statement signifies the
/// [statement id](StatId) of that statement. The evaluation of the statement
/// graph starts at the first statement.
#[derive(Debug, Clone)]
pub(super) struct StatGraph {
    start: Option<NodeRef<StatNode>>,
    graph: Graph<StatNode>,
}

/// Local variables that have to be represented in memory during program execution.
pub(super) type LocalVars = HashMap<VarRepr, DataRef>;

#[derive(Debug, Clone)]
/// Function representation. The first vector are the variables to which the
/// arguments will be assigned to and the [statement graph](StatGraph) is the dataflow graph
/// that is evaluated.
pub(super) struct Function(pub Vec<VarRepr>, pub LocalVars, pub StatGraph);

#[derive(Debug, Clone)]
/// The entire program in the three-code representation. The first map is a map
/// of all functions defined by the program. The [statement graph](StatGraph) is
/// the main body of the program. The last map is a map of all statically-defined
/// data in the program.
pub(super) struct ThreeCode(
    pub HashMap<String, Function>,
    pub LocalVars,
    pub StatGraph,
    pub HashMap<DataRef, Vec<u8>>,
);

fn get_type_width(expr_type: ir::Type) -> Size {
    match expr_type {
        ir::Type::Num(ir::NumSize::DWord) => Size::DWord,
        ir::Type::Num(ir::NumSize::Word) => Size::Word,
        ir::Type::Num(ir::NumSize::Byte) => Size::Byte,
        ir::Type::Bool => Size::Byte,
        ir::Type::Ptr => Size::DWord,
    }
}

impl Into<i32> for Size {
    fn into(self) -> i32 {
        match self {
            Size::DWord => 4,
            Size::Word => 2,
            Size::Byte => 1,
        }
    }
}

struct PrintFmtDataRefFlags {
    integer: Option<DataRef>,
    ptr: Option<DataRef>,
    bool_true: Option<DataRef>,
    bool_false: Option<DataRef>,
    string: Option<DataRef>,
    char: Option<DataRef>,
    eol: Option<DataRef>,
}

fn push_stats_statgraph(
    stats: Vec<StatCode>,
    stat_graph: &mut StatGraph,
    end: &mut Option<NodeRef<StatNode>>,
) {
    for stat in stats {
        let new_node = stat_graph.graph.new_node(StatNode::new_final(stat));
        if let Some(end) = end {
            let mut end_node = end.set(StatNode::deleted());
            if let StatType::Final(stat_code) = end_node.stat_type {
                end_node.stat_type = StatType::Simple(stat_code, new_node.clone());
            } else {
                panic!("End node not a terminal node")
            }
            end.set(end_node);
            new_node.get_mut().incoming.push(end.clone());
        } else {
            stat_graph.start = Some(new_node.clone());
        }
        *end = Some(new_node);
    }
}

fn ensure_format(
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, Vec<u8>>,
    format: &str,
    fmt_flag: &mut Option<DataRef>,
) -> DataRef {
    if let Some(format) = *fmt_flag {
        format
    } else {
        *fmt_flag = Some(*free_data_ref);
        let mut format = format.to_owned().into_bytes();
        format.push(0);
        data_refs
            .insert(*free_data_ref, format)
            .map(|_| panic!("Data reference already used"));
        let data_ref = *free_data_ref;
        *free_data_ref += 1;
        data_ref
    }
}

fn translate_statement(
    stat: ir::Stat,
    free_var: VarRepr,
    stat_graph: &mut StatGraph,
    end_stat: &mut Option<NodeRef<StatNode>>,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, Vec<u8>>,
    print_fmt_flags: &mut PrintFmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    functions: &HashMap<String, ir::Function>,
    options: &Options,
) {
    let translation_data = ExprTranslationData::new(vars, functions, options);
    match stat {
        ir::Stat::AssignVar(var, expr) => {
            let mut stats = Vec::new();
            translate_expr(expr, var, &mut stats, translation_data);
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::AssignPtr(ptr_expr, expr) => {
            let mut stats = Vec::new();
            let store_width =
                get_type_width(translate_expr(expr, free_var, &mut stats, translation_data));
            let ptr_const =
                translate_ptr_expr(ptr_expr, free_var + 1, &mut stats, translation_data);
            if let Some(ptr_const) = ptr_const && options.propagation != PropagationOpt::None {
                stats.push(StatCode::StoreImm(ptr_const, free_var, store_width));
            } else {
                propagate_ptr_const(free_var + 1, &mut stats, ptr_const);
                stats.push(StatCode::StoreVar(free_var + 1, free_var, store_width));
            }
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::Free(ptr_expr, _) => {
            let mut stats = Vec::new();
            let ptr_const = translate_ptr_expr(ptr_expr, free_var, &mut stats, translation_data);
            propagate_ptr_const(free_var, &mut stats, ptr_const);
            stats.push(StatCode::Call(free_var, "free".to_string(), vec![free_var]));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintExpr(ir::Expr::Num(num_expr)) => {
            let mut stats = Vec::new();
            let (num_const, _) =
                translate_num_expr(num_expr, free_var, &mut stats, translation_data);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                data_refs.insert(*free_data_ref, format!("{}\0", num_const).into_bytes()).map(|_| panic!("Data reference already used"));
                stats.push(StatCode::Assign(free_var, OpSrc::DataRef(*free_data_ref, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var]));
                *free_data_ref += 1;

            } else {
                propagate_num_const(free_var, &mut stats, num_const);
                let integer_format = ensure_format(free_data_ref, data_refs, "%d", &mut print_fmt_flags.integer);
                stats.push(StatCode::Assign(free_var + 1, OpSrc::DataRef(integer_format, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var + 1, free_var]))
            }
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintExpr(ir::Expr::Bool(bool_expr)) => {
            let mut stats = Vec::new();
            let bool_const = translate_bool_expr(bool_expr, free_var, &mut stats, translation_data);
            if let Some(bool_const) = bool_const && options.propagation != PropagationOpt::None {
                let bool_format = if bool_const {
                    ensure_format(free_data_ref, data_refs, "true", &mut print_fmt_flags.bool_true)
                } else {
                    ensure_format(free_data_ref, data_refs, "false", &mut print_fmt_flags.bool_false)
                };
                stats.push(StatCode::Assign(free_var, OpSrc::DataRef(bool_format, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var]));
                push_stats_statgraph(stats, stat_graph, end_stat);
            } else {
                propagate_bool_const(free_var, &mut stats, bool_const);
                push_stats_statgraph(stats, stat_graph, end_stat);
                let true_format = ensure_format(free_data_ref, data_refs, "true", &mut print_fmt_flags.bool_true);
                let false_format = ensure_format(free_data_ref, data_refs, "false", &mut print_fmt_flags.bool_false);
                
                let print_node = stat_graph.graph.new_node(StatNode::new_final(StatCode::Call(free_var, "printf".to_string(), vec![free_var + 1])));
                let true_set = stat_graph.graph.new_node(StatNode::new_simple(StatCode::Assign(free_var + 1, OpSrc::DataRef(true_format, 0)), print_node.clone()));
                let false_set = stat_graph.graph.new_node(StatNode::new_simple(StatCode::Assign(free_var + 1, OpSrc::DataRef(false_format, 0)), print_node.clone()));
                print_node.get_mut().incoming.push(true_set.clone());
                print_node.get_mut().incoming.push(false_set.clone());

                let branch_node = stat_graph.graph.new_node(StatNode::new_branch(free_var, true_set.clone(), false_set.clone()));
                true_set.get_mut().incoming.push(branch_node.clone());
                false_set.get_mut().incoming.push(branch_node.clone());
                if let Some(end_stat) = end_stat {
                    let mut end_node = end_stat.set(StatNode::deleted());
                    if let StatType::Final(stat_code) = end_node.stat_type {
                        end_node.stat_type = StatType::Simple(stat_code, branch_node.clone());
                    } else {
                        unreachable!()
                    }
                    end_stat.set(end_node);
                    branch_node.get_mut().incoming.push(end_stat.clone());
                } else {
                    panic!("No calculation steps for the boolean constant");
                }
                
                *end_stat = Some(print_node);
            }
        }
        ir::Stat::PrintExpr(ir::Expr::Ptr(ptr_expr)) => {
            let mut stats = Vec::new();
            let ptr_const =
                translate_ptr_expr(ptr_expr, free_var, &mut stats, translation_data);
            propagate_ptr_const(free_var, &mut stats, ptr_const);
            let hex_format = ensure_format(free_data_ref, data_refs, "%x", &mut print_fmt_flags.ptr);
            stats.push(StatCode::Assign(free_var + 1, OpSrc::DataRef(hex_format, 0)));
            stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var + 1, free_var]));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintChar(num_expr) => {
            let mut stats = Vec::new();
            let (num_const, _) =
                translate_num_expr(num_expr, free_var, &mut stats, translation_data);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                data_refs.insert(*free_data_ref, format!("{}\0", num_const as u8 as char).into_bytes()).map(|_| panic!("Data reference already used"));
                stats.push(StatCode::Assign(free_var, OpSrc::DataRef(*free_data_ref, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var]));
                *free_data_ref += 1;

            } else {
                propagate_num_const(free_var, &mut stats, num_const);
                let integer_format = ensure_format(free_data_ref, data_refs, "%c", &mut print_fmt_flags.char);
                stats.push(StatCode::Assign(free_var + 1, OpSrc::DataRef(integer_format, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var + 1, free_var]))
            }
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintStr(ptr_expr, num_expr) => {
            let mut stats = Vec::new();
            let ptr_const = translate_ptr_expr(ptr_expr, free_var, &mut stats, translation_data);
            propagate_ptr_const(free_var, &mut stats, ptr_const);
            let (num_const, _) = translate_num_expr(num_expr, free_var + 1, &mut stats, translation_data);
            if let Some(num_const) = num_const && options.propagation != PropagationOpt::None {
                data_refs.insert(*free_data_ref, format!("%.{}s", num_const).into_bytes()).map(|_| panic!("Data reference already used"));
                stats.push(StatCode::Assign(free_var + 1, OpSrc::DataRef(*free_data_ref, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![]));
                *free_data_ref += 1;
            } else {
                propagate_num_const(free_var + 1, &mut stats, num_const);
                let string_format = ensure_format(free_data_ref, data_refs, "%.*s", &mut print_fmt_flags.string);
                stats.push(StatCode::Assign(free_var + 2, OpSrc::DataRef(string_format, 0)));
                stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var + 2, free_var + 1, free_var]));
            }
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        ir::Stat::PrintEol() => {
            let mut stats = Vec::new();
            let eol_format = ensure_format(free_data_ref, data_refs, "\n", &mut print_fmt_flags.eol);
            stats.push(StatCode::Assign(free_var, OpSrc::DataRef(eol_format, 0)));
            stats.push(StatCode::Call(free_var, "printf".to_string(), vec![free_var]));
            push_stats_statgraph(stats, stat_graph, end_stat);
        }
        _ => todo!()
    }
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from((program, options): (ir::Program, &Options)) -> ThreeCode {
        todo!()
    }
}
