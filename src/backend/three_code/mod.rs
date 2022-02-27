mod eval;
mod expr;
mod stat;

use super::Options;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use eval::eval_expr;
use expr::{translate_bool_expr, translate_expr, translate_num_expr};
use stat::{translate_statement, FmtDataRefFlags};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::iter::zip;
use std::mem;
use std::rc::Rc;

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

#[derive(Debug, PartialEq, Eq, Clone)]
/// Statement type
pub(super) enum StatCode {
    /// Assignment of one variable to another
    Assign(VarRepr, OpSrc),
    /// Assignment of a binary operation to a variable.
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    Load(VarRepr, VarRepr, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    Store(VarRepr, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
    /// A call to a function that ommits the return value. If the function name
    /// is not in the list of the [program](ThreeCode) functions then it is assumed
    /// to be external and linked to by the linker.
    VoidCall(String, Vec<VarRepr>),
    /// Returns from the function.
    Return(VarRepr),
}

/// General type of the statement. Used in the dataflow graph.
#[derive(Debug, Clone)]
pub(super) enum StatType {
    /// Used internally for the purposes of creating the graph.
    Dummy(Vec<StatNode>),
    /// Simple data manipulaiton statement that has a superceding statement.
    Simple(Vec<StatNode>, StatCode, StatNode),
    /// Data manipulation statement that is expected to finish the execution one
    /// way or another.
    Final(Vec<StatNode>, StatCode),
    /// A simple branch instruction that checks if the value in the variable is
    /// a boolean 1 or a 0.
    Branch(Vec<StatNode>, VarRepr, StatNode, StatNode),
    /// A self-looping infinite loop, which might occur as a user program.
    Loop(Vec<StatNode>),
}

/// A statement graph node.
type StatNode = NodeRef<StatType>;

#[derive(Debug, Clone)]
/// Function representation.
pub(super) struct Function {
    /// Variables for function argumetns
    pub args: Vec<VarRepr>,
    /// First statement of the program
    pub code: Option<StatNode>,
    /// Reference for usage when calling scanf
    pub read_ref: Option<DataRef>,
}

/// Type of the data reference under a [data reference id](DataRef).
#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum DataRefType {
    /// Represents an ascii string
    String(Vec<u8>),
    /// Represents a numerical struct with no padding
    Struct(Vec<(Size, i32)>),
}

#[derive(Debug, Clone)]
/// The entire program in the three-code representation.
pub(super) struct ThreeCode {
    /// All functions in the program
    pub functions: HashMap<String, Function>,
    /// Static data references in the program
    pub data_refs: HashMap<DataRef, DataRefType>,
    /// Graph of all statement nodes in the program
    pub graph: Graph<StatType>,
    /// Reference for usage when calling scanf
    pub read_ref: Option<DataRef>,
    /// First statement of the program
    pub code: Option<StatNode>,
    /// Function to call as an int overflow/underflow handler for checking for
    /// 32-bit overflows.
    pub int_handler: Option<String>,
}

impl StatType {
    fn new_final(stat_code: StatCode) -> Self {
        StatType::Final(Vec::new(), stat_code)
    }

    fn new_simple(stat_code: StatCode, next: StatNode) -> Self {
        StatType::Simple(Vec::new(), stat_code, next)
    }

    fn new_branch(cond: VarRepr, if_true: StatNode, if_false: StatNode) -> Self {
        StatType::Branch(Vec::new(), cond, if_true, if_false)
    }

    fn new_loop() -> Self {
        StatType::Loop(Vec::new())
    }

    fn add_incoming(&mut self, node: StatNode) {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Final(incoming, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Dummy(incoming) => incoming.push(node),
        }
    }

    fn set_incoming(&mut self, incoming: Vec<StatNode>) {
        match self {
            Self::Simple(old_incoming, _, _)
            | Self::Final(old_incoming, _)
            | Self::Branch(old_incoming, _, _, _)
            | Self::Loop(old_incoming)
            | Self::Dummy(old_incoming) => *old_incoming = incoming,
        }
    }

    fn incoming(&self) -> Vec<StatNode> {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Final(incoming, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Dummy(incoming) => incoming.clone(),
        }
    }

    fn append(&mut self, node: StatNode) {
        let mut tmp_node = Self::deleted();
        mem::swap(self, &mut tmp_node);
        tmp_node = match tmp_node {
            Self::Final(incoming, stat_code) => Self::Simple(incoming, stat_code, node),
            _ => panic!("Node not final"),
        };
        mem::swap(self, &mut tmp_node);
    }
}

impl Deleted for StatType {
    fn deleted() -> Self {
        Self::Dummy(Vec::new())
    }
}

#[derive(Clone)]
pub(self) enum StatLine {
    Empty(Rc<RefCell<Graph<StatType>>>),
    Line {
        start_node: StatNode,
        end_node: StatNode,
        graph: Rc<RefCell<Graph<StatType>>>,
    },
}

impl StatLine {
    fn new(graph: Rc<RefCell<Graph<StatType>>>) -> Self {
        StatLine::Empty(graph)
    }

    fn add_stat(&mut self, stat_code: StatCode) {
        self.add_node(
            self.graph()
                .borrow_mut()
                .new_node(StatType::new_final(stat_code)),
        );
    }

    fn add_node(&mut self, new_node: StatNode) {
        *self = match self.clone() {
            Self::Empty(graph) => Self::Line {
                start_node: new_node.clone(),
                end_node: new_node,
                graph,
            },
            Self::Line {
                start_node,
                end_node,
                graph,
            } => {
                new_node.get_mut().add_incoming(end_node.clone());
                end_node.get_mut().append(new_node.clone());
                Self::Line {
                    start_node,
                    end_node: new_node,
                    graph,
                }
            }
        };
    }

    fn set_end(&mut self, new_node: StatNode) {
        *self = match self.clone() {
            Self::Empty(graph) => Self::Line {
                start_node: new_node.clone(),
                end_node: new_node,
                graph,
            },
            Self::Line {
                start_node,
                end_node,
                graph,
            } => Self::Line {
                start_node,
                end_node: new_node,
                graph,
            },
        };
    }

    fn splice(&mut self, start_node: StatNode, end_node: StatNode) {
        self.add_node(start_node);
        self.set_end(end_node);
    }

    fn graph(&self) -> Rc<RefCell<Graph<StatType>>> {
        match self {
            Self::Empty(graph) => graph.clone(),
            Self::Line {
                start_node: _,
                end_node: _,
                graph,
            } => graph.clone(),
        }
    }

    fn start_node(&self) -> Option<StatNode> {
        match self {
            Self::Empty(_) => None,
            Self::Line {
                start_node,
                end_node: _,
                graph: _,
            } => Some(start_node.clone()),
        }
    }

    fn end_node(&self) -> Option<StatNode> {
        match self {
            Self::Empty(_) => None,
            Self::Line {
                start_node: _,
                end_node,
                graph: _,
            } => Some(end_node.clone()),
        }
    }
}

fn translate_block(
    ir::Block(_, stats, block_ending): ir::Block,
    free_var: VarRepr,
    stat_graph: Rc<RefCell<Graph<StatType>>>,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut Option<DataRef>,
    fmt_flags: &mut FmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) -> (StatNode, Vec<StatNode>, Option<StatNode>) {
    let mut stat_line = StatLine::new(stat_graph.clone());
    for stat in stats {
        translate_statement(
            stat,
            free_var,
            &mut stat_line,
            free_data_ref,
            data_refs,
            read_ref,
            fmt_flags,
            vars,
            function_types,
            options,
        );
    }
    let (cond_outputs, else_output) = match block_ending {
        ir::BlockEnding::Exit(num_expr) => {
            translate_num_expr(
                num_expr,
                free_var,
                &mut stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::VoidCall("exit".to_string(), vec![free_var]));
            (Vec::new(), None)
        }
        ir::BlockEnding::Return(expr) => {
            translate_expr(
                expr,
                free_var,
                &mut stat_line,
                vars,
                function_types,
                options,
            );
            stat_line.add_stat(StatCode::Return(free_var));
            (Vec::new(), None)
        }
        ir::BlockEnding::CondJumps(conds, last) => {
            let mut outputs = Vec::new();
            outputs.reserve_exact(conds.len());
            stat_line.add_node(stat_graph.borrow_mut().new_node(StatType::deleted()));

            for (bool_expr, block_id) in conds {
                let mut bool_stat_line = StatLine::new(stat_graph.clone());
                translate_bool_expr(
                    bool_expr,
                    free_var,
                    &mut bool_stat_line,
                    vars,
                    function_types,
                    options,
                );
                let true_dummy = stat_graph.borrow_mut().new_node(StatType::deleted());
                let false_dummy = stat_graph.borrow_mut().new_node(StatType::deleted());
                let cond_node = stat_graph.borrow_mut().new_node(StatType::new_branch(
                    free_var,
                    true_dummy.clone(),
                    false_dummy.clone(),
                ));
                true_dummy.get_mut().add_incoming(cond_node.clone());
                false_dummy.get_mut().add_incoming(cond_node.clone());
                bool_stat_line.splice(cond_node.clone(), false_dummy);

                let end_node = stat_line.end_node().unwrap();
                match &end_node.get_mut().incoming()[..] {
                    [] => {
                        stat_line = bool_stat_line;
                    }
                    [last_cond] => {
                        if let StatType::Branch(_, _, _, false_node) = &mut *last_cond.get_mut() {
                            *false_node = bool_stat_line.start_node().unwrap();
                        } else {
                            panic!("Expected a condition node")
                        }
                        stat_line.set_end(bool_stat_line.end_node().unwrap());
                    }
                    _ => panic!("Expected a dummy placeholder"),
                }
                stat_graph.borrow_mut().remove_node(end_node);

                outputs.push(cond_node);
            }

            let end_node = stat_line.end_node().unwrap();
            (
                outputs,
                match &end_node.clone().get_mut().incoming()[..] {
                    [] => {
                        stat_line = StatLine::new(stat_graph.clone());
                        stat_line.add_node(stat_graph.borrow_mut().new_node(StatType::new_loop()));
                        None
                    }
                    [last_node] => Some(last_node.clone()),
                    _ => panic!("Expected a non-singleton dummy placeholder"),
                },
            )
        }
    };
    (stat_line.start_node().unwrap(), cond_outputs, else_output)
}

fn clean_up_block_graph_dfs(
    block_graph: &mut ir::BlockGraph,
    block_id: ir::BlockId,
    mappings: &mut HashMap<ir::BlockId, ir::BlockId>,
    visited: &mut HashSet<ir::BlockId>,
) -> ir::BlockId {
    if visited.contains(&block_id) {
        return block_id;
    }
    visited.insert(block_id);
    let next_block_id = match &mut block_graph[block_id] {
        ir::Block(_, _, ir::BlockEnding::Exit(_) | ir::BlockEnding::Return(_)) => return block_id,
        ir::Block(_, stats, ir::BlockEnding::CondJumps(conds, else_id)) => {
            if conds.len() > 0 || stats.len() > 0 {
                mappings.insert(block_id, block_id);
                return block_id;
            }
            else_id
        }
    };
    let mapping_id = clean_up_block_graph_dfs(block_graph, block_id, mappings, visited);
    mappings.insert(block_id, mapping_id);
    mapping_id
}

// Make sure that there isn't an obsolete condition and that there is no
// empty conditionless block
fn clean_up_block_graph(block_graph: &mut ir::BlockGraph) {
    for ir::Block(_, _, block_ending) in block_graph.iter_mut() {
        if let ir::BlockEnding::CondJumps(conds, else_id) = block_ending {
            while let Some((_, true_id)) = conds.last() && *true_id == *else_id {
                conds.pop();
            }
        }
    }

    let mut edge_mappings = HashMap::new();
    let mut visited = HashSet::new();
    for block_id in 0..block_graph.len() {
        clean_up_block_graph_dfs(block_graph, block_id, &mut edge_mappings, &mut visited);
    }

    let mut new_block_graph_mappings = HashMap::new();
    for (new_block_id, old_block_id) in (0..block_graph.len())
        .filter(|block_id| edge_mappings[block_id] != *block_id)
        .enumerate()
    {
        new_block_graph_mappings.insert(old_block_id, new_block_id);
    }

    let mut old_block_graph = Vec::new();
    mem::swap(block_graph, &mut old_block_graph);

    *block_graph = old_block_graph
        .into_iter()
        .enumerate()
        .filter_map(|(block_id, block)| {
            if edge_mappings[&block_id] != block_id {
                None
            } else {
                Some(block)
            }
        })
        .collect();
    for ir::Block(incoming, _, ending) in block_graph {
        for incoming_id in incoming {
            *incoming_id = new_block_graph_mappings[&edge_mappings[incoming_id]];
        }
        if let ir::BlockEnding::CondJumps(conds, else_id) = ending {
            *else_id = new_block_graph_mappings[&edge_mappings[else_id]];
            for (_, cond_id) in conds {
                *cond_id = new_block_graph_mappings[&edge_mappings[else_id]];
            }
        }
    }
}

fn translate_block_graph(
    mut block_graph: ir::BlockGraph,
    stat_graph: Rc<RefCell<Graph<StatType>>>,
    free_var: VarRepr,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut Option<ir::DataRef>,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) -> Option<StatNode> {
    clean_up_block_graph(&mut block_graph);

    let mut fmt_flags = FmtDataRefFlags::default();
    let (start_nodes, block_cond_nodes): (Vec<_>, Vec<_>) = block_graph
        .into_iter()
        .map(|block| {
            let cond_ids = match &block {
                ir::Block(_, _, ir::BlockEnding::CondJumps(conds, else_id)) => Some((
                    conds
                        .iter()
                        .map(|(_, cond_id)| *cond_id)
                        .collect::<Vec<_>>(),
                    *else_id,
                )),
                _ => None,
            };
            let (start_node, cond_nodes, else_node) = translate_block(
                block,
                free_var,
                stat_graph.clone(),
                free_data_ref,
                data_refs,
                read_ref,
                &mut fmt_flags,
                vars,
                function_types,
                options,
            );
            (
                start_node,
                cond_ids.map(|(cond_ids, else_id)| {
                    assert_eq!(cond_nodes.len(), cond_ids.len());
                    (
                        zip(cond_ids, cond_nodes).collect::<Vec<_>>(),
                        (else_id, else_node.unwrap()),
                    )
                }),
            )
        })
        .unzip();

    for cond_nodes in &block_cond_nodes {
        if let Some((cond_maps, (else_id, else_node))) = cond_nodes {
            for (cond_id, cond_node) in cond_maps {
                if let StatType::Branch(_, _, true_node, _) = &mut *cond_node.get_mut() {
                    stat_graph.borrow_mut().remove_node(true_node.clone());
                    start_nodes[*cond_id]
                        .get_mut()
                        .add_incoming(cond_node.clone());
                    *true_node = start_nodes[*cond_id].clone();
                } else {
                    panic!("Expected a condition node")
                }
            }
            match &mut *else_node.get_mut() {
                StatType::Simple(_, _, next_node) | StatType::Branch(_, _, _, next_node) => {
                    stat_graph.borrow_mut().remove_node(next_node.clone());
                    start_nodes[*else_id]
                        .get_mut()
                        .add_incoming(else_node.clone());
                    *next_node = start_nodes[*else_id].clone();
                }
                _ => panic!("Expected a simple node or a branch node"),
            }
        }
    }

    start_nodes.into_iter().next()
}

fn translate_function(
    ir::Function(_, args, mut local_vars, block_graph): ir::Function,
    stat_graph: Rc<RefCell<Graph<StatType>>>,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    function_types: &HashMap<String, ir::Type>,
    options: &Options,
) -> Function {
    let mut three_code_args = Vec::new();
    for (arg_type, arg) in args {
        local_vars.insert(arg, arg_type);
        three_code_args.push(arg);
    }
    let free_var: VarRepr = local_vars
        .keys()
        .map(|var| *var)
        .max()
        .map(|var| var + 1)
        .unwrap_or(0);
    let mut read_ref = None;
    let start_node = translate_block_graph(
        block_graph,
        stat_graph.clone(),
        free_var,
        free_data_ref,
        data_refs,
        &mut read_ref,
        &mut local_vars,
        function_types,
        options,
    );
    Function {
        args: three_code_args,
        code: start_node,
        read_ref: read_ref,
    }
}

impl From<(ir::Program, &Options)> for ThreeCode {
    fn from(
        (ir::Program(functions, local_vars, block_graph, data_refs, int_handler), options): (
            ir::Program,
            &Options,
        ),
    ) -> ThreeCode {
        let stat_graph = Rc::new(RefCell::new(Graph::new()));
        let free_var: VarRepr = local_vars
            .keys()
            .map(|var| *var)
            .max()
            .map(|var| var + 1)
            .unwrap_or(0);
        let function_types = functions
            .iter()
            .map(|(name, function)| {
                let &ir::Function(ret_type, _, _, _) = function;
                (name.clone(), ret_type)
            })
            .collect::<HashMap<_, _>>();
        let mut data_refs = data_refs
            .into_iter()
            .map(|(data_ref, exprs)| {
                (data_ref, {
                    let struct_consts = exprs.into_iter().map(eval_expr).collect::<Vec<_>>();
                    if struct_consts
                        .iter()
                        .all(|(const_size, _)| *const_size == Size::Byte)
                    {
                        DataRefType::String(
                            struct_consts
                                .into_iter()
                                .map(|(_, val)| val as u8)
                                .collect(),
                        )
                    } else {
                        DataRefType::Struct(struct_consts)
                    }
                })
            })
            .collect::<HashMap<_, _>>();
        let mut free_data_ref = data_refs
            .keys()
            .map(|data_ref| *data_ref)
            .max()
            .map(|data_ref| data_ref + 1)
            .unwrap_or(0);
        let functions = functions
            .into_iter()
            .map(|(name, function)| {
                (
                    name,
                    translate_function(
                        function,
                        stat_graph.clone(),
                        &mut free_data_ref,
                        &mut data_refs,
                        &function_types,
                        options,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();
        let mut read_ref = None;
        let start_node = translate_block_graph(
            block_graph,
            stat_graph.clone(),
            free_var,
            &mut free_data_ref,
            &mut data_refs,
            &mut read_ref,
            &local_vars,
            &function_types,
            options,
        );
        ThreeCode {
            functions: functions,
            data_refs: data_refs,
            graph: Rc::try_unwrap(stat_graph)
                .expect("Graph used in more than one place")
                .into_inner(),
            read_ref,
            code: start_node,
            int_handler: int_handler,
        }
    }
}
