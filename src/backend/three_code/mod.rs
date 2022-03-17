//! Definition of (and translation from [ir] to) the architecture-agnostic
//! [threecode](ThreeCode) representation.
//!
//! It is designed to be a generalized assembly language to allow for extension
//! to many different architectures. It supports basic operations, calls (including
//! void), stack memory references associated with functions (readref), and
//! structs in its data section.
//!
//! It is structured as a control flow graph with a single statement per node,
//! which allows for fine grained analysis of live ranges, dead code, etc.
//!
//! Control flow is managed by the [stat type](StatType) enumeration. While most
//! statements are contained in [stat code nodes](StatCode), including binary
//! operations (which can be set as overflow checked), assignments, load/store and
//! function calls.
//!
//! Display is implemented for the threecode to allow for debugging, and analysis
//! of the optimizations performed on threecode.

mod eval;
mod expr;
mod stat;
pub mod three_code_display;

use self::expr::assign_op_src;

use super::data_flow::DataflowNode;
use super::Options;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{self as ir, DataRef, VarRepr};
use eval::eval_expr;
use expr::{translate_bool_expr, translate_expr, translate_num_expr};
use stat::{translate_statement, FmtDataRefFlags};
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, LinkedList};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
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
    /// A reference to a special field on the stack just for reading into variables
    ReadRef,
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
    AssignOp(VarRepr, OpSrc, BinOp, OpSrc, bool),
    /// Load from a reference to a pointer. The first variable reference is
    /// the load destination and the second one is the pointer to the data. The
    /// number of bytes loaded is signified by the [size](Size) field.
    Load(VarRepr, OpSrc, Size),
    /// Store to a pointer reference. The first variable reference is the pointer to the
    /// store destination and the second one is the variable to store the data from.
    /// The number of bytes stored is signified by the [size](Size) field.
    Store(OpSrc, VarRepr, Size),
    /// A call to a function. If the function name is not in the list of the
    /// [program](ThreeCode) functions then it is assumed to be external and linked
    /// to by the linker.
    Call(VarRepr, String, Vec<VarRepr>),
    /// A call to a function that ommits the return value. If the function name
    /// is not in the list of the [program](ThreeCode) functions then it is assumed
    /// to be external and linked to by the linker.
    VoidCall(String, Vec<VarRepr>),
}

/// General type of the statement. Used in the dataflow graph.
#[derive(Clone)]
pub(super) enum StatType {
    /// Used internally for the purposes of creating the graph.
    Dummy(Vec<StatNode>),
    /// Simple data manipulaiton statement that has a superceding statement.
    Simple(Vec<StatNode>, StatCode, StatNode),
    /// A simple branch instruction that checks if the value in the variable is
    /// a boolean 1 or a 0.
    Branch(Vec<StatNode>, OpSrc, StatNode, StatNode),
    /// A self-looping infinite loop, which might occur as a user program.
    Loop(Vec<StatNode>),
    /// A return from a function. The variable contains the return value.
    Return(Vec<StatNode>, Option<OpSrc>),
}

/// A statement graph node.
pub(super) type StatNode = NodeRef<StatType>;

#[derive(Debug, Clone)]
/// Function representation.
pub(super) struct Function {
    /// Variables for function argumetns
    pub args: Vec<VarRepr>,
    /// First statement of the program
    pub code: StatNode,
    /// Graph of all statement nodes in the function
    pub graph: Graph<StatType>,
    /// Whether a [read ref operand source](OpSrc::ReadRef) is used in the function code
    pub read_ref: bool,
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
    /// Graph of all statement nodes in the main program body
    pub graph: Graph<StatType>,
    /// Whether a [read ref operand source](OpSrc::ReadRef) is used in the main program code
    pub read_ref: bool,
    /// First statement of the program
    pub code: StatNode,
    /// Function to call as an int overflow/underflow handler for checking for
    /// 32-bit overflows.
    pub int_handler: Option<String>,
}

impl StatType {
    /// Creates a new simple statement with an empty list of incoming nodes.
    pub(super) fn new_simple(stat_code: StatCode, next: StatNode) -> Self {
        StatType::Simple(vec![], stat_code, next)
    }

    /// Creates a new branch statement with an empty list of incoming nodes.
    pub(super) fn new_branch(cond: OpSrc, if_true: StatNode, if_false: StatNode) -> Self {
        StatType::Branch(vec![], cond, if_true, if_false)
    }

    /// Creates a new loop statement with an empty list of incoming nodes.
    pub(super) fn new_loop() -> Self {
        StatType::Loop(vec![])
    }

    /// Creates a new return statement with an empty list of incoming nodes.
    pub(super) fn new_return(ret: Option<OpSrc>) -> Self {
        StatType::Return(vec![], ret)
    }

    /// Adds an incoming node to the list of incoming nodes.
    pub(super) fn add_incoming(&mut self, node: StatNode) {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Return(incoming, _)
            | Self::Dummy(incoming) => incoming.push(node),
        }
    }

    /// Sets the list of incoming nodes.
    pub(super) fn set_incoming(&mut self, incoming: Vec<StatNode>) {
        match self {
            Self::Simple(old_incoming, _, _)
            | Self::Branch(old_incoming, _, _, _)
            | Self::Loop(old_incoming)
            | Self::Return(old_incoming, _)
            | Self::Dummy(old_incoming) => *old_incoming = incoming,
        }
    }

    pub(super) fn remove_incoming(&mut self, node: &StatNode) {
        match self {
            Self::Simple(old_incoming, _, _)
            | Self::Branch(old_incoming, _, _, _)
            | Self::Loop(old_incoming)
            | Self::Return(old_incoming, _)
            | Self::Dummy(old_incoming) => old_incoming.retain(|other_node| other_node != node),
        }
    }

    /// Substitutes one child of a node for a new one
    pub(super) fn substitute_child(&mut self, old_child: &StatNode, new_child: &StatNode) {
        let mut tmp_node = Self::deleted();
        mem::swap(self, &mut tmp_node);
        let mut tmp_node = match tmp_node {
            Self::Simple(incoming, stat_code, next_node) => Self::Simple(
                incoming,
                stat_code,
                if &next_node == old_child {
                    new_child.clone()
                } else {
                    next_node
                },
            ),
            Self::Branch(incoming, var, true_node, false_node) => Self::Branch(
                incoming,
                var,
                if &true_node == old_child {
                    new_child.clone()
                } else {
                    true_node
                },
                if &false_node == old_child {
                    new_child.clone()
                } else {
                    false_node
                },
            ),
            tmp_node => tmp_node,
        };
        mem::swap(self, &mut tmp_node);
    }

    /// Constructs the list of successor nodes.
    pub(super) fn successors(&self) -> Vec<StatNode> {
        match self {
            Self::Loop(_) | Self::Return(_, _) | Self::Dummy(_) => vec![],
            Self::Simple(_, _, next_node) => vec![next_node.clone()],
            Self::Branch(_, _, true_node, false_node) => {
                vec![true_node.clone(), false_node.clone()]
            }
        }
    }

    /// If a node is a simple node, sets
    /// the next node as the given node and returns the old next node for removal.
    pub(super) fn append(&mut self, next_node: StatNode) -> StatNode {
        let mut tmp_node = Self::deleted();
        mem::swap(self, &mut tmp_node);
        let (incoming, stat_code, old_next_node) =
            if let Self::Simple(incoming, stat_code, old_next_node) = tmp_node {
                (incoming, stat_code, old_next_node)
            } else {
                panic!("Expected a simple node")
            };
        let mut tmp_node = Self::Simple(incoming, stat_code, next_node);
        mem::swap(self, &mut tmp_node);
        old_next_node
    }
}

impl DataflowNode for StatType {
    fn incoming(&self) -> Vec<&StatNode> {
        match self {
            Self::Simple(incoming, _, _)
            | Self::Branch(incoming, _, _, _)
            | Self::Loop(incoming)
            | Self::Return(incoming, _)
            | Self::Dummy(incoming) => incoming.iter().collect(),
        }
    }

    fn outgoing(&self) -> Vec<&StatNode> {
        match self {
            Self::Loop(_) | Self::Return(_, _) | Self::Dummy(_) => vec![],
            Self::Simple(_, _, next_node) => vec![next_node],
            Self::Branch(_, _, true_node, false_node) => {
                vec![true_node, false_node]
            }
        }
    }
}

pub(super) fn hashed<T: Hash>(x: T) -> u8 {
    let mut h = DefaultHasher::new();
    x.hash(&mut h);
    h.finish() as u8
}

pub(super) fn hashed_array<T: Hash, C: IntoIterator<Item = T>>(collection: C) -> LinkedList<u8> {
    collection.into_iter().map(hashed).collect()
}

impl Debug for StatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Dummy(incoming) => f
                .debug_tuple("Dummy")
                .field(&hashed_array(incoming))
                .finish(),
            Self::Simple(incoming, stat_code, next_node) => f
                .debug_tuple("Simple")
                .field(&hashed_array(incoming))
                .field(stat_code)
                .field(&hashed(next_node))
                .finish(),
            Self::Branch(incoming, var, true_node, false_node) => f
                .debug_tuple("Branch")
                .field(&hashed_array(incoming))
                .field(var)
                .field(&hashed(true_node))
                .field(&hashed(false_node))
                .finish(),
            Self::Loop(incoming) => f
                .debug_tuple("Loop")
                .field(&hashed_array(incoming))
                .finish(),
            Self::Return(incoming, var) => f
                .debug_tuple("Return")
                .field(&hashed_array(incoming))
                .field(var)
                .finish(),
        }
    }
}

impl Deleted for StatType {
    fn deleted() -> Self {
        Self::Dummy(vec![])
    }
}

impl ThreeCode {
    /// Makes sure there are no left-over dummy nodes in the graph
    pub(super) fn check_dummy(&self) -> Result<(), ()> {
        let Self {
            functions: _,
            data_refs: _,
            graph,
            read_ref: _,
            code: _,
            int_handler: _,
        } = self;
        for node in graph {
            if let StatType::Dummy(_) = &*node.get() {
                return Err(());
            }
        }
        Ok(())
    }
}

/// A line-like graph with a single start and end node.
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
    /// Creates a new statement line.
    fn new(graph: Rc<RefCell<Graph<StatType>>>) -> Self {
        StatLine::Empty(graph)
    }

    /// Appends the given statement into the statement line.
    fn add_stat(&mut self, stat_code: StatCode) {
        let dummy_node = self.graph().borrow_mut().new_node(StatType::deleted());
        let new_node = self
            .graph()
            .borrow_mut()
            .new_node(StatType::new_simple(stat_code, dummy_node));
        self.add_node(new_node);
    }

    /// Appends the given node into the statement line. Assumes that it is a
    /// final node.
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
                graph
                    .borrow_mut()
                    .remove_node(end_node.get_mut().append(new_node.clone()));
                Self::Line {
                    start_node,
                    end_node: new_node,
                    graph,
                }
            }
        };
    }

    /// Sets a new end node for the statement line.
    fn set_end(&mut self, new_node: StatNode) {
        *self = match self.clone() {
            Self::Empty(graph) => Self::Line {
                start_node: new_node.clone(),
                end_node: new_node,
                graph,
            },
            Self::Line {
                start_node,
                end_node: _,
                graph,
            } => Self::Line {
                start_node,
                end_node: new_node,
                graph,
            },
        };
    }

    /// Appends a graph starting and ending with given nodes. The nodes
    /// are assumed to be connected. The new end node is assumed to be a final
    /// node.
    fn splice(&mut self, start_node: StatNode, end_node: StatNode) {
        self.add_node(start_node);
        self.set_end(end_node);
    }

    /// Returns the underlying graph.
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

    /// Returns the start node, if there are any nodes in the graph.
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

    /// Returns the end node, if there are any nodes in the graph.
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

/// Translates a single statement block. Returns the start node of the statement,
/// the endpoints corresponding to the conditional branches from the block and
/// the node corresponding to the unconditional branch, if one exists. The endpoint
/// nodes are branch nodes and have dummy nodes installed at their ends, which
/// should be removed when attaching actual nodes. The nodes corresponding to
/// the conditional branches should have nodes attached to the true branch, and
/// the one corresponding to the unconditional branch should have a node attached
/// to the false branch. The arguments are as follows:
///  - The block of statements to translate.
///  - The graph in which to allocate new nodes.
///  - Some variable not used in the program. It is assumed that all
///    variables superceding are not used either.
///  - Some data reference not used in the program. It is assumed that all data
///    references superceding are not used either. Will be automatically updated
///    if a data reference is placed under it.
///  - All static data references used in the program.
///  - An optional data reference to a read field. The field is supposed to be
///    4 bytes long. The initial value of that field should be set to Null.
///    Updated if a data reference is needed.
///  - [Format flags](FmtDataRefFlags). See the type comment for more detail.
///    Updated if necessary.
///  - Types of variables used within the intermediate representation.
///  - Types of return values of functions used within the intermediate
///    representation.
///  - [Compilation options](Options).
#[allow(clippy::too_many_arguments)]
fn translate_block(
    ir::Block(_, stats, block_ending): ir::Block,
    stat_graph: Rc<RefCell<Graph<StatType>>>,
    free_var: VarRepr,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut bool,
    fmt_flags: &mut FmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
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
            let (_, op_src) = translate_num_expr(
                num_expr,
                free_var,
                &mut stat_line,
                vars,
                function_types,
                options,
            );
            assign_op_src(free_var, op_src, &mut stat_line);
            stat_line.add_stat(StatCode::VoidCall("exit".to_string(), vec![free_var]));
            let return_node = stat_graph.borrow_mut().new_node(StatType::new_return(None));
            stat_line.add_node(return_node);
            (vec![], None)
        }
        ir::BlockEnding::Return(expr) => {
            if let Some(expr) = expr {
                let (_, op_src) = translate_expr(
                    expr,
                    free_var,
                    &mut stat_line,
                    vars,
                    function_types,
                    options,
                );
                let return_node = stat_graph
                    .borrow_mut()
                    .new_node(StatType::new_return(Some(op_src)));
                stat_line.add_node(return_node);
            } else {
                let return_node = stat_graph.borrow_mut().new_node(StatType::new_return(None));
                stat_line.add_node(return_node);
            }
            (vec![], None)
        }
        ir::BlockEnding::CondJumps(conds, _) => {
            let mut outputs = vec![];
            outputs.reserve_exact(conds.len());
            let dummy_node = stat_graph.borrow_mut().new_node(StatType::deleted());
            stat_line.add_node(dummy_node);

            for (bool_expr, _) in conds {
                let mut bool_stat_line = StatLine::new(stat_graph.clone());
                let op_src = translate_bool_expr(
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
                    op_src,
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
                        if let StatType::Simple(_, _, end_node_ref)
                        | StatType::Branch(_, _, _, end_node_ref) = &mut *last_cond.get_mut()
                        {
                            *end_node_ref = bool_stat_line.start_node().unwrap();
                        } else {
                            panic!("Expected a simple node or a condition node")
                        }
                        stat_line.set_end(bool_stat_line.end_node().unwrap());
                    }
                    _ => panic!("Expected a dummy placeholder"),
                }
                stat_graph.borrow_mut().remove_node(end_node);

                outputs.push(cond_node);
            }

            let end_node = stat_line.end_node().unwrap();
            let else_output = match end_node.get_mut().incoming()[..] {
                [] => {
                    stat_line = StatLine::new(stat_graph.clone());
                    let loop_node = stat_graph.borrow_mut().new_node(StatType::new_loop());
                    stat_line.add_node(loop_node);
                    None
                }
                [last_node] => Some(last_node.clone()),
                _ => panic!("Expected a non-singleton dummy placeholder"),
            };
            (outputs, else_output)
        }
    };
    (stat_line.start_node().unwrap(), cond_outputs, else_output)
}

/// Helper recursion function for the [block graph cleanup](clean_up_block_graph).
fn clean_up_block_graph_dfs(
    block_graph: &mut ir::BlockGraph,
    block_id: ir::BlockId,
    mappings: &mut HashMap<ir::BlockId, ir::BlockId>,
) -> ir::BlockId {
    if mappings.contains_key(&block_id) {
        return mappings[&block_id];
    }

    mappings.insert(block_id, block_id);
    let next_block_id = match &mut block_graph[block_id] {
        ir::Block(_, _, ir::BlockEnding::Exit(_) | ir::BlockEnding::Return(_)) => {
            mappings.insert(block_id, block_id);
            return block_id;
        }
        ir::Block(_, stats, ir::BlockEnding::CondJumps(conds, else_id)) => {
            if !conds.is_empty() || !stats.is_empty() {
                mappings.insert(block_id, block_id);
                return block_id;
            }
            *else_id
        }
    };
    let mapping_id = clean_up_block_graph_dfs(block_graph, next_block_id, mappings);
    mappings.insert(block_id, mapping_id);
    mapping_id
}

/// Gets rid of conditional branches that lead to the same block as the unconditional branch
/// and directly precede it in terms of execution, as well as blocks with just
/// unconditional branches and no statements, unless they point to themselves.
fn clean_up_block_graph(block_graph: &mut ir::BlockGraph) {
    let mut edge_mappings = HashMap::new();
    for block_id in 0..block_graph.len() {
        clean_up_block_graph_dfs(block_graph, block_id, &mut edge_mappings);
    }

    let mut new_block_graph_mappings = HashMap::new();
    for (new_block_id, old_block_id) in (0..block_graph.len())
        .filter(|block_id| edge_mappings[block_id] == *block_id)
        .enumerate()
    {
        new_block_graph_mappings.insert(old_block_id, new_block_id);
    }

    let mut old_block_graph = vec![];
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

    for ir::Block(incoming, _, ending) in &mut *block_graph {
        for incoming_id in incoming {
            *incoming_id = new_block_graph_mappings[&edge_mappings[incoming_id]];
        }
        if let ir::BlockEnding::CondJumps(conds, else_id) = ending {
            *else_id = new_block_graph_mappings[&edge_mappings[else_id]];
            for (_, cond_id) in conds {
                *cond_id = new_block_graph_mappings[&edge_mappings[cond_id]];
            }
        }
    }
}

/// Translates an entire [block graph](ir::BlockGraph). Returns the node corresponding to the
/// first statement to execute, provided there is any. The arguments are as follows:
///  - The [block graph](ir::BlockGraph) to translate.
///  - The graph in which to allocate new nodes.
///  - Some variable not used in the program. It is assumed that all
///    variables superceding are not used either.
///  - Some data reference not used in the program. It is assumed that all data
///    references superceding are not used either. Will be automatically updated
///    if a data reference is placed under it.
///  - All static data references used in the program.
///  - An optional data reference to a read field. The field is supposed to be
///    4 bytes long. The initial value of that field should be set to Null.
///    Updated if a data reference is needed.
///  - [Format flags](FmtDataRefFlags). See the type comment for more detail.
///    Updated if necessary.
///  - Types of variables used within the intermediate representation.
///  - Types of return values of functions used within the intermediate
///    representation.
///  - [Compilation options](Options).
#[allow(clippy::too_many_arguments)]
fn translate_block_graph(
    mut block_graph: ir::BlockGraph,
    stat_graph: Rc<RefCell<Graph<StatType>>>,
    free_var: VarRepr,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    read_ref: &mut bool,
    fmt_flags: &mut FmtDataRefFlags,
    vars: &HashMap<VarRepr, ir::Type>,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> StatNode {
    clean_up_block_graph(&mut block_graph);

    let (start_nodes, block_cond_nodes): (Vec<_>, Vec<_>) = block_graph
        .into_iter()
        .map(|block| {
            let cond_ids = match &block {
                ir::Block(_, stats, ir::BlockEnding::CondJumps(conds, else_id)) => {
                    if !stats.is_empty() || !conds.is_empty() {
                        Some((
                            conds
                                .iter()
                                .map(|(_, cond_id)| *cond_id)
                                .collect::<Vec<_>>(),
                            *else_id,
                        ))
                    } else {
                        None
                    }
                }
                _ => None,
            };
            let (start_node, cond_nodes, else_node) = translate_block(
                block,
                stat_graph.clone(),
                free_var,
                free_data_ref,
                data_refs,
                read_ref,
                fmt_flags,
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

    for (cond_maps, (else_id, else_node)) in block_cond_nodes.iter().flatten() {
        for (cond_id, cond_node) in cond_maps {
            if let StatType::Branch(_, _, true_node, _) = &mut *cond_node.get_mut() {
                stat_graph.borrow_mut().remove_node(true_node.clone());
                *true_node = start_nodes[*cond_id].clone();
            } else {
                panic!("Expected a condition node")
            };
            start_nodes[*cond_id]
                .get_mut()
                .add_incoming(cond_node.clone());
        }
        if let StatType::Simple(_, _, next_node) | StatType::Branch(_, _, _, next_node) =
            &mut *else_node.get_mut()
        {
            stat_graph.borrow_mut().remove_node(next_node.clone());
            *next_node = start_nodes[*else_id].clone();
        } else {
            panic!("Expected a simple node or a branch node")
        }
        start_nodes[*else_id]
            .get_mut()
            .add_incoming(else_node.clone());
    }

    start_nodes
        .into_iter()
        .next()
        .unwrap_or_else(|| stat_graph.borrow_mut().new_node(StatType::new_return(None)))
}

/// Translates a function. Returns the function in a three-code representation.
/// The arguments are as follows:
///  - The function to translate
///  - The graph in which to allocate new nodes.
///  - Some data reference not used in the program. It is assumed that all data
///    references superceding are not used either. Will be automatically updated
///    if a data reference is placed under it.
///  - All static data references used in the program.
///  - [Format flags](FmtDataRefFlags). See the type comment for more detail.
///    Updated if necessary.
///  - Types of return values of functions used within the intermediate
///    representation.
///  - [Compilation options](Options).
fn translate_function(
    ir::Function(_, args, mut local_vars, block_graph): ir::Function,
    free_data_ref: &mut DataRef,
    data_refs: &mut HashMap<DataRef, DataRefType>,
    fmt_flags: &mut FmtDataRefFlags,
    function_types: &HashMap<String, Option<ir::Type>>,
    options: &Options,
) -> Function {
    let mut three_code_args = vec![];
    for (arg_type, arg) in args {
        local_vars.insert(arg, arg_type);
        three_code_args.push(arg);
    }
    let free_var: VarRepr = local_vars
        .keys()
        .copied()
        .max()
        .map(|var| var + 1)
        .unwrap_or(0);
    let mut read_ref = false;
    let stat_graph: Rc<RefCell<Graph<StatType>>> = Rc::new(RefCell::new(Graph::new()));

    let start_node = translate_block_graph(
        block_graph,
        stat_graph.clone(),
        free_var,
        free_data_ref,
        data_refs,
        &mut read_ref,
        fmt_flags,
        &local_vars,
        function_types,
        options,
    );
    Function {
        args: three_code_args,
        code: start_node,
        graph: Rc::try_unwrap(stat_graph)
            .expect("Graphs exists in more than one place")
            .into_inner(),
        read_ref,
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
            .copied()
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
            .copied()
            .max()
            .map(|data_ref| data_ref + 1)
            .unwrap_or(0);
        let mut fmt_flags = FmtDataRefFlags::default();
        let functions = functions
            .into_iter()
            .map(|(name, function)| {
                (
                    name,
                    translate_function(
                        function,
                        &mut free_data_ref,
                        &mut data_refs,
                        &mut fmt_flags,
                        &function_types,
                        options,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();
        let mut read_ref = false;
        let start_node = translate_block_graph(
            block_graph,
            stat_graph.clone(),
            free_var,
            &mut free_data_ref,
            &mut data_refs,
            &mut read_ref,
            &mut fmt_flags,
            &local_vars,
            &function_types,
            options,
        );
        ThreeCode {
            functions,
            data_refs,
            graph: Rc::try_unwrap(stat_graph)
                .expect("Graph used in more than one place")
                .into_inner(),
            read_ref,
            code: start_node,
            int_handler,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::stat::tests::match_graph;
    use super::{
        super::Options, translate_block, translate_block_graph, translate_function, DataRefType,
        Function, OpSrc, Size, StatCode, StatType, ThreeCode,
    };
    use crate::{
        backend::three_code::stat::FmtDataRefFlags,
        graph::{Deleted, Graph},
        intermediate as ir,
    };
    use std::mem;
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    #[test]
    fn translate_block_test() {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut data_refs = HashMap::new();
        let mut read_ref = false;
        let (start_node, _, _) = translate_block(
            ir::Block(
                vec![],
                vec![
                    ir::Stat::ReadIntVar(0),
                    ir::Stat::PrintExpr(ir::Expr::Num(ir::NumExpr::Var(0))),
                ],
                ir::BlockEnding::Exit(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
            ),
            graph.clone(),
            1,
            &mut 0,
            &mut data_refs,
            &mut read_ref,
            &mut FmtDataRefFlags::default(),
            &HashMap::from([(0, ir::Type::Num(ir::NumSize::DWord))]),
            &HashMap::new(),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
            },
        );

        assert!(read_ref);
        assert_eq!(
            data_refs,
            HashMap::from([(0, DataRefType::String("%d\0".as_bytes().to_vec()))])
        );

        let mut graph = Rc::try_unwrap(graph)
            .expect("Multiple references to the graph")
            .into_inner();
        let return_node = graph.new_node(StatType::new_return(None));
        let exit_node = graph.new_node(StatType::new_simple(
            StatCode::VoidCall("exit".to_string(), vec![1]),
            return_node,
        ));
        let exit_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::Const(0)),
            exit_node,
        ));
        let flush_call_node = graph.new_node(StatType::new_simple(
            StatCode::VoidCall("fflush".to_string(), vec![1]),
            exit_set_node,
        ));
        let stdout_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::Const(0)),
            flush_call_node,
        ));
        let print_call_node = graph.new_node(StatType::new_simple(
            StatCode::VoidCall("printf".to_string(), vec![2, 1]),
            stdout_set_node,
        ));
        let print_fmt_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(2, OpSrc::DataRef(0, 0)),
            print_call_node,
        ));
        let print_val_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::Var(0)),
            print_fmt_set_node,
        ));
        let scan_load_node = graph.new_node(StatType::new_simple(
            StatCode::Load(0, OpSrc::ReadRef, Size::DWord),
            print_val_set_node,
        ));
        let scan_call_node = graph.new_node(StatType::new_simple(
            StatCode::VoidCall("scanf".to_string(), vec![2, 1]),
            scan_load_node,
        ));
        let scan_fmt_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(2, OpSrc::DataRef(0, 0)),
            scan_call_node,
        ));
        let scan_set_ref = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::ReadRef),
            scan_fmt_set_node,
        ));
        let scan_store_node = graph.new_node(StatType::new_simple(
            StatCode::Store(OpSrc::ReadRef, 0, Size::DWord),
            scan_set_ref,
        ));

        match_graph(start_node, scan_store_node);
    }

    #[test]
    fn translate_block_graph_test() {
        let graph = Rc::new(RefCell::new(Graph::new()));
        let mut data_refs = HashMap::new();
        let start_node = translate_block_graph(
            vec![
                ir::Block(
                    vec![0],
                    vec![ir::Stat::AssignVar(
                        0,
                        ir::Expr::Bool(ir::BoolExpr::Const(true)),
                    )],
                    ir::BlockEnding::CondJumps(
                        vec![
                            (ir::BoolExpr::Const(true), 0),
                            (ir::BoolExpr::Var(0), 1),
                            (ir::BoolExpr::Const(true), 2),
                        ],
                        2,
                    ),
                ),
                ir::Block(
                    vec![0],
                    vec![],
                    ir::BlockEnding::Return(Some(ir::Expr::Bool(ir::BoolExpr::Var(0)))),
                ),
                ir::Block(vec![0, 4], vec![], ir::BlockEnding::CondJumps(vec![], 3)),
                ir::Block(vec![2], vec![], ir::BlockEnding::CondJumps(vec![], 4)),
                ir::Block(vec![3], vec![], ir::BlockEnding::CondJumps(vec![], 2)),
            ],
            graph.clone(),
            1,
            &mut 0,
            &mut data_refs,
            &mut false,
            &mut FmtDataRefFlags::default(),
            &HashMap::from([(0, ir::Type::Bool)]),
            &HashMap::new(),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
            },
        );

        let mut graph = Rc::try_unwrap(graph)
            .expect("Multiple references to the graph")
            .into_inner();
        let loop_node = graph.new_node(StatType::new_loop());
        let return_node = graph.new_node(StatType::new_return(Some(OpSrc::Var(1))));
        let return_assign_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::Var(0)),
            return_node,
        ));
        let branch3_node = graph.new_node(StatType::new_branch(
            OpSrc::Const(1),
            loop_node.clone(),
            loop_node,
        ));
        let branch2_node = graph.new_node(StatType::new_branch(
            OpSrc::Var(1),
            return_assign_node,
            branch3_node,
        ));
        let branch2_cond_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(1, OpSrc::Var(0)),
            branch2_node,
        ));
        let branch1_node = graph.new_node(StatType::deleted());
        let var_set_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(0, OpSrc::Const(1)),
            branch1_node.clone(),
        ));
        branch1_node.set(StatType::new_branch(
            OpSrc::Const(1),
            var_set_node.clone(),
            branch2_cond_set_node,
        ));

        match_graph(start_node, var_set_node);
    }

    #[test]
    fn translate_function_test() {
        let mut data_refs = HashMap::new();
        let Function {
            args,
            code,
            mut graph,
            read_ref,
        } = translate_function(
            ir::Function(
                Some(ir::Type::Bool),
                vec![(ir::Type::Num(ir::NumSize::DWord), 0)],
                HashMap::from([(1, ir::Type::Num(ir::NumSize::Byte))]),
                vec![ir::Block(
                    vec![],
                    vec![],
                    ir::BlockEnding::Return(Some(ir::Expr::Bool(ir::BoolExpr::Const(false)))),
                )],
            ),
            &mut 0,
            &mut data_refs,
            &mut FmtDataRefFlags::default(),
            &HashMap::from([("function".to_string(), Some(ir::Type::Bool))]),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
            },
        );
        assert_eq!(args, vec![0]);
        assert!(!read_ref);

        let return_node = graph.new_node(StatType::new_return(Some(OpSrc::Const(0))));
        match_graph(code, return_node);
    }

    #[test]
    fn translate_void_function_test() {
        let mut data_refs = HashMap::new();
        let Function {
            args,
            code,
            mut graph,
            read_ref,
        } = translate_function(
            ir::Function(
                None,
                vec![(ir::Type::Num(ir::NumSize::DWord), 0)],
                HashMap::from([(1, ir::Type::Num(ir::NumSize::Byte))]),
                vec![ir::Block(vec![], vec![], ir::BlockEnding::Return(None))],
            ),
            &mut 0,
            &mut data_refs,
            &mut FmtDataRefFlags::default(),
            &HashMap::from([("function".to_string(), None)]),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
            },
        );
        assert_eq!(args, vec![0]);
        assert!(!read_ref);

        let return_node = graph.new_node(StatType::new_return(None));
        match_graph(code, return_node);
    }

    #[test]
    fn translate_program_test() {
        let ThreeCode {
            functions: _,
            data_refs,
            graph: program_graph,
            read_ref,
            code,
            int_handler,
        } = (
            ir::Program(
                HashMap::new(),
                HashMap::new(),
                vec![ir::Block(
                    vec![],
                    vec![],
                    ir::BlockEnding::Exit(ir::NumExpr::Const(ir::NumSize::DWord, 0)),
                )],
                HashMap::new(),
                Some("malloc".to_string()),
            ),
            &Options {
                dead_code_removal: false,
                const_propagation: false,
                inlining: Some(1000),
                tail_call: false,
                const_branch: false,
                show_arm_temp_rep: false,
                show_three_code: false,
            },
        )
            .into();
        assert_eq!(data_refs, HashMap::new());
        assert_eq!(int_handler, Some("malloc".to_string()));
        assert!(!read_ref);

        let mut graph = Graph::new();
        let return_node = graph.new_node(StatType::new_return(None));
        let exit_node = graph.new_node(StatType::new_simple(
            StatCode::VoidCall("exit".to_string(), vec![0]),
            return_node,
        ));
        let exit_assign_node = graph.new_node(StatType::new_simple(
            StatCode::Assign(0, OpSrc::Const(0)),
            exit_node,
        ));
        match_graph(code, exit_assign_node);
        mem::drop(program_graph);
    }
}
