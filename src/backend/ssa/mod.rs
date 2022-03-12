mod trans_from;
mod trans_into;

use super::three_code as tc;
use crate::graph::{Deleted, Graph, NodeRef};
use crate::intermediate::{DataRef, VarRepr};
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Clone)]
/// Type of the source operand for an operation
pub(super) enum ValSrc {
    /// Constant value
    Const(i32),
    /// Value of a data reference to the static data in the static data vector in [program](ThreeCode)
    DataRef(DataRef, i32),
    /// Variable declared in the given node
    Var(VarNode),
    /// Function argument
    Arg(VarRepr),
    /// From the phi operator
    Phi(PhiNode),
    /// A reference to a special field on the stack just for reading into variables
    ReadRef,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) struct PhiStat(pub(super) Vec<VarNode>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum VarSrc {
    Val(ValSrc),
    BinOp(ValSrc, tc::BinOp, ValSrc),
    Load(VarNode, tc::Size),
    Call(String, Vec<VarNode>),
    Deleted,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum StatCode {
    Var(VarNode),
    Store(VarNode, VarNode, tc::Size),
    Call(String, Vec<VarNode>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum StatType {
    Simple(Option<StatNode>, StatCode, StatNode),
    Phi(Vec<StatNode>, Vec<PhiNode>, StatNode),
    Branch(Option<StatNode>, VarNode, StatNode, StatNode),
    Return(Option<StatNode>, Option<VarNode>),
    Loop(Option<StatNode>),
    Dummy(Option<StatNode>),
}

pub(super) type PhiNode = NodeRef<PhiStat>;
pub(super) type VarNode = NodeRef<VarSrc>;
pub(super) type StatNode = NodeRef<StatType>;

/// Function representation.
pub(super) struct Function {
    /// Variables for function argumetns
    pub args: Vec<VarRepr>,
    /// First statement of the program
    pub code: StatNode,
    /// Whether a [read ref operand source](ValSrc::ReadRef) is used in the function code
    pub read_ref: bool,
}

pub(super) struct SSA {
    /// All functions in the program.
    pub functions: HashMap<String, Function>,
    /// Static data references in the program.
    pub data_refs: HashMap<DataRef, tc::DataRefType>,
    /// Graph of all statement nodes in the program.
    pub stat_graph: Graph<StatType>,
    /// Graph of all variable nodes in the program.
    pub var_graph: Graph<VarSrc>,
    /// Graph of all phi operator nodes in the program.
    pub phi_graph: Graph<PhiStat>,
    /// Whether a [read ref operand source](ValSrc::ReadRef) is used in the main program code.
    pub read_ref: bool,
    /// First statement of the program
    pub code: StatNode,
    /// Function to call as an int overflow/underflow handler for checking for
    /// 32-bit overflows.
    pub int_handler: Option<String>,
}

impl StatType {
    pub(super) fn new_simple(stat_code: StatCode, next_node: StatNode) -> Self {
        Self::Simple(None, stat_code, next_node)
    }

    pub(super) fn new_branch(var: VarNode, true_node: StatNode, false_node: StatNode) -> Self {
        Self::Branch(None, var, true_node, false_node)
    }

    pub(super) fn new_phi(phis: Vec<PhiNode>, next_node: StatNode) -> Self {
        Self::Phi(vec![], phis, next_node)
    }

    pub(super) fn new_return(var: Option<VarNode>) -> Self {
        Self::Return(None, var)
    }

    pub(super) fn new_loop() -> Self {
        Self::Loop(None)
    }

    pub(super) fn incoming(&self) -> Vec<StatNode> {
        match self {
            Self::Simple(Some(incoming_node), _, _)
            | Self::Branch(Some(incoming_node), _, _, _)
            | Self::Return(Some(incoming_node), _)
            | Self::Loop(Some(incoming_node))
            | Self::Dummy(Some(incoming_node)) => {
                vec![incoming_node.clone()]
            }
            Self::Phi(incoming, _, _) => incoming.clone(),
            _ => vec![],
        }
    }

    pub(super) fn set_incoming(&mut self, new_incoming_node: StatNode) {
        match self {
            Self::Simple(incoming_node, _, _)
            | Self::Branch(incoming_node, _, _, _)
            | Self::Return(incoming_node, _)
            | Self::Loop(incoming_node)
            | Self::Dummy(incoming_node) => {
                *incoming_node = Some(new_incoming_node);
            }
            Self::Phi(incoming, _, _) => *incoming = vec![new_incoming_node],
        }
    }

    pub(super) fn successors(&self) -> Vec<StatNode> {
        match self {
            Self::Loop(_) | Self::Return(_, _) | Self::Dummy(_) => vec![],
            Self::Simple(_, _, next_node) | Self::Phi(_, _, next_node) => {
                vec![next_node.clone()]
            }
            Self::Branch(_, _, true_node, false_node) => {
                vec![true_node.clone(), false_node.clone()]
            }
        }
    }

    pub(super) fn substitute_child(&mut self, old_child: &StatNode, new_child: &StatNode) {
        match self {
            Self::Simple(_, _, next_node) | Self::Phi(_, _, next_node) => {
                if &*next_node == old_child {
                    *next_node = new_child.clone()
                }
            }
            Self::Branch(_, _, true_node, false_node) => {
                if &*true_node == old_child {
                    *true_node = new_child.clone()
                }
                if &*false_node == old_child {
                    *false_node = new_child.clone()
                }
            }
            _ => {}
        };
    }
}

impl Deleted for StatType {
    fn deleted() -> Self {
        StatType::Dummy(None)
    }
}

impl Deleted for VarSrc {
    fn deleted() -> Self {
        VarSrc::Deleted
    }
}

impl Deleted for PhiStat {
    fn deleted() -> Self {
        PhiStat(vec![])
    }
}

impl SSA {
    /// Helper function that validates most of the runtime assertions needed for SSA
    fn validate(&self) -> Result<(), ()> {
        let SSA {
            functions,
            data_refs: _,
            stat_graph,
            var_graph,
            phi_graph,
            read_ref: _,
            code: _,
            int_handler,
        } = self;
        if let Some(int_handler) = int_handler.as_ref() && !functions.contains_key(int_handler) {
            return Err(());
        }
        let mut set_phis = HashSet::new();
        let mut set_vars = HashSet::new();
        for node in stat_graph {
            match &*node.get() {
                StatType::Phi(incoming, phis, _) => {
                    for phi in phis {
                        if phi.get().0.len() != incoming.len() {
                            return Err(());
                        }
                        set_phis.insert(phi.clone());
                    }
                }
                StatType::Simple(_, StatCode::Var(var), _) => {
                    set_vars.insert(var.clone());
                }
                StatType::Dummy(_) => return Err(()),
                _ => {}
            }
        }
        for var in var_graph {
            if !set_vars.contains(&var) {
                return Err(());
            }
            if let VarSrc::Deleted = &*var.get() {
                return Err(());
            }
        }
        for phi in phi_graph {
            if !set_phis.contains(&phi) {
                return Err(());
            }
        }
        Ok(())
    }
}
