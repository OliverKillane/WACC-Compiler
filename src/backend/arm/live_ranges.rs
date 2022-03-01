//! Creates a graph of live ranges for every node in the control flow graph of
//! the arm representation. These are then used for register allocation.

use std::{cmp::min, collections::HashMap};

use lazy_static::__Deref;
use linked_hash_set::LinkedHashSet;

use super::arm_repr::{
    ArmNode, ControlFlow, FlexOffset, FlexOperand, Ident, MemOp, MemOperand, Program, Stat,
    Subroutine, Temporary,
};

/// The identifier type for live range sets in the LiveRanges structure.
type SetId = usize;

/// Struct containing the live ranges for all nodes in the control flow graph.
/// In order to conserve memory & speed up, sets can be shared (liveout of a node
/// is live-in of another).
pub struct LiveRanges {
    /// A map of set identifiers to Maps from Temporary Identifiers to
    /// Instructions till next use. Effectively a live set with associated
    /// usage data.
    live_set_map: HashMap<SetId, HashMap<Temporary, usize>>,

    /// A map from each node to its (live-in set, live-out set, definitions,
    /// uses, and any contributing live sets). As we share live-in and live-out
    /// sets for adjacent instructions, the only type of node with contributors
    /// are branches (where the live-ins of the branches are distinct from the
    /// live-out of the branch instruction).
    #[allow(clippy::type_complexity)]
    arm_node_map: HashMap<ArmNode, (SetId, SetId, Vec<Temporary>, Vec<Temporary>, Vec<SetId>)>,
}

impl LiveRanges {
    /// Create a new empty Live Ranges struct
    pub fn new() -> Self {
        Self {
            live_set_map: HashMap::new(),
            arm_node_map: HashMap::new(),
        }
    }

    /// Given an optional predecessor, if the predecessor is in the live ranges
    /// structure, return its live-out set identifier, otherwise create a new one.
    /// Note: For branches the liveout cannot be used, so we always create a
    /// new set, and update the branch's contributing sets.
    fn generate_live_in(&mut self, prec: &Option<ArmNode>) -> SetId {
        if let Some(node) = prec {
            if matches!(node.get().deref(), ControlFlow::Branch(_, _, _, _)) {
                // if the precursor was a branch, then create our own
                //set, and add it to the branch node's contributions
                let set_id = self.get_new_set();
                if let Some((_, _, _, _, contribs)) = self.arm_node_map.get_mut(node) {
                    contribs.push(set_id)
                }
                set_id
            } else {
                // Precursor is not a branch, if it exists, use its
                // live out set, else just create our own.
                match self.get_set_ids(node) {
                    Some((_, liveout)) => liveout,
                    None => self.get_new_set(),
                }
            }
        } else {
            self.get_new_set()
        }
    }

    /// If the successor has a live_in set, the use this as the live_out. If
    /// the successor is a multi, we can check all non-branch control flows
    /// going to that multi.
    fn generate_live_out(&mut self, succ_node: &ArmNode) -> Option<SetId> {
        if let Some((livein, _, _, _, _)) = self.arm_node_map.get(succ_node) {
            return Some(*livein);
        } else if let ControlFlow::Multi(precs, _) = succ_node.get().deref() {
            for prec in precs {
                if let Some((_, liveout, _, _, _)) = self.arm_node_map.get(prec) && !matches!(prec.get().deref(), ControlFlow::Branch(_,_,_,_)) {
                    return Some(*liveout);
                }
            }
        }
        None
    }

    /// Create an entry for a node, if an old entry exists, it is replaced.
    fn create_entry(&mut self, node: &ArmNode) {
        // Get the uses and definitions
        let mut uses = Vec::new();
        let mut defs = Vec::new();
        node.get_defs_and_uses(&mut defs, &mut uses);

        // Get the live_out set of a simple node, if no successor livein can be
        // used, use a new set.
        let simple_live_out = |succ: &Option<ArmNode>, live: &mut Self| match succ {
            Some(node) => match live.generate_live_out(node) {
                Some(setid) => setid,
                None => live.get_new_set(),
            },
            None => live.get_new_set(),
        };

        let sets = match node.get().deref() {
            ControlFlow::Simple(prec, _, succ) => {
                // If a live out set exists, use it, else create a new one.
                Some((
                    self.generate_live_in(prec),
                    simple_live_out(succ, self),
                    Vec::new(),
                ))
            }
            ControlFlow::Branch(prec, true_succ, _, false_succ) => {
                // A branch has a livein and liveout, as well as contributing sets from its (potentially) two branches.
                let mut contribs = Vec::new();

                if let Some(set_id) = self.generate_live_out(true_succ) {
                    contribs.push(set_id)
                }

                if let Some(node) = false_succ {
                    if let Some(set_id) = self.generate_live_out(node) {
                        contribs.push(set_id)
                    }
                }

                Some((self.generate_live_in(prec), self.get_new_set(), contribs))
            }
            ControlFlow::Return(prec, _) => {
                Some((self.generate_live_in(prec), self.get_new_set(), Vec::new()))
            }
            ControlFlow::Multi(precs, succ) => {
                let prev_set = precs.iter().filter_map(|node| {
                    if let Some((_, liveout, _, _, _)) = self.arm_node_map.get(node) && !matches!(node.get().deref(), ControlFlow::Branch(_,_,_,_)) {
                        Some(*liveout)
                    } else {None}
                }).last();

                let livein_set_id = if let Some(set_id) = prev_set {
                    set_id
                } else {
                    self.get_new_set()
                };
                precs
                    .iter()
                    .filter(|node| matches!(node.get().deref(), &ControlFlow::Branch(_, _, _, _)))
                    .for_each(|branch_node| self.add_contributor(branch_node, livein_set_id));

                Some((livein_set_id, simple_live_out(succ, self), Vec::new()))
            }

            // There are no entries for these types of control flow
            ControlFlow::Removed => None,
            ControlFlow::Ltorg(_) => None,
        };

        if let Some((livein, liveout, contrib)) = sets {
            self.arm_node_map
                .insert(node.clone(), (livein, liveout, defs, uses, contrib));
        }
    }

    /// get the 'livein' and 'liveout; set ids of a given node.
    fn get_set_ids(&self, node: &ArmNode) -> Option<(SetId, SetId)> {
        self.arm_node_map
            .get(node)
            .map(|(livein, liveout, _, _, _)| (*livein, *liveout))
    }

    /// Get a new live ranges set.
    fn get_new_set(&mut self) -> SetId {
        let new_id = self.live_set_map.len();
        self.live_set_map.insert(new_id, HashMap::new());
        new_id
    }

    /// If a node exists in the liveranges structure, add a contributor set
    /// identifier to it.
    fn add_contributor(&mut self, node: &ArmNode, set_id: SetId) {
        if let Some((_, _, _, _, contribs)) = self.arm_node_map.get_mut(node) {
            contribs.push(set_id)
        }
    }

    /// Recalculate the live ranges based on the live in, live out and
    /// contributing sets. If any changes are made, returns true. If the node
    /// is not in the liveranges struct, no changes are made.
    fn update_live_ranges(&mut self, node: &ArmNode) -> bool {
        if let Some((livein, liveout, defs, uses, contribs)) = self.arm_node_map.get(node) {
            let mut changes = false;

            // Create a temporary map for generating the live-in, live-out sets.
            // Note that this avoids a large number of shared mutable reference
            // issues (all sets are contained in the same live_set_map)
            let mut temp_map = HashMap::new();

            // get the live outs from all contributors, note as only branches
            // have contributors, only branches use this step. Otherwise it has
            // already been updated as it is also the successor node's live-in set.
            for contrib_set in contribs.iter().map(|set_id| {
                self.live_set_map
                    .get(set_id)
                    .expect("All contributions are valid set ids")
            }) {
                for (temp, use_distance) in contrib_set {
                    temp_map
                        .entry(*temp)
                        .and_modify(|v| *v = min(*v, *use_distance))
                        .or_insert(*use_distance);
                }
            }

            // Compare and update the live_out set.
            let live_out = self
                .live_set_map
                .get_mut(liveout)
                .expect("Liveout idents are always valid");
            for (temp, use_distance) in temp_map.iter() {
                match live_out.insert(*temp, *use_distance) {
                    Some(old_use_distance) => {
                        changes = changes || old_use_distance != *use_distance
                    }
                    None => changes = true,
                }
            }

            // Remove definitions from the temporary map.
            for def in defs {
                temp_map.remove(def);
            }

            // Insert the live-out - definitions (temp_map) into the live_in set.
            let live_in = self
                .live_set_map
                .get_mut(livein)
                .expect("Liveout idents are always valid");
            for (temp, use_distance) in temp_map.into_iter() {
                match live_in.insert(temp, use_distance + 1) {
                    Some(old_use_distance) => {
                        changes = changes || old_use_distance == use_distance + 1
                    }
                    None => changes = true,
                }
            }

            // Insert the uses into the live in set, it should already be there
            // with distance zero.
            for temp in uses {
                match live_in.insert(*temp, 0) {
                    Some(0) => (),
                    Some(_) | None => changes = true,
                }
            }

            changes
        } else {
            false
        }
    }
}

/// Get a set of all nodes connected to a start_node, continuing until reaching
/// nodes already added to the set. As this DFS goes only to successor sets, the
/// order of the linked set can be considered a topological sort of the arm nodes.
fn get_ordered_node_set(start_node: ArmNode, nodes: &mut LinkedHashSet<ArmNode>) {
    let mut current_node = start_node;
    while nodes.insert(current_node.clone()) {
        let next = match current_node.get().deref() {
            ControlFlow::Simple(_, _, succ) | ControlFlow::Multi(_, succ) => succ.clone(),
            ControlFlow::Branch(_, succ_true, _, succ_false) => {
                get_ordered_node_set(succ_true.clone(), nodes);
                succ_false.clone()
            }
            _ => None,
        };

        match next {
            Some(next_node) => current_node = next_node,
            None => break,
        }
    }
}

/// Generate the live ranges for the main, and all arm nodes in all functions of
/// a program.
pub fn get_live_ranges(
    Program {
        data,
        reserved_stack,
        main,
        functions,
        cfg,
    }: &Program,
) -> LiveRanges {
    let mut live_ranges = LiveRanges::new();
    traverse_live_ranges(main.clone(), &mut live_ranges);
    for Subroutine {
        args,
        start_node,
        reserved_stack,
    } in functions.values()
    {
        traverse_live_ranges(start_node.clone(), &mut live_ranges);
    }
    live_ranges
}

/// Generate all the live ranges for nodes reachable by a traversal from the
/// start_node. Hence this can be used with the start node of any routine to get
/// the live ranges of all instructions.
fn traverse_live_ranges(start_node: ArmNode, liveranges: &mut LiveRanges) {
    // traverse all nodes from the start, build up a vector of the nodes. in the reverse order of the control flow.
    let mut armnodes = LinkedHashSet::new();
    get_ordered_node_set(start_node, &mut armnodes);

    // Reverse order is used as live range information flows from successors to
    // predecessors.
    let armnodes = armnodes.into_iter().rev().collect::<Vec<_>>();

    for node in armnodes.iter() {
        liveranges.create_entry(node);
    }

    // Update the ranges until there are no changes
    while armnodes
        .iter()
        .any(|node| liveranges.update_live_ranges(node))
    {}
}

/// Trait used to enforce all components of the arm representation can have temporary definitions and uses found.
trait DefsUses {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>);
}

impl DefsUses for FlexOperand {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        if let FlexOperand::ShiftReg(Ident::Temp(t), _) = self {
            uses.push(*t)
        }
    }
}

impl DefsUses for MemOperand {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        match self {
            MemOperand::Zero(Ident::Temp(t))
            | MemOperand::PreIndex(Ident::Temp(t), _, _)
            | MemOperand::PostIndex(Ident::Temp(t), _) => uses.push(*t),
            _ => (),
        }
    }
}

impl DefsUses for FlexOffset {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        if let FlexOffset::ShiftReg(_, Ident::Temp(t), _) = self {
            uses.push(*t)
        }
    }
}

impl DefsUses for Stat {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        let add_temps = |id: &Ident, vec: &mut Vec<Temporary>| {
            if let Ident::Temp(tid) = id {
                vec.push(*tid)
            }
        };
        match self {
            Stat::ApplyOp(_, _, _, dst, arg1, flexop) => {
                add_temps(dst, defs);
                add_temps(arg1, uses);
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::Mul(_, _, dst, arg1, arg2) => {
                add_temps(dst, defs);
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::MulA(_, _, dsthi, dstlo, arg1, arg2) => {
                add_temps(dsthi, defs);
                add_temps(dstlo, defs);
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::MulOp(_, _, _, dsthi, dstlo, arg1, arg2) => {
                add_temps(dsthi, defs);
                add_temps(dstlo, defs);
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::Move(_, _, _, dst, flexop) => {
                add_temps(dst, defs);
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::Cmp(_, _, dst, flexop) => {
                add_temps(dst, defs);
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::SatOp(_, _, dst, arg1, arg2) => {
                add_temps(dst, defs);
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::ReadCPSR(dst) => add_temps(dst, defs),
            Stat::MemOp(op, _, _, ident, _) => add_temps(
                ident,
                match op {
                    MemOp::Ldr => defs,
                    MemOp::Str => uses,
                },
            ),
            Stat::Push(_, idents) => idents.iter().for_each(|arg| add_temps(arg, uses)),
            Stat::Pop(_, idents) => idents.iter().for_each(|arg| add_temps(arg, defs)),
            Stat::Link(_, _) => todo!(),
            Stat::Call(_, _, dst, args) => {
                if let Some(Ident::Temp(dst)) = dst {
                    defs.push(*dst)
                }
                args.iter().for_each(|arg| add_temps(arg, uses))
            }
            Stat::AssignStackWord(Ident::Temp(dst)) => defs.push(*dst),
            _ => (),
        }
    }
}

impl DefsUses for ControlFlow {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        match self {
            ControlFlow::Simple(_, stat, _) => stat.get_defs_and_uses(defs, uses),
            ControlFlow::Return(_, Some(Ident::Temp(arg))) => uses.push(*arg),
            _ => (),
        }
    }
}

impl DefsUses for ArmNode {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        self.get().deref().get_defs_and_uses(defs, uses)
    }
}
