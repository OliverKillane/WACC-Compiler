use atomic_refcell::{AtomicRef, AtomicRefCell, AtomicRefMut};
use std::{
    collections::HashSet,
    fmt::Debug,
    hash::Hash,
    mem,
    sync::Arc,
};

/// Node graph. Houses all the [nodes](NodeRef). [Deletes](Deleted) nodes only when they
/// get deleted explicitly or when the graph goes out of scope (gets dropped).
#[derive(Debug, Clone)]
pub struct Graph<T: Deleted>(HashSet<NodeRef<T>>);

/// A node reference. Represents a node in the graph.
pub struct NodeRef<T: Deleted>(Arc<AtomicRefCell<T>>);

pub struct EdgeRef<T: Deleted>(NodeRef<T>, NodeRef<T>);

/// A trait used in the [graph](Graph) to delete nodes whenever they become obsolete.
pub trait Deleted {
    fn deleted() -> Self;
}

impl<T: Deleted> Graph<T> {
    /// Creates a new empty graph.
    pub fn new() -> Self {
        Graph(HashSet::new())
    }

    /// Creates a new node reference wth the given node contents inside.
    pub fn new_node(&mut self, node: T) -> NodeRef<T> {
        let node_ref = NodeRef(Arc::new(AtomicRefCell::new(node)));
        self.0.insert(node_ref.clone());
        node_ref
    }

    /// [Deletes](Deleted) the specific [node](NodeRef).
    pub fn remove_node(&mut self, node_ref: NodeRef<T>) {
        self.0.remove(&node_ref);
        node_ref.delete();
    }

    /// Iterates over all the [nodes](NodeRef) in the graph.
    pub fn iter(&self) -> impl Iterator<Item = NodeRef<T>> {
        self.into_iter()
    }
}

impl<T: Deleted> Default for Graph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Deleted> Drop for Graph<T> {
    fn drop(&mut self) {
        self.0.iter().for_each(NodeRef::delete);
    }
}

impl<T: Deleted> IntoIterator for &Graph<T> {
    type Item = NodeRef<T>;
    type IntoIter = <HashSet<NodeRef<T>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.clone().into_iter()
    }
}

impl<T: Deleted> NodeRef<T> {
    /// Sets the value of the node to a new value and returns the old value
    pub fn set(&self, mut data: T) -> T {
        mem::swap(&mut data, &mut self.0.borrow_mut());
        data
    }

    /// Gets a [ref cell](RefCell) reference to the node.
    pub fn get(&self) -> AtomicRef<'_, T> {
        self.0.borrow()
    }

    /// Gets a mutable [ref cell](RefCell) reference to the node.
    pub fn get_mut(&self) -> AtomicRefMut<'_, T> {
        self.0.borrow_mut()
    }

    /// [Deletes](Deleted) a node.
    fn delete(&self) {
        *self.0.borrow_mut() = T::deleted()
    }
}

impl<T: Deleted + Debug> Debug for NodeRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeRef{{{:?}}}", self.0.as_ptr())
    }
}

impl<T: Deleted> Clone for NodeRef<T> {
    fn clone(&self) -> Self {
        NodeRef(self.0.clone())
    }
}

impl<T: Deleted> Hash for NodeRef<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.as_ptr() as usize);
    }
}

impl<T: Deleted> PartialEq for NodeRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl<T: Deleted> Eq for NodeRef<T> {}

mod test {
    use super::*;

    struct Callback<'l>(
        Arc<AtomicRefCell<dyn FnMut()>>,
        Option<NodeRef<Callback<'l>>>,
    );
    impl<'l> Deleted for Callback<'l> {
        fn deleted() -> Self {
            Callback(Arc::new(AtomicRefCell::new(|| {})), None)
        }
    }
    impl<'l> Drop for Callback<'l> {
        fn drop(&mut self) {
            let mut tmp_fn: Arc<AtomicRefCell<dyn FnMut() + '_>> =
                Arc::new(AtomicRefCell::new(|| {}));
            mem::swap(&mut self.0, &mut tmp_fn);
            tmp_fn.borrow_mut()();
        }
    }

    #[test]
    fn single_removed() {
        let c = Arc::new(AtomicRefCell::new(0));
        let c_closure = c.clone();
        let c_add = Arc::new(AtomicRefCell::new(move || *c_closure.borrow_mut() += 1));
        {
            let mut g = Graph::new();
            let _a = g.new_node(Callback(c_add.clone(), None));
        }
        assert_eq!(*c.borrow_mut(), 1);
    }

    #[test]
    fn cycle_removed() {
        let count = Arc::new(AtomicRefCell::new(0));
        let count_closure = count.clone();
        let count_add: Arc<AtomicRefCell<dyn FnMut() + '_>> =
            Arc::new(AtomicRefCell::new(move || *count_closure.borrow_mut() += 1));
        {
            let mut g = Graph::new();
            let a = g.new_node(Callback(count_add.clone(), None));
            let b = g.new_node(Callback(count_add.clone(), None));
            let c = g.new_node(Callback(count_add.clone(), None));
            a.get_mut().1 = Some(b.clone());
            b.get_mut().1 = Some(c.clone());
            c.get_mut().1 = Some(a.clone());
        }
        assert_eq!(*count.borrow_mut(), 3);
    }

    #[test]
    fn node_removed() {
        let count = Arc::new(AtomicRefCell::new(0));
        let count_closure = count.clone();
        let count_add: Arc<AtomicRefCell<dyn FnMut() + '_>> =
            Arc::new(AtomicRefCell::new(move || *count_closure.borrow_mut() += 1));
        {
            let mut g = Graph::new();
            let a = g.new_node(Callback(count_add.clone(), None));
            let b = g.new_node(Callback(count_add.clone(), None));
            let c = g.new_node(Callback(count_add.clone(), None));
            a.get_mut().1 = Some(b.clone());
            b.get_mut().1 = Some(c.clone());
            c.get_mut().1 = Some(a.clone());
            g.remove_node(a);
            assert_eq!(*count.borrow_mut(), 1);
        }
        assert_eq!(*count.borrow_mut(), 3);
    }
}
