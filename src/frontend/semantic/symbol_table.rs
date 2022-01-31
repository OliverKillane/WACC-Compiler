//! The three required symbol tables for semantic analysis:
//! - Function symbol table containing function identifiers and types
//! - Variable symbol table mapping renamed variables to types
//! - Local symbol table using scope to map strings to renamed identifiers
//!   (used as a key into the global variable table)

use crate::frontend::ast::*;
use crate::frontend::semantic::semantic_errors::*;
use std::collections::HashMap;

/// Function symbol table, holds the globally accessible functions identifier
/// (string references) and the types and names of parameters
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionSymbolTable<'a>(HashMap<&'a str, (Type, Vec<(&'a str, Type)>)>);

impl<'a> FunctionSymbolTable<'a> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Define a function with the type (Return type, [param types]). The if a
    /// function is already in the table it is replaced.
    pub fn def_fun(&mut self, ident: &'a str, fun_type: (Type, Vec<(&'a str, Type)>)) {
        self.0.insert(ident, fun_type);
    }

    /// Get the type of a given function by its identifier.
    pub fn get_fun(&self, ident: &str) -> Option<(Type, Vec<(&'a str, Type)>)> {
        match self.0.get(ident) {
            Some(fun_type) => Some(fun_type.clone()),
            None => None,
        }
    }
}

/// A flat variable symbol table with all variables renamed to integers,
/// and associated with types identifiable through usize integers (same as in
/// the renamed AST).
pub struct VariableSymbolTable(HashMap<usize, Type>);

impl VariableSymbolTable {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Define a variable with a type, identifier, and span of definition within
    /// the scope of the local symbol table.
    pub fn def_var<'a, 'b>(
        &mut self,
        ident: &'a str,
        var_type: Type,
        span: &'a str,
        local: &mut LocalSymbolTable<'a, 'b>,
    ) -> Option<SemanticError<'a>> {
        match local.in_scope(ident) {
            Some(def) => Some(SemanticError::RepeatDefinitionVariable(ident, def)),
            None => {
                let id = self.0.len();
                self.0.insert(id, var_type);
                local.add_var(ident, id, span);
                None
            }
        }
    }

    /// Get the type of a variable from the vantage point of the local scope.
    pub fn get_type<'a, 'b>(
        &self,
        ident: &'a str,
        local: &LocalSymbolTable<'a, 'b>,
    ) -> Option<(usize, Type)> {
        match local.get_var(ident) {
            Some((id, _)) => Some((
                id,
                self.0
                    .get(&id)
                    .expect("Failure: symbol in local table but not global.")
                    .clone(),
            )),
            None => None,
        }
    }
}

/// A chainable local symbol table, used to translate the current (and parent)
/// scope variables to integers for use in the flat variable symbol table and
/// the definition of the variable.
///
/// Used as a key to get variables by their string identifiers from the
/// variable symbol table.
/// ```
/// let mut var_symb = VariableSymbolTable::new();
/// let mut local_symb = LocalSymbolTable::new_root();
///
/// // define in current (local_symb) scope
/// var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb);
/// ```
pub struct LocalSymbolTable<'a, 'b> {
    current: HashMap<&'a str, (usize, &'a str)>,
    parent: Option<&'b Self>,
}

impl<'a, 'b> LocalSymbolTable<'a, 'b> {
    /// Create a new root scope (no parent).
    pub fn new_root() -> Self {
        LocalSymbolTable {
            current: HashMap::new(),
            parent: None,
        }
    }

    /// Create new child scope local symbol table.
    pub fn new_child(parent: &'b LocalSymbolTable<'a, 'b>) -> Self {
        LocalSymbolTable {
            current: HashMap::new(),
            parent: Some(&parent),
        }
    }

    /// Checks if a variable is in the current scope level. If one is, then
    /// the span of the definition of the first is returned.
    pub fn in_scope(&self, ident: &'a str) -> Option<&'a str> {
        match self.current.get(ident) {
            Some((_, def)) => Some(def),
            None => None,
        }
    }

    /// Add a variable to the current scope. Assumes the variable is not already
    /// in the current scope, and panics in that case.
    ///
    /// Should only be called once it has been established the variable is not
    /// yet defined.
    pub fn add_var(&mut self, ident: &'a str, id: usize, span: &'a str) {
        if let Some(_) = self.current.insert(ident, (id, span)) {
            panic!("Repeat add of variable to local symbol table.")
        }
    }

    /// Get the renamed identifier and definition span of a variable.
    /// If the variable is undefined None is returned.
    pub fn get_var(&self, ident: &'a str) -> Option<(usize, &'a str)> {
        let mut symb = self;
        loop {
            if let Some((id, span)) = symb.current.get(ident) {
                break Some((*id, span));
            }

            match symb.parent {
                Some(parent) => symb = parent,
                None => break None,
            }
        }
    }
}

/// Gets the current symbol table and returns:
/// - Symbol table of function identifier to function type
/// - Vector of functions with correct declarations (no repeated name/s
///   for function or parameters), these functions can be further analysed.
/// - Vector of errors from invalid functions.
///
/// The errors considered are:
/// - Function name is repeated, cannot process further semantic error thrown.
///
/// *Note:* the parameter identifiers are not checked, that is done when the
/// function body analysis is done.
fn get_fn_symbols<'a>(
    fn_defs: Vec<WrapSpan<'a, Function<'a, &'a str>>>,
) -> (
    FunctionSymbolTable,
    Vec<Function<'a, &'a str>>,
    SemanticErrorSummary<'a>,
) {
    let mut fun_symb = FunctionSymbolTable::new();
    let mut def_table: HashMap<&str, &str> = HashMap::new();
    let mut valid_fun: Vec<Function<'a, &'a str>> = Vec::new();
    let mut errors: SemanticErrorSummary<'a> = Vec::new();

    for WrapSpan(fn_def_span, Function(ret_type, fn_name, params, stats)) in fn_defs {
        match def_table.insert(fn_name, fn_def_span) {
            Some(orig) => errors.push(WrapSpan(
                fn_def_span,
                SemanticError::RepeatDefinitionFunction(fn_name, orig),
            )),
            None => {
                fun_symb.def_fun(
                    fn_name,
                    (
                        ret_type.clone(),
                        params
                            .iter()
                            .map(|WrapSpan(_, Param(t, name))| (*name, t.clone()))
                            .collect(),
                    ),
                );
                valid_fun.push(Function::<'a>(ret_type, fn_name, params, stats))
            }
        }
    }

    (fun_symb, valid_fun, errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_symb_can_add_and_find_vars_in_current_scope() {
        let mut symb_tab = LocalSymbolTable::new_root();

        symb_tab.add_var("var1", 0, "int var1 = 9");
        symb_tab.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_tab.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_tab.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_tab.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_tab.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_tab.current.len(), 2);
    }

    #[test]
    fn local_symb_can_add_variables_to_child() {
        let symb_parent = LocalSymbolTable::new_root();

        let mut symb_child = LocalSymbolTable::new_child(&symb_parent);

        symb_child.add_var("var2", 1, "bool var2 = true");
        symb_child.add_var("var1", 0, "int var1 = 9");

        assert_eq!(symb_child.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_child.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_child.current.len(), 2);
        assert_eq!(symb_parent.current.len(), 0);
    }

    #[test]
    fn local_symb_can_find_variables_in_parent() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_parent.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_parent.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_parent.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_parent.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_parent.current.len(), 2);

        let symb_child = LocalSymbolTable::new_child(&symb_parent);

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));
    }

    #[test]
    fn local_symb_variables_can_shadow() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        assert_eq!(symb_parent.in_scope("var1"), Some("int var1 = 9"));
        assert_eq!(symb_parent.in_scope("var2"), Some("bool var2 = true"));

        assert_eq!(symb_parent.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_parent.get_var("var2"), Some((1, "bool var2 = true")));

        assert_eq!(symb_parent.current.len(), 2);

        let mut symb_child = LocalSymbolTable {
            current: HashMap::new(),
            parent: Some(&symb_parent),
        };

        assert_eq!(symb_child.get_var("var1"), Some((0, "int var1 = 9")));
        assert_eq!(symb_child.get_var("var2"), Some((1, "bool var2 = true")));

        symb_child.add_var("var1", 2, "char var2 = 'a'");
        symb_child.add_var("var2", 3, "string var1 = \"hello\"");

        assert_eq!(symb_child.get_var("var1"), Some((2, "char var2 = 'a'")));
        assert_eq!(
            symb_child.get_var("var2"),
            Some((3, "string var1 = \"hello\""))
        );
    }

    #[test]
    fn local_symb_sibling_scopes_isolated() {
        let mut symb_parent = LocalSymbolTable::new_root();

        symb_parent.add_var("var1", 0, "int var1 = 9");
        symb_parent.add_var("var2", 1, "bool var2 = true");

        {
            let mut symb_child1 = LocalSymbolTable::new_child(&symb_parent);

            assert_eq!(symb_child1.get_var("var1"), Some((0, "int var1 = 9")));
            assert_eq!(symb_child1.get_var("var2"), Some((1, "bool var2 = true")));
            assert_eq!(symb_child1.get_var("var3"), None);

            assert_eq!(symb_parent.current.len(), 2);

            symb_child1.add_var("var3", 2, "char var3 = 'b'");

            assert_eq!(symb_child1.get_var("var3"), Some((2, "char var3 = 'b'")));
        }

        {
            let symb_child2 = LocalSymbolTable::new_child(&symb_parent);

            assert_eq!(symb_child2.get_var("var1"), Some((0, "int var1 = 9")));
            assert_eq!(symb_child2.get_var("var2"), Some((1, "bool var2 = true")));

            assert_eq!(symb_parent.current.len(), 2);

            assert_eq!(symb_child2.get_var("var3"), None);
        }
    }

    #[test]
    #[should_panic]
    fn local_symb_fails_to_redefine_in_same_scope() {
        let mut symb_tab = LocalSymbolTable::new_root();

        symb_tab.add_var("var1", 0, "int var1 = 9");

        symb_tab.add_var("var1", 1, "bool var2 = true");
    }

    #[test]
    fn var_symb_can_define_variables() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        assert_eq!(local_symb.current.len(), 2);
    }

    #[test]
    fn var_symb_cannot_redefine_variables_in_same_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        assert_eq!(
            var_symb.def_var("var1", Type::Bool, "bool var1 = true", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var1",
                "int var1 = 9"
            ))
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'b'", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var2",
                "char var2 = 'a'"
            ))
        );

        assert_eq!(
            var_symb.get_type("var1", &mut local_symb),
            Some((0, Type::Int))
        );
        assert_eq!(
            var_symb.get_type("var2", &mut local_symb),
            Some((1, Type::Char))
        );

        assert_eq!(local_symb.current.len(), 2);
        assert_eq!(var_symb.0.len(), 2);
    }

    #[test]
    fn var_symb_can_reference_variables_in_parent_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut local_symb),
            None
        );

        let mut symb_child = LocalSymbolTable::new_child(&local_symb);

        assert_eq!(
            var_symb.get_type("var1", &mut symb_child),
            Some((0, Type::Int))
        );
        assert_eq!(
            var_symb.get_type("var2", &mut symb_child),
            Some((1, Type::Char))
        );
    }

    #[test]
    fn var_symb_can_define_variables_in_child_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 9", &mut parent_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var2", Type::Char, "char var2 = 'a'", &mut parent_symb),
            None
        );

        {
            let mut symb_child = LocalSymbolTable::new_child(&parent_symb);

            assert_eq!(
                var_symb.def_var(
                    "var3",
                    Type::String,
                    "string var3 = \"hello world\"",
                    &mut symb_child
                ),
                None
            );
            assert_eq!(
                var_symb.def_var(
                    "var4",
                    Type::Pair(box Type::Int, box Type::Int),
                    "pair(int, int) var3 = null",
                    &mut symb_child
                ),
                None
            );

            assert_eq!(
                var_symb.get_type("var3", &mut symb_child),
                Some((2, Type::String))
            );
            assert_eq!(
                var_symb.get_type("var4", &mut symb_child),
                Some((3, Type::Pair(box Type::Int, box Type::Int)))
            );
        }

        assert_eq!(var_symb.get_type("var3", &mut parent_symb), None);
        assert_eq!(var_symb.get_type("var4", &mut parent_symb), None);
    }

    #[test]
    fn var_symb_can_shadow_variables_in_parent_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut parent_symb),
            None
        );

        let mut child_symb = LocalSymbolTable::new_child(&parent_symb);

        assert_eq!(
            var_symb.def_var("var1", Type::Char, "char var1 = 'a'", &mut child_symb),
            None
        );

        assert_eq!(var_symb.0.len(), 2);
    }

    #[test]
    fn var_symb_sibling_scopes_isolated() {
        let mut var_symb = VariableSymbolTable::new();
        let mut parent_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut parent_symb),
            None
        );

        {
            let mut child_symb = LocalSymbolTable::new_child(&parent_symb);
            assert_eq!(
                var_symb.def_var("var3", Type::Char, "char var3 = 'a'", &mut child_symb),
                None
            );
            assert_eq!(
                var_symb.get_type("var1", &mut child_symb),
                Some((0, Type::Int))
            );
            assert_eq!(
                var_symb.get_type("var3", &mut child_symb),
                Some((1, Type::Char))
            );
            assert_eq!(var_symb.0.len(), 2);
        }

        {
            let mut child_symb = LocalSymbolTable::new_child(&parent_symb);
            assert_eq!(
                var_symb.def_var("var3", Type::Int, "int var1 = 3", &mut child_symb),
                None
            );
            assert_eq!(
                var_symb.get_type("var1", &mut child_symb),
                Some((0, Type::Int))
            );
            assert_eq!(
                var_symb.get_type("var3", &mut child_symb),
                Some((2, Type::Int))
            );
            assert_eq!(var_symb.0.len(), 3);
        }
    }

    #[test]
    fn var_symb_cannot_redefine_in_the_same_scope() {
        let mut var_symb = VariableSymbolTable::new();
        let mut local_symb = LocalSymbolTable::new_root();

        assert_eq!(
            var_symb.def_var("var1", Type::Int, "int var1 = 5", &mut local_symb),
            None
        );
        assert_eq!(
            var_symb.def_var("var1", Type::Char, "char var1 = 'a'", &mut local_symb),
            Some(SemanticError::RepeatDefinitionVariable(
                "var1",
                "int var1 = 5"
            ))
        );
    }

    #[test]
    fn fun_symb_can_define_functions() {
        let mut fun_symb = FunctionSymbolTable::new();

        fun_symb.def_fun(
            "fun1",
            (Type::Int, vec![("a", Type::Int), ("b", Type::Char)]),
        );
        assert_eq!(
            fun_symb.get_fun("fun1"),
            Some((Type::Int, vec![("a", Type::Int), ("b", Type::Char)]))
        );
    }

    #[test]
    fn get_fn_symbols_can_get_valid_function_symbols() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool logical_or(bool x, bool y)",
                Function(
                    Type::Bool,
                    "logical_or",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun("fun1", (Type::Int, vec![("a", Type::Int)]));
        fn_symb_expected.def_fun(
            "example",
            (Type::Char, vec![("a", Type::Int), ("b", Type::String)]),
        );
        fn_symb_expected.def_fun(
            "logical_or",
            (Type::Bool, vec![("x", Type::Bool), ("y", Type::Bool)]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "logical_or",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_detects_function_redefinition() {
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string b)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string b", Param(Type::String, "b")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool fun1(bool x, bool y)",
                Function(
                    Type::Bool,
                    "fun1",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun("fun1", (Type::Int, vec![("a", Type::Int)]));
        fn_symb_expected.def_fun(
            "example",
            (Type::Char, vec![("a", Type::Int), ("b", Type::String)]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string b", Param(Type::String, "b")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![WrapSpan(
            "bool fun1(bool x, bool y)",
            SemanticError::RepeatDefinitionFunction("fun1", "int fun1(int a)"),
        )];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }

    #[test]
    fn get_fn_symbols_ignores_parameter_names() {
        // As noted in get_fn_symbols, parameter naming issues are ignored and checked later.
        let fn_defs = vec![
            WrapSpan(
                "int fun1(int a)",
                Function(
                    Type::Int,
                    "fun1",
                    vec![WrapSpan("int a", Param(Type::Int, "a"))],
                    vec![],
                ),
            ),
            WrapSpan(
                "char example(int a, string begin)",
                Function(
                    Type::Char,
                    "example",
                    vec![
                        WrapSpan("int a", Param(Type::Int, "a")),
                        WrapSpan("string str", Param(Type::String, "str")),
                    ],
                    vec![],
                ),
            ),
            WrapSpan(
                "bool logical_or(bool x, bool y)",
                Function(
                    Type::Bool,
                    "logical_or",
                    vec![
                        WrapSpan("bool x", Param(Type::Bool, "x")),
                        WrapSpan("bool y", Param(Type::Bool, "y")),
                    ],
                    vec![],
                ),
            ),
        ];

        let (fn_symb, valid_fns, errors) = get_fn_symbols(fn_defs);

        let mut fn_symb_expected = FunctionSymbolTable::new();
        fn_symb_expected.def_fun("fun1", (Type::Int, vec![("a", Type::Int)]));
        fn_symb_expected.def_fun(
            "example",
            (Type::Char, vec![("a", Type::Int), ("str", Type::String)]),
        );
        fn_symb_expected.def_fun(
            "logical_or",
            (Type::Bool, vec![("x", Type::Bool), ("y", Type::Bool)]),
        );

        let valid_fns_expected = vec![
            Function(
                Type::Int,
                "fun1",
                vec![WrapSpan("int a", Param(Type::Int, "a"))],
                vec![],
            ),
            Function(
                Type::Char,
                "example",
                vec![
                    WrapSpan("int a", Param(Type::Int, "a")),
                    WrapSpan("string str", Param(Type::String, "str")),
                ],
                vec![],
            ),
            Function(
                Type::Bool,
                "logical_or",
                vec![
                    WrapSpan("bool x", Param(Type::Bool, "x")),
                    WrapSpan("bool y", Param(Type::Bool, "y")),
                ],
                vec![],
            ),
        ];

        let errors_expected: SemanticErrorSummary = vec![];

        assert_eq!(fn_symb_expected, fn_symb);
        assert_eq!(valid_fns_expected, valid_fns);
        assert_eq!(errors_expected, errors);
    }
}
