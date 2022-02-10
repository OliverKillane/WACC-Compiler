//! Provides the interface for the error system, including all functions
//! required to build up summary components and summary cells.
//!
//! Builder-like pattern use to construct error messages for printing.

use super::span_utils::get_relative_range;
use std::collections::LinkedList;

/// Summary type for the [error cell component](SummaryComponent).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryType {
    /// An error type for the component.
    Error,
    /// A warning type for the component.
    Warning,
}

/// Error cell component. Represents an expression at which a specific error has
/// occured. Contains the [type of the summary](SummaryType), the internal code
/// of the summary to distinguish between different summaries, a span for the
/// expression at which the error occured, an optional first declaration of the
/// item causing the problem, a full error message displayed above the code
/// presentation, an optional shorthand message that will be displayed within
/// the code presentation and an optional "note" message.
pub struct SummaryComponent<'l> {
    pub(super) summary_type: SummaryType,
    pub(super) summary_code: u32,
    pub(super) span: &'l str,
    pub(super) declaration: Option<&'l str>,
    pub(super) message: String,
    pub(super) shorthand: Option<String>,
    pub(super) note: Option<String>,
}

impl<'l> SummaryComponent<'l> {
    /// Creates a new error component.
    pub fn new(
        summary_type: SummaryType,
        summary_code: u32,
        span: &'l str,
        message: String,
    ) -> Self {
        assert!(!span.is_empty());
        Self {
            summary_type,
            summary_code,
            span,
            declaration: None,
            message,
            shorthand: None,
            note: None,
        }
    }

    /// Sets a declaration of the item causing a problem to be the given span
    /// within the input code.
    pub fn set_declaration(&mut self, declaration: &'l str) {
        assert!(!declaration.is_empty());
        self.declaration = Some(declaration);
    }

    /// Sets a shorthand message for the component to be the given string.
    pub fn set_shorthand(&mut self, shorthand: String) {
        if shorthand.contains('\n') {
            panic!("Shorthand must be one-line only");
        }
        self.shorthand = Some(shorthand);
    }

    /// Sets a "note" message to be the given string.
    pub fn set_note(&mut self, note: String) {
        self.note = Some(note);
    }
}

/// An error cell. Contains the span of the statement containing all the
/// [expressions with errors](SummaryComponent), an optional title for the
/// error in the error cell and the expression components.
pub struct SummaryCell<'l> {
    pub(super) span: &'l str,
    pub(super) title: Option<String>,
    pub(super) components: LinkedList<SummaryComponent<'l>>,
}

impl<'l> SummaryCell<'l> {
    /// Creates a new error cell.
    pub fn new(span: &'l str) -> Self {
        assert!(!span.is_empty());
        SummaryCell {
            span,
            title: None,
            components: LinkedList::new(),
        }
    }

    /// Sets a new title for the error cell.
    pub fn set_title(&mut self, title: String) {
        self.title = Some(title);
    }

    /// Adds a new [expression with error](SummaryComponent) to the error cell.
    pub fn add_component(&mut self, component: SummaryComponent<'l>) {
        #[cfg(debug_assertions)]
        get_relative_range(self.span, component.span).expect("Component span not within the cell");
        self.components.push_back(component);
    }
}

/// Stage at which the error occured. The integer values of the errors signify
/// the error code returned by the binary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryStage {
    /// A parser error.
    Parser = 100,
    /// A semantic analysis error.
    Semantic = 200,
}

/// Full error summary. Contains the filepath to the file on which the error
/// is printed, the input string for the code, the stage of compilation at which
/// the error happened, a list of [error cells](SummaryCell) with the
/// statement-specific errors and an optional separator between the cells.
pub struct Summary<'l> {
    pub(super) filepath: Option<String>,
    pub(super) input: &'l str,
    pub(super) stage: SummaryStage,
    pub(super) cells: Vec<SummaryCell<'l>>,
    pub(super) sep: Option<char>,
}

impl<'l> Summary<'l> {
    /// Creates a new error summary.
    pub fn new(input: &'l str, stage: SummaryStage) -> Self {
        assert!(!input.is_empty());
        Self {
            filepath: None,
            input,
            stage,
            cells: Vec::new(),
            sep: None,
        }
    }

    /// Sets the file path to the origin file of the input
    pub fn set_filepath(&mut self, filepath: String) {
        self.filepath = Some(filepath);
    }

    /// Sets a separator between the cells to the given character.
    pub fn set_sep(&mut self, sep: char) {
        self.sep = Some(sep);
    }

    /// Adds a new error cell.
    pub fn add_cell(&mut self, cell: SummaryCell<'l>) {
        #[cfg(debug_assertions)]
        {
            get_relative_range(self.input, cell.span).expect("Cell span not within the input");
            for component in &cell.components {
                if let Some(declaration) = component.declaration {
                    get_relative_range(self.input, declaration)
                        .expect("Declaration not within the input");
                }
            }
        }
        self.cells.push(cell);
    }
}

#[cfg(test)]
mod test {
    use super::{Summary, SummaryCell, SummaryComponent, SummaryStage, SummaryType};

    use std::panic::{catch_unwind, set_hook, take_hook, UnwindSafe};

    fn catch_panic<F: FnOnce() + UnwindSafe>(f: F) -> String {
        let prev_hook = take_hook();
        set_hook(Box::new(|_info| {}));
        let unwind = catch_unwind(f);
        set_hook(prev_hook);
        unwind
            .err()
            .unwrap()
            .downcast_ref::<String>()
            .unwrap()
            .clone()
    }
    #[test]
    fn test_component_not_within_cell() {
        assert_eq!(
            catch_panic(|| {
                let input = "abcdef";
                let mut cell = SummaryCell::new(&input[1..]);
                cell.add_component(SummaryComponent::new(
                    SummaryType::Error,
                    200,
                    &input[0..2],
                    "message".to_string(),
                ));
            }),
            "Component span not within the cell",
        );
    }

    #[test]
    fn test_cell_not_within_input() {
        assert_eq!(
            catch_panic(|| {
                let input = "abcdef";
                let mut summary = Summary::new(&input[1..], SummaryStage::Parser);
                summary.add_cell(SummaryCell::new(&input[0..2]));
            }),
            "Cell span not within the input"
        )
    }

    #[test]
    fn test_declaraton_not_within_input() {
        assert_eq!(
            catch_panic(|| {
                let input = "abcdef";
                let mut summary = Summary::new(&input[1..], SummaryStage::Parser);
                let mut cell = SummaryCell::new(&input[1..]);
                let mut component =
                    SummaryComponent::new(SummaryType::Error, 200, &input[1..], String::new());
                component.set_declaration(&input[0..]);
                cell.add_component(component);
                summary.add_cell(cell);
            }),
            "Declaration not within the input"
        )
    }
}
