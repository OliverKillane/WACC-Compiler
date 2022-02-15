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
#[derive(Debug)]
pub struct SummaryComponent<'l> {
    /// Component type. Shows up next to the main error message. Determines the
    /// color of the component.
    pub(super) summary_type: SummaryType,
    /// Component code. Used for differentiation of different component types.
    /// Shows up in square brackets next to the summary type.
    pub(super) summary_code: u32,
    /// Component span. Determines the part that is underlined in a summary cell.
    /// Must be within the bounds of the input of the whole summary. Must not
    /// overlap with other spans within the cell. Must not overlap with other
    /// declarations within the [cell](SummaryCell).
    pub(super) span: &'l str,
    /// Component declaration. Displays an optional message "First declared here"
    /// within the [cell](SummaryCell), colored on a different color.
    pub(super) declaration: Option<&'l str>,
    /// Main component message. Shows up above the code presentation.
    pub(super) message: String,
    /// Shordhand message. Shows up optionally under the arrow leading from the
    /// underlined span. If not provided, only the component number will be
    /// shown.
    pub(super) shorthand: Option<String>,
    /// Note message. Shows up optionally below the code presentation. Used to
    /// inform the user of some important things to remember.
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
    pub fn set_declaration(mut self, declaration: &'l str) -> Self {
        assert!(!declaration.is_empty());
        self.declaration = Some(declaration);
        self
    }

    /// Sets a shorthand message for the component to be the given string.
    pub fn set_shorthand(mut self, shorthand: String) -> Self {
        if shorthand.contains('\n') {
            panic!("Shorthand must be one-line only");
        }
        self.shorthand = Some(shorthand);
        self
    }

    /// Sets a "note" message to be the given string.
    pub fn set_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }
}

/// An error cell. Contains the span of the statement containing all the
/// [expressions with errors](SummaryComponent), an optional title for the
/// error in the error cell and the expression components.
#[derive(Debug)]
pub struct SummaryCell<'l> {
    /// The span of the entire cell. The parts of the line in the code presentation
    /// that are not the span will be dimmed out. Determines the location of the
    /// cell displayed at the top of it.
    pub(super) span: &'l str,
    /// The title of the cell. Displays an optional message next to the location
    /// of the cell.
    pub(super) title: Option<String>,
    /// List of all cell [components](SummaryComponent).
    pub(super) components: LinkedList<SummaryComponent<'l>>,
}

impl<'l> SummaryCell<'l> {
    /// Creates a new error cell. Sets the span to the provided span.
    pub fn new(span: &'l str) -> Self {
        assert!(!span.is_empty());
        SummaryCell {
            span,
            title: None,
            components: LinkedList::new(),
        }
    }

    /// Sets a new title for the error cell.
    pub fn set_title(&mut self, title: String) -> &mut Self {
        self.title = Some(title);
        self
    }

    /// Adds a new [expression with error](SummaryComponent) to the error cell.
    pub fn add_component(&mut self, component: SummaryComponent<'l>) -> &mut Self {
        #[cfg(debug_assertions)]
        get_relative_range(self.span, component.span).expect("Component span not within the cell");
        self.components.push_back(component);
        self
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
#[derive(Debug)]
pub struct Summary<'l> {
    /// Input filepath. Optionally displayed before the input cell location.
    pub(super) filepath: Option<String>,
    /// The text of the entire code.
    pub(super) input: &'l str,
    /// Summary stage. Displayed at the top of the summary. Determines the code
    /// of the summary.
    pub(super) stage: SummaryStage,
    /// A list of all [summary cells](SummaryCell).
    pub(super) cells: Vec<SummaryCell<'l>>,
    /// An optional separator between the summary cells.
    pub(super) sep: Option<char>,
}

impl<'l> Summary<'l> {
    /// Creates a new error summary. Sets the input and stage of the summary to
    /// the values provided.
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
    pub fn set_filepath(&mut self, filepath: String) -> &mut Self {
        self.filepath = Some(filepath);
        self
    }

    /// Sets a separator between the cells to the given character.
    pub fn set_sep(&mut self, sep: char) -> &mut Self {
        self.sep = Some(sep);
        self
    }

    /// Adds a new error cell.
    pub fn add_cell(&mut self, cell: SummaryCell<'l>) -> &mut Self {
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
        self
    }

    pub fn get_code(&self) -> i32 {
        self.stage as i32
    }

    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }
}

#[cfg(test)]
mod test {
    use super::{Summary, SummaryCell, SummaryComponent, SummaryStage, SummaryType};

    #[test]
    #[should_panic(expected = "Component span not within the cell")]
    fn test_component_not_within_cell() {
        let input = "abcdef";
        let mut cell = SummaryCell::new(&input[1..]);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[0..2],
            "message".to_string(),
        ));
    }

    #[test]
    #[should_panic(expected = "Cell span not within the input")]
    fn test_cell_not_within_input() {
        let input = "abcdef";
        let mut summary = Summary::new(&input[1..], SummaryStage::Parser);
        summary.add_cell(SummaryCell::new(&input[0..2]));
    }

    #[test]
    #[should_panic(expected = "Declaration not within the input")]
    fn test_declaraton_not_within_input() {
        let input = "abcdef";
        let mut summary = Summary::new(&input[1..], SummaryStage::Parser);
        let mut cell = SummaryCell::new(&input[1..]);
        cell.add_component(
            SummaryComponent::new(SummaryType::Error, 200, &input[1..], String::new())
                .set_declaration(&input[0..]),
        );
        summary.add_cell(cell);
    }
}
