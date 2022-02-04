use std::collections::LinkedList;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryType {
    Error,
    Warning,
}

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

    pub fn set_declaration(&mut self, declaration: &'l str) {
        self.declaration = Some(declaration);
    }

    pub fn set_shorthand(&mut self, shorthand: String) {
        self.shorthand = Some(shorthand);
    }

    pub fn set_note(&mut self, note: String) {
        self.note = Some(note);
    }
}

pub struct SummaryCell<'l> {
    pub(super) span: &'l str,
    pub(super) components: LinkedList<SummaryComponent<'l>>,
}

impl<'l> SummaryCell<'l> {
    pub fn new(span: &'l str) -> Self {
        SummaryCell {
            span,
            components: LinkedList::new(),
        }
    }

    pub fn add_component(&mut self, component: SummaryComponent<'l>) {
        self.components.push_back(component);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryStage {
    Parser = 100,
    Semantic = 200,
}
pub struct Summary<'l> {
    pub(super) filepath: &'l str,
    pub(super) input: &'l str,
    pub(super) stage: SummaryStage,
    pub(super) cells: Vec<SummaryCell<'l>>,
    pub(super) sep: Option<char>,
}

impl<'l> Summary<'l> {
    pub fn new(filepath: &'l str, input: &'l str, stage: SummaryStage) -> Self {
        Self {
            filepath,
            input,
            stage,
            cells: Vec::new(),
            sep: None,
        }
    }

    pub fn set_sep(&mut self, sep: char) {
        self.sep = Some(sep);
    }

    pub fn add_cell(&mut self, cell: SummaryCell<'l>) {
        self.cells.push(cell);
    }
}
