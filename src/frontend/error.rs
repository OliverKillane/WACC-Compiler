use std::fmt::Display;

pub enum SummaryType {
    Error,
    Warning,
}

pub struct SummaryComponent<'l> {
    summary_type: SummaryType,
    span: &'l str,
    declaration: Option<&'l str>,
    message: String,
    shorthand: Option<String>,
    note: Option<String>,
}

impl<'l> SummaryComponent<'l> {
    pub fn new(summary_type: SummaryType, span: &'l str, message: String) -> Self {
        Self {
            summary_type,
            span,
            declaration: None,
            message,
            shorthand: None,
            note: None,
        }
    }

    pub fn add_declaration(&mut self, declaration: &'l str) {
        self.declaration = Some(declaration);
    }

    pub fn add_shorthand(&mut self, shorthand: String) {
        self.shorthand = Some(shorthand);
    }

    pub fn add_note(&mut self, note: String) {
        self.note = Some(note);
    }
}

pub struct SummaryCell<'l> {
    span: &'l str,
    components: Vec<SummaryComponent<'l>>,
}

impl<'l> SummaryCell<'l> {
    pub fn new(span: &'l str) -> Self {
        SummaryCell {
            span,
            components: Vec::new(),
        }
    }

    pub fn add_component(&mut self, component: SummaryComponent<'l>) {
        self.components.push(component);
    }
}

pub enum SummaryStage {
    Parser = 100,
    Semantic = 200,
}
pub struct Summary<'l> {
    input: &'l str,
    stage: SummaryStage,
    cells: Vec<SummaryCell<'l>>,
}

impl<'l> Summary<'l> {
    pub fn new(input: &'l str, stage: SummaryStage) -> Self {
        Self {
            input,
            stage,
            cells: Vec::new(),
        }
    }

    pub fn add_cell(&mut self, cell: SummaryCell<'l>) {
        self.cells.push(cell);
    }
}

impl<'l> Display for Summary<'l> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
