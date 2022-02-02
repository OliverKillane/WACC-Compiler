use std::fmt::Display;

enum SummaryType {
    Error,
    Warning,
    Note,
}

struct SummaryComponent<'l> {
    summary_type: SummaryType,
    span: &'l str,
    declaration: Option<&'l str>,
    message: String,
    shorthand: Option<String>,
}

impl<'l> SummaryComponent<'l> {
    fn new(summary_type: SummaryType, span: &'l str, message: String) -> Self {
        Self {
            summary_type,
            span,
            declaration: None,
            message,
            shorthand: None,
        }
    }

    fn add_declaration(&mut self, declaration: &'l str) {
        self.declaration = Some(declaration);
    }

    fn add_shorthand(&mut self, shorthand: String) {
        self.shorthand = Some(shorthand);
    }
}

pub struct SummaryCell<'l> {
    span: &'l str,
    components: Vec<SummaryComponent<'l>>,
}

impl<'l> SummaryCell<'l> {
    fn new(span: &'l str) -> Self {
        SummaryCell {
            span,
            components: Vec::new(),
        }
    }

    fn add_component(&mut self, component: SummaryComponent<'l>) {
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
    fn new(input: &'l str, stage: SummaryStage) -> Self {
        Self {
            input,
            stage,
            cells: Vec::new(),
        }
    }

    fn add_cell(&mut self, cell: SummaryCell<'l>) {
        self.cells.push(cell);
    }
}

impl<'l> Display for Summary<'l> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
