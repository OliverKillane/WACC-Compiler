use colored::{Color, Colorize};
use hyphenation::{Language, Load, Standard};
use std::{
    cmp::min,
    collections::{HashMap, LinkedList},
    fmt::{Display, Write},
};
use textwrap::{fill, Options};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryType {
    Error,
    Warning,
}

impl Into<Color> for SummaryType {
    fn into(self) -> Color {
        match self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
        }
    }
}

impl ToString for SummaryType {
    fn to_string(&self) -> String {
        match self {
            Self::Error => String::from("error"),
            Self::Warning => String::from("warning"),
        }
    }
}

pub struct SummaryComponent<'l> {
    summary_type: SummaryType,
    summary_code: &'static str,
    span: &'l str,
    declaration: Option<&'l str>,
    message: String,
    shorthand: Option<String>,
    note: Option<String>,
}

impl<'l> SummaryComponent<'l> {
    pub fn new(
        summary_type: SummaryType,
        summary_code: &'static str,
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
    span: &'l str,
    components: Vec<SummaryComponent<'l>>,
}

struct SummaryRef<'l> {
    span: &'l str,
    text: String,
    color: Color,
}

fn get_relative_range(input: &str, span: &str) -> Option<(usize, usize)> {
    let input_begin = input.as_ptr() as usize;
    let input_end = input_begin + input.len();

    let span_begin = span.as_ptr() as usize;
    let span_end = span_begin + span.len();

    if input_begin > span_begin || input_end <= span_begin {
        None
    } else {
        Some((
            span_begin - input_begin,
            min(span_end, input_end) - input_begin,
        ))
    }
}

fn get_coords(
    input: &str,
    input_lines: &Vec<&str>,
    input_lines_positions: &Vec<usize>,
    span: &str,
) -> Option<(usize, usize)> {
    let (input_pos, _) = get_relative_range(input, span)?;
    let line_num = input_lines_positions
        .binary_search(&input_pos)
        .unwrap_or_else(|idx| idx - 1);
    assert!(
        line_num < input_lines_positions.len(),
        "span before the first input line position"
    );
    let (line_pos, _) = get_relative_range(input_lines[line_num], span)?;
    Some((line_num, line_pos))
}

static DECLARED_COLOR: Color = Color::BrightBlue;
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

    fn messages(&self) -> LinkedList<String> {
        self.components
            .iter()
            .map(|component| {
                format!(
                    "{}: {}",
                    format!(
                        "{}[{}]",
                        component.summary_type.to_string(),
                        component.summary_code
                    )
                    .color(component.summary_type)
                    .bold(),
                    &component.message
                )
            })
            .collect()
    }

    fn refs(&self) -> Vec<SummaryRef<'l>> {
        let declarations = self
            .components
            .iter()
            .enumerate()
            .filter_map(|(index, component)| {
                component.declaration.map(|declaration| SummaryRef {
                    span: declaration,
                    text: format!("[{}] first declared here", index),
                    color: DECLARED_COLOR,
                })
            });
        let shorthands = self
            .components
            .iter()
            .enumerate()
            .map(|(index, component)| SummaryRef {
                span: component.span,
                text: format!(
                    "[{}] {}",
                    index + 1,
                    component.shorthand.as_ref().unwrap_or(&String::new())
                ),
                color: component.summary_type.into(),
            });
        declarations.chain(shorthands).collect()
    }

    fn notes(&self) -> Vec<String> {
        self.components
            .iter()
            .enumerate()
            .filter_map(|(index, component)| {
                component
                    .note
                    .as_ref()
                    .map(|note| format!("[{}] {}: {}", index + 1, "note".bold(), note))
            })
            .collect()
    }

    fn fmt_messages(
        &self,
        input: &str,
        input_lines: &Vec<&str>,
        input_lines_positions: &Vec<usize>,
        width: usize,
    ) -> String {
        let dictionary = Standard::from_embedded(Language::EnglishUS).unwrap();
        let text_wrapper_options = Options::new(width).word_splitter(dictionary);
        self.messages()
            .iter()
            .enumerate()
            .map(move |(index, message)| {
                let index = (index + 1).to_string() + ". ";
                let index_len = index.len();
                fill(
                    &(index.bold().to_string() + message)[..],
                    text_wrapper_options
                        .clone()
                        .subsequent_indent(&(0..index_len).map(|_| ' ').collect::<String>()[..]),
                ) + "\n"
            })
            .collect::<String>()
    }

    fn fmt(&self, filepath: &str, input: &str, stage: SummaryStage, width: usize) -> String {
        let (input_lines, input_lines_positions): (Vec<&str>, Vec<usize>) = input
            .lines()
            .map(|line| (line, get_relative_range(input, line).unwrap().0))
            .unzip();
        let (span_line, span_column) =
            get_coords(input, &input_lines, &input_lines_positions, self.span).unwrap();
        format!(
            "{}:{}:{}\n{}\n{}\n{}\n",
            filepath,
            span_line,
            span_column,
            self.fmt_messages(input, &input_lines, &input_lines_positions, width),
            "",
            ""
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SummaryStage {
    Parser = 100,
    Semantic = 200,
}
pub struct Summary<'l> {
    filepath: &'l str,
    input: &'l str,
    stage: SummaryStage,
    cells: Vec<SummaryCell<'l>>,
    sep: Option<char>,
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

static MIN_SUMMARY_WIDTH: usize = 40;
static DEFAULT_SUMMARY_WIDTH: usize = 80;
impl<'l> Display for Summary<'l> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut width = f.width().unwrap_or(DEFAULT_SUMMARY_WIDTH);
        if (width < MIN_SUMMARY_WIDTH) {
            width = usize::MAX;
        }
        for (index, cell) in self.cells.iter().enumerate() {
            if let Some(sep) = self.sep && index != 0 {
                write!(
                    f,
                    "\n{}\n\n",
                    (0..width).into_iter().map(|_| sep).collect::<String>()
                )?;
            }
            f.write_str(&cell.fmt(self.filepath, self.input, self.stage, width))?;
        }
        Ok(())
    }
}

#[test]
fn test() {
    let input = "int a = 0;\nint b = 0;\nint c = 0;";
    let a = &input[..10];
    let b = &input[12..21];
    let c = &input[24..32];
    let mut summary = Summary::new("file/path.rs", input, SummaryStage::Semantic);
    summary.set_sep('-');

    let mut cell = SummaryCell::new(b);
    for _ in (0..10) {
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            "E200",
            &b[3..4],
            "This is a sample error message".to_string(),
        ));
    }
    summary.add_cell(cell);

    let cell = SummaryCell::new(c);
    summary.add_cell(cell);

    println!("{}", summary)
}
