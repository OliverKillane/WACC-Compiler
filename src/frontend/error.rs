use colored::{Color, Colorize};
use hyphenation::Load;
use std::{
    cmp::{max, min},
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
    summary_code: u32,
    span: &'l str,
    declaration: Option<&'l str>,
    message: String,
    shorthand: Option<String>,
    note: Option<String>,
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

static NOTE_COLOR: Color = Color::Blue;
impl<'l> SummaryComponent<'l> {
    fn fmt_message(&self) -> String {
        format!(
            "{}: {}",
            format!("{}[{}]", self.summary_type.to_string(), self.summary_code)
                .color(self.summary_type)
                .bold(),
            &self.message
        )
    }

    fn fmt_note(&self) -> Option<String> {
        Some(format!(
            "{}: {}",
            "note".color(Color::Blue).bold(),
            self.note.as_ref()?
        ))
    }
}

pub struct SummaryCell<'l> {
    span: &'l str,
    components: LinkedList<SummaryComponent<'l>>,
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

struct SpanLocator<'l> {
    input: &'l str,
    input_lines: Vec<(usize, usize)>,
}

impl<'l> SpanLocator<'l> {
    fn new(input: &'l str) -> Self {
        SpanLocator {
            input,
            input_lines: input
                .lines()
                .map(|line| get_relative_range(input, line).unwrap())
                .collect(),
        }
    }

    fn get_coords(&self, span: &str) -> Option<(usize, usize)> {
        let (input_pos, _) = get_relative_range(self.input, span)?;
        let line_num = self
            .input_lines
            .binary_search_by(|(line_start, _)| line_start.cmp(&input_pos))
            .unwrap_or_else(|idx| idx - 1);
        assert!(
            line_num != usize::MAX,
            "span before the first input line position"
        );
        let (line_pos, _) = get_relative_range(
            &self.input[self.input_lines[line_num].0..self.input_lines[line_num].1],
            span,
        )?;
        Some((line_num, line_pos))
    }
}

struct SummaryCellFmtMeta<'l> {
    span_locator: SpanLocator<'l>,
    dictionary: hyphenation::Standard,
}

impl<'l> SummaryCellFmtMeta<'l> {
    fn new(input: &'l str) -> Self {
        SummaryCellFmtMeta {
            span_locator: SpanLocator::new(input),
            dictionary: hyphenation::Standard::from_embedded(hyphenation::Language::EnglishUS)
                .unwrap(),
        }
    }
}

static DECLARED_COLOR: Color = Color::BrightBlue;
impl<'l> SummaryCell<'l> {
    fn fmt_messages(&self, metadata: &SummaryCellFmtMeta, width: usize) -> String {
        let text_wrapper_options = Options::new(width).word_splitter(metadata.dictionary.clone());
        self.components
            .iter()
            .enumerate()
            .map(|(index, component)| {
                let index = (index + 1).to_string() + ". ";
                let indent: String = (0..index.len()).map(|_| ' ').collect();
                fill(
                    &(index.bold().to_string() + &component.fmt_message()),
                    text_wrapper_options.clone().subsequent_indent(&indent),
                ) + "\n"
            })
            .collect()
    }

    fn fmt_refs_wrapped_line(
        &self,
        annotations_prefix: &str,
        line: String,
        metadata: &SummaryCellFmtMeta,
        components: LinkedList<&SummaryComponent>,
    ) -> String {
        todo!()
    }

    fn fmt_refs_line(
        &self,
        metadata: &SummaryCellFmtMeta,
        max_line_num: usize,
        line_num: usize,
        components: LinkedList<&SummaryComponent>,
        width: usize,
    ) -> String {
        let max_line_num_width = max_line_num.to_string().len();
        let line_num_repr = line_num.to_string();
        let line_num_repr: String = (0..max_line_num_width - line_num_repr.len())
            .map(|_| ' ')
            .collect::<String>()
            + &line_num_repr;
        let line_num_prefix = format!(" {} | ", line_num);
        let blank_prefix = format!(
            " {} | ",
            (0..max_line_num_width).map(|_| ' ').collect::<String>()
        );

        let (statement_line_span_begin, statement_line_span_end) =
            metadata.span_locator.input_lines[line_num];
        let statement_line =
            &metadata.span_locator.input[statement_line_span_begin..statement_line_span_end];

        let wrapped_statement_line = fill(
            statement_line,
            Options::new(width - line_num_prefix.len()).word_splitter(metadata.dictionary.clone()),
        );
        let wrapped_statement_line_locator = SpanLocator::new(&wrapped_statement_line);
        let mut wrapped_lines_components = HashMap::new();
        for component in components {
            let (line_num, _) = wrapped_statement_line_locator
                .get_coords(component.span)
                .expect("Component span not within the bounds of statement line");
            wrapped_lines_components.try_insert(line_num, LinkedList::new());
            wrapped_lines_components
                .get_mut(&line_num)
                .unwrap()
                .push_back(component);
        }

        let wrapped_statement_line_lines: Vec<&str> = wrapped_statement_line.lines().collect();

        let mut wrapped_lines_components = wrapped_lines_components.into_iter().collect::<Vec<_>>();
        wrapped_lines_components.sort_by_key(|(line_num, _)| *line_num);
        wrapped_lines_components
            .into_iter()
            .map(|(line_num, components)| {
                self.fmt_refs_wrapped_line(
                    &blank_prefix,
                    if line_num == 0 {
                        &line_num_prefix
                    } else {
                        &blank_prefix
                    }
                    .to_string()
                        + wrapped_statement_line_lines[line_num],
                    metadata,
                    components,
                )
            })
            .collect()
    }

    fn fmt_refs(&self, metadata: &SummaryCellFmtMeta, width: usize) -> String {
        let mut max_line_num = 0;
        let mut lines_refs = HashMap::new();
        for component in &self.components {
            let (line_num, _) = metadata
                .span_locator
                .get_coords(component.span)
                .expect("Component span not within the bounds of input");
            lines_refs.try_insert(line_num, LinkedList::new());
            lines_refs.get_mut(&line_num).unwrap().push_back(component);
            max_line_num = max(max_line_num, line_num);
        }
        let mut lines_refs = lines_refs.into_iter().collect::<Vec<_>>();
        lines_refs.sort_by_key(|(line_num, _)| *line_num);
        lines_refs
            .into_iter()
            .map(|(line_num, components)| {
                self.fmt_refs_line(metadata, max_line_num, line_num, components, width)
            })
            .collect()
    }

    fn fmt_notes(&self, metadata: &SummaryCellFmtMeta, width: usize) -> String {
        let text_wrapper_options = Options::new(width).word_splitter(metadata.dictionary.clone());
        self.components
            .iter()
            .enumerate()
            .filter_map(|(index, component)| {
                let index = format!(" [{}] ", index + 1);
                let indent: String = (0..index.len()).map(|_| ' ').collect();
                Some(
                    fill(
                        &format!("{}{}", index.bold(), component.fmt_note()?),
                        text_wrapper_options.clone().subsequent_indent(&indent),
                    ) + "\n",
                )
            })
            .collect()
    }

    fn fmt(&self, filepath: &str, input: &str, stage: SummaryStage, mut width: usize) -> String {
        let lines_count = input.lines().count();
        let max_line_count_width = lines_count.to_string().len();
        let code_offset = 4 + 20;
        if width < max_line_count_width + code_offset {
            width = usize::MAX;
        }

        let metadata = SummaryCellFmtMeta::new(input);
        let (span_line, span_column) = metadata
            .span_locator
            .get_coords(self.span)
            .expect("Span not within the bounds of input");
        format!(
            "{}:{}:{}\n{}\n{}\n{}\n",
            filepath,
            span_line,
            span_column,
            self.fmt_messages(&metadata, width),
            self.fmt_refs(&metadata, width),
            self.fmt_notes(&metadata, width)
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
    for i in (0..10) {
        let mut component = SummaryComponent::new(
            if i % 2 == 0 {
                SummaryType::Error
            } else {
                SummaryType::Warning
            },
            200,
            &b[3..4],
            "This is a sample error message".to_string(),
        );
        component.set_note("This is a sample note".to_string());
        cell.add_component(component);
    }
    summary.add_cell(cell);

    let cell = SummaryCell::new(c);
    summary.add_cell(cell);

    println!("{}", summary)
}
