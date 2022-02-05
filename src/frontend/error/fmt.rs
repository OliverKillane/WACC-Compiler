use super::prelude::*;
use super::span_utils::*;

use colored::{control::SHOULD_COLORIZE, Color, Colorize};
use nom::combinator::fail;
use std::iter::zip;
use std::ops::Add;
use std::{
    cmp::{max, min},
    collections::{HashMap, LinkedList},
    fmt::{Display, Write},
};

impl SummaryType {
    fn color(&self) -> Color {
        match self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
        }
    }
}

static NOTE_COLOR: Color = Color::Blue;
impl<'l> SummaryComponent<'l> {
    fn fmt_message(&self, index: usize) -> (String, usize, String) {
        (
            format!(
                "{} {}: ",
                format!("{}.", index).bold(),
                format!("{}[{}]", self.summary_type.to_str(), self.summary_code)
                    .color(self.summary_type.color())
                    .bold()
            ),
            format!(
                "{}. {}[{}]: ",
                index,
                self.summary_type.to_str(),
                self.summary_code
            )
            .len(),
            self.message.clone(),
        )
    }

    fn fmt_shorthand(&self, index: usize) -> String {
        if let Some(shorthand) = self.shorthand.as_ref() {
            format!("[{}] {}", index, shorthand)
        } else {
            format!("[{}]", index)
        }
    }

    fn fmt_note(&self, index: usize) -> Option<String> {
        Some(format!(
            "{} {}: {}",
            format!("[{}]", index).bold(),
            "note".color(Color::Blue).bold(),
            self.note.as_ref()?
        ))
    }
}

static DECLARED_COLOR: Color = Color::BrightBlue;
static EXCLUDED_COLOR: Color = Color::BrightBlack;
static TITLE_COLOR: Color = Color::Magenta;
impl<'l> SummaryCell<'l> {
    fn fmt_messages(
        components: &Vec<&SummaryComponent<'l>>,
        input_locator: &SpanLocator,
    ) -> String {
        let (max_prefix, messages) = components
            .iter()
            .enumerate()
            .map(|(index, component)| component.fmt_message(index + 1))
            .fold(
                (0, LinkedList::new()),
                |(max_prefix, mut items), (prefix, prefix_len, message)| {
                    items.push_back((prefix, prefix_len, message));
                    (max(max_prefix, prefix_len), items)
                },
            );
        messages
            .into_iter()
            .map(|(prefix, prefix_len, message)| {
                format!(
                    "{}{}{}\n",
                    prefix,
                    " ".repeat(max_prefix - prefix_len),
                    message
                )
            })
            .collect()
    }

    fn get_annotations(
        components: &Vec<&SummaryComponent<'l>>,
    ) -> LinkedList<(&'l str, String, Color)> {
        components
            .iter()
            .enumerate()
            .map(|(index, component)| {
                (
                    component.span,
                    component.fmt_shorthand(index + 1),
                    component.summary_type.color(),
                )
            })
            .chain(
                components
                    .iter()
                    .enumerate()
                    .filter_map(|(index, component)| {
                        Some((
                            component.declaration?,
                            format!("[{}] first declared here", index + 1),
                            DECLARED_COLOR,
                        ))
                    }),
            )
            .collect()
    }
    fn group_annotations(
        input_locator: &SpanLocator,
        annotations: Vec<(&'l str, String, Color)>,
    ) -> HashMap<usize, LinkedList<(&'l str, String, Color)>> {
        let mut line_nums = HashMap::new();
        for annotation @ (span, _, _) in annotations {
            let line_num = input_locator.get_line_num(span).unwrap();
            line_nums.try_insert(line_num, LinkedList::new());
            line_nums.get_mut(&line_num).unwrap().push_back(annotation);
        }
        line_nums
    }

    fn fmt_refs_line(
        line: &str,
        unincluded_prefix: usize,
        unincluded_suffix: usize,
        line_num: usize,
        line_num_width: usize,
        annotations: LinkedList<(&str, String, Color)>,
    ) -> String {
        let mut annotations = annotations
            .into_iter()
            .map(|(span, message, color)| {
                let (span_begin, span_end) = get_relative_range(line, span).unwrap();
                (span_begin, span_end, message, color)
            })
            .collect::<Vec<_>>();
        // Inserting spaces in-between the spans
        let mut last_end = 0;
        let mut spaces = Vec::new();
        for &(span_begin, span_end, _, _) in &annotations {
            if last_end > span_begin {
                panic!("Overlapping spans in a summary cell");
            }
            if last_end == span_begin && last_end != 0 {
                spaces.push(last_end);
            }
            last_end = span_end
        }
        if !spaces.is_empty() {
            let mut spaces_index = 0;
            for (span_begin, span_end, _, _) in &mut annotations {
                if spaces_index < spaces.len() && spaces[spaces_index] <= *span_begin {
                    spaces_index += 1;
                }
                *span_begin += spaces_index;
                *span_end += spaces_index;
            }
        }

        // Inserting spaces inside the line
        let line = if spaces.is_empty() {
            line.to_string()
        } else {
            let mut spaced_line = String::new();
            let mut spaces_index = 0;
            for (index, char) in line.chars().enumerate() {
                if spaces_index < spaces.len() && spaces[spaces_index] == index {
                    spaced_line += " ";
                    spaces_index += 1;
                }
                spaced_line.push(char);
            }
            spaced_line
        };

        // Adding inter-annotation arrow lines
        let arrow_lines = annotations.iter().rev().fold(
            LinkedList::<(String, String)>::new(),
            |mut rows, (column, _, message, color)| {
                let mut line_top = if let Some((row, _)) = rows.front() {
                    row.clone()
                } else {
                    " ".to_string().repeat(line.len())
                };
                let mut line_bottom = line_top.clone();
                line_bottom.replace_range(
                    *column..=*column,
                    &format!(
                        "{}",
                        (if *column <= message.len() { "V" } else { "|" })
                            .color(*color)
                            .bold()
                    ),
                );
                line_top.replace_range(*column..=*column, &format!("{}", "|".color(*color).bold()));
                rows.push_front((line_top, line_bottom));
                rows
            },
        );

        // Adding left arrows inside messages' lines
        for (span_begin, _, message, color) in &mut annotations {
            if message.len() < *span_begin {
                *message += " <";
                if message.len() <= *span_begin {
                    *message += &(0..*span_begin - message.len())
                        .map(|_| '-')
                        .collect::<String>();
                    *message += "/"
                }
            }
        }

        // Adding arrow lines after the messages
        let message_suffixes = annotations
            .iter()
            .map(|(_, _, message, _)| {
                let mut message_suffix = String::new();
                let mut message_suffix_len = 0usize;
                for &(column, _, _, color) in &annotations {
                    if column >= message.len() {
                        message_suffix += &format!(
                            "{}{}",
                            (message_suffix_len + message.len()..column)
                                .map(|_| ' ')
                                .collect::<String>(),
                            "|".color(color).bold()
                        );
                        message_suffix_len = column - message.len() + 1;
                    }
                }
                message_suffix
            })
            .collect::<LinkedList<_>>();
        for ((_, _, message, color), message_suffix) in zip(&mut annotations, message_suffixes) {
            *message = format!("{}{}", message.color(*color).bold(), &message_suffix);
        }

        // Piecing everything together
        let line_num = (line_num + 1).to_string();
        let line_num_indent = format!(
            " {}{} | ",
            (0..line_num_width - line_num.len())
                .map(|_| ' ')
                .collect::<String>(),
            line_num
        );
        let blank_indent = format!(" {} | ", " ".to_string().repeat(line_num_width));

        let mut full_refs = line_num_indent;
        if SHOULD_COLORIZE.should_colorize() {
            let (mut prefix, mut remaining) = annotations.iter().fold(
                (String::new(), 0),
                |(mut prefix, last_end), &(span_begin, span_end, _, color)| {
                    prefix += &format!(
                        "{}{}{}",
                        if last_end < unincluded_prefix {
                            format!(
                                "{}",
                                &line[last_end..unincluded_prefix].color(EXCLUDED_COLOR)
                            )
                        } else {
                            String::new()
                        },
                        &line[max(last_end, unincluded_prefix)..span_begin],
                        &line[span_begin..span_end].underline().color(color).bold()
                    );
                    (prefix, span_end)
                },
            );
            if remaining < line.len() - unincluded_suffix {
                prefix += &line[remaining..line.len() - unincluded_suffix];
                remaining = line.len() - unincluded_suffix;
            }
            prefix += &format!("{}", line[remaining..].color(EXCLUDED_COLOR));
            full_refs += &prefix;
            full_refs += "\n";
        } else {
            full_refs += &line;
            full_refs += "\n";
            let mut underlines = " ".to_string().repeat(line.len());
            for &(span_begin, span_end, _, _) in &annotations {
                underlines.replace_range(
                    span_begin..span_end,
                    &"^".to_string().repeat(span_end - span_begin),
                );
            }
            full_refs += &blank_indent;
            full_refs += &underlines;
            full_refs += "\n";
        }
        for ((arrows_top, arrows_bottom), (_, _, message, _)) in zip(arrow_lines, annotations) {
            full_refs += &blank_indent;
            full_refs += &arrows_top;
            full_refs += "\n";
            full_refs += &blank_indent;
            full_refs += &arrows_bottom;
            full_refs += "\n";
            full_refs += &blank_indent;
            full_refs += &message;
            full_refs += "\n";
        }
        full_refs
    }

    fn fmt_refs(
        components: &Vec<&SummaryComponent<'l>>,
        span: &str,
        input_locator: &SpanLocator,
    ) -> String {
        let mut annotations = Self::get_annotations(components)
            .into_iter()
            .collect::<Vec<_>>();
        annotations.sort_by_key(|&(span, _, _)| input_locator.get_range(span).unwrap().0);
        let mut annotations: Vec<_> = Self::group_annotations(input_locator, annotations)
            .into_iter()
            .collect();
        annotations.sort_by_key(|(line_num, _)| *line_num);
        if annotations.is_empty() {
            return String::new();
        }
        let (min_line, max_line) = annotations
            .iter()
            .map(|(index, _)| index)
            .fold((usize::MAX, 0usize), |(min_line, max_line), &line_num| {
                (min(min_line, line_num), max(max_line, line_num))
            });
        let line_num_width = (max_line + 1).to_string().len();
        let mut separators = zip(&annotations, annotations.iter().skip(1))
            .map(|(&(last_line, _), &(next_line, _))| (last_line, next_line))
            .collect::<LinkedList<_>>()
            .into_iter()
            .map(|(last_line, next_line)| {
                if last_line + 1 == next_line {
                    format!(" {} |\n", " ".to_string().repeat(line_num_width))
                } else {
                    " ...\n".to_string()
                }
            });
        annotations
            .into_iter()
            .map(|(line_num, annotations)| {
                let line = input_locator.get_input_line(line_num).unwrap();
                let (span_begin, span_end) = input_locator.get_range(span).unwrap();
                let (line_begin, line_end) = input_locator.get_range(line).unwrap();
                Self::fmt_refs_line(
                    line,
                    max(span_begin as isize - line_begin as isize, 0) as usize,
                    max(line_end as isize - span_end as isize, 0) as usize,
                    line_num,
                    line_num_width,
                    annotations,
                )
            })
            .intersperse_with(|| separators.next().unwrap())
            .collect()
    }

    fn fmt_notes(components: &Vec<&SummaryComponent<'l>>, input_locator: &SpanLocator) -> String {
        components
            .into_iter()
            .enumerate()
            .filter_map(|(index, component)| Some(format!(" {}\n", component.fmt_note(index + 1)?)))
            .collect()
    }

    fn fmt(&self, filepath: &str, input: &str) -> String {
        let input_locator = SpanLocator::new(input);
        let mut components = self.components.iter().collect::<Vec<_>>();
        components.sort_by_key(|component| input_locator.get_range(component.span));

        let (span_line, span_column) = input_locator.get_coords(self.span).unwrap();
        format!(
            "{}",
            format!(
                "{}:{}:{}{}\n",
                filepath,
                span_line + 1,
                span_column + 1,
                self.title
                    .as_ref()
                    .map(|title| " -> ".to_string() + &title[..])
                    .unwrap_or(String::new())
            )
            .color(TITLE_COLOR)
            .bold()
        ) + &Self::fmt_messages(&components, &input_locator)
            + &Self::fmt_refs(&components, self.span, &input_locator)
            + &Self::fmt_notes(&components, &input_locator)
    }
}

static DEFAULT_SUMMARY_WIDTH: usize = 80;
static MAIN_HEADER_COLOR: Color = Color::Cyan;
impl<'l> Display for Summary<'l> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut width = f.width().unwrap_or(DEFAULT_SUMMARY_WIDTH);
        write!(
            f,
            "{}",
            format!(
                "An error has occured during {}\n",
                match self.stage {
                    SummaryStage::Parser => "parsing",
                    SummaryStage::Semantic => "semantic analysis",
                }
            )
            .color(MAIN_HEADER_COLOR)
            .bold()
            .italic()
        );
        for (index, cell) in self.cells.iter().enumerate() {
            if let Some(sep) = self.sep && index != 0 {
                write!(
                    f,
                    "\n{}\n\n",
                    (0..width).into_iter().map(|_| sep).collect::<String>()
                )?;
            }
            f.write_str(&cell.fmt(self.filepath, self.input))?;
        }
        Ok(())
    }
}

#[test]
fn test() {
    let input =
        "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nint x = 3, a = 0;\n\nint b = 0;\nint c = 0;";
    let stat = &input[20..53];
    let mut summary = Summary::new("file/path.rs", input, SummaryStage::Semantic);
    summary.set_sep('-');

    let mut cell = SummaryCell::new(stat);
    cell.set_title("Example title".to_string());
    for (i, &(b, e)) in [(11, 16), (19, 22), (23, 33)].iter().enumerate() {
        let mut component = SummaryComponent::new(
            if i % 2 == 0 {
                SummaryType::Error
            } else {
                SummaryType::Warning
            },
            200,
            &stat[b..e],
            format!("{}{}", "This is a sample error message nr ", i),
        );
        component.set_shorthand("ex. shorthand".to_string());
        if i % 3 == 0 {
            component.set_note(format!("{}{}", "This is a sample note nr ", i));
        }
        if i % 2 == 1 {
            component.set_declaration(&stat[0..3])
        }
        cell.add_component(component);
    }
    summary.add_cell(cell);

    println!("{}", summary)
}
