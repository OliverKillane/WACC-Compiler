//! Contains formatting logic for colouring, highlighting messages and adding
//! arrows, taking into account unicode variable length encodings.

use super::prelude::*;
use super::span_utils::*;

use colored::{control::SHOULD_COLORIZE, Color, Colorize};
use std::{
    cmp::{max, min},
    collections::{HashMap, LinkedList},
    fmt::Display,
    iter::zip,
};
use unicode_width::UnicodeWidthChar;

impl SummaryType {
    /// Produces a color for a summary type. Not implemented as Into as to not
    /// make the function public.
    fn color(&self) -> Color {
        match self {
            Self::Error => Color::Red,
            Self::Warning => Color::Yellow,
        }
    }

    /// Produces a string for a summary type. Not implemented as ToString not to
    /// make the function public.
    fn get_str(&self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
        }
    }
}

static NOTE_COLOR: Color = Color::Blue;
impl<'l> SummaryComponent<'l> {
    /// Produces a formatted message for a component in the notes section of the
    /// error cell.
    fn fmt_message(&self, index: usize) -> (String, String, usize, String) {
        (
            format!("{} ", format!("{}.", index).bold()),
            format!(
                "{}: ",
                format!("{}[{}]", self.summary_type.get_str(), self.summary_code)
                    .color(self.summary_type.color())
                    .bold()
            ),
            format!("{}[{}]: ", self.summary_type.get_str(), self.summary_code).len(),
            self.message.clone(),
        )
    }

    /// Produces a formatted shorthand for a component in the notes section of the
    /// error cell.
    fn fmt_shorthand(&self, index: usize) -> String {
        if let Some(shorthand) = self.shorthand.as_ref()
            && !shorthand.trim_matches(&[' ', '\t', '\n'][..]).is_empty() {
            format!("[{}] {}", index, shorthand.trim_matches(&[' ', '\t', '\n'][..]))
        } else {
            format!("[{}]", index)
        }
    }

    /// Produces a formatted note for a component in the notes section of the
    /// error cell.
    fn fmt_note(&self, index: usize) -> Option<String> {
        Some(format!(
            "{} {}: {}",
            format!("[{}]", index).bold(),
            "note".color(Color::Blue).bold(),
            self.note.as_ref()?
        ))
    }

    /// Produces a formatted code-less declared message in the declarations
    /// section of the error cell.
    fn fmt_declaration(
        &self,
        index: usize,
        multi_input_locator: &MultiInputLocator<'l>,
    ) -> Option<String> {
        let declaration = self.declaration?;
        let filepath = multi_input_locator.get_filepath(declaration);
        let (line, column) = multi_input_locator
            .get_locator(declaration)
            .get_coords(declaration);
        Some(format!(
            "{} {}",
            format!("[{}]", index).bold(),
            format!("first declared in {}:{}:{}", filepath, line + 1, column + 1)
                .color(DECLARED_COLOR)
                .bold()
        ))
    }
}

static DECLARED_COLOR: Color = Color::BrightBlue;
static EXCLUDED_COLOR: Color = Color::BrightBlack;
static TITLE_COLOR: Color = Color::Magenta;
impl<'l> SummaryCell<'l> {
    /// Formats the full messages section of a single error cell.
    fn fmt_messages(components: &[&SummaryComponent<'l>]) -> String {
        let (max_prefix, messages) = components
            .iter()
            .enumerate()
            .map(|(index, component)| component.fmt_message(index + 1))
            .fold(
                (0, LinkedList::new()),
                |(max_prefix, mut items), (index, prefix, prefix_len, message)| {
                    items.push_back((index, prefix, prefix_len, message));
                    (max(max_prefix, prefix_len), items)
                },
            );
        messages
            .into_iter()
            .map(|(index, prefix, prefix_len, message)| {
                format!(
                    "{}{}{}{}\n",
                    index,
                    " ".repeat(max_prefix - prefix_len),
                    prefix,
                    message
                )
            })
            .collect()
    }

    /// Simplifies the error cell components before formatting them line-by-line.
    fn get_annotations(
        components: &[&SummaryComponent<'l>],
        input_locator: &SpanLocator,
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
                        input_locator
                            .get_optional_range(component.declaration?)
                            .map(|_| {
                                (
                                    component.declaration.unwrap(),
                                    format!("[{}] first declared here", index + 1),
                                    DECLARED_COLOR,
                                )
                            })
                    }),
            )
            .collect()
    }

    /// Groups the spans in a single error cell into lines.
    fn group_annotations(
        input_locator: &SpanLocator,
        annotations: Vec<(&'l str, String, Color)>,
    ) -> HashMap<usize, LinkedList<(&'l str, String, Color)>> {
        let mut line_nums: HashMap<usize, LinkedList<(&str, String, Color)>> = HashMap::new();
        for annotation @ (span, _, _) in annotations {
            let line_num = input_locator.get_line_num(span);
            if let Some(anns) = line_nums.get_mut(&line_num) {
                anns.push_back(annotation)
            } else {
                line_nums.insert(line_num, LinkedList::from([annotation]));
            }
        }
        line_nums
    }

    /// Colors the given text and underlines the given ranges accordingly.
    fn fmt_colored_underlines(
        text: &str,
        color: Option<Color>,
        underlines: LinkedList<(usize, usize, Color)>,
    ) -> String {
        if underlines.is_empty() {
            color.map_or(text.to_string(), |color| format!("{}", text.color(color)))
        } else {
            let mut underlined_text = String::new();
            let mut last_end = 0;
            for (underline_begin, underline_end, underline_color) in underlines {
                let default_span = &text[last_end..underline_begin];
                if let Some(color) = color {
                    underlined_text += &format!("{}", default_span.color(color));
                } else {
                    underlined_text += default_span;
                }
                underlined_text += &format!(
                    "{}",
                    &text[underline_begin..underline_end]
                        .color(underline_color)
                        .underline()
                        .bold()
                );
                last_end = underline_end;
            }
            if let Some(color) = color {
                underlined_text += &format!("{}", text[last_end..].color(color));
            } else {
                underlined_text += &text[last_end..];
            }
            underlined_text
        }
    }

    /// Takes care of underlining the code in a single code presentation line.
    #[allow(clippy::too_many_arguments)]
    fn fmt_refs_line_underlines(
        line: &str,
        line_len: usize,
        line_index_to_column: &HashMap<usize, usize>,
        unincluded_prefix: usize,
        unincluded_suffix: usize,
        line_num_prefix: &str,
        blank_num_prefix: &str,
        underlines: LinkedList<(usize, usize, Color)>,
    ) -> String {
        assert!(unincluded_prefix + unincluded_suffix <= line.len());
        let mut full_refs = line_num_prefix.to_string();
        if SHOULD_COLORIZE.should_colorize() {
            let unincluded_prefix_underlines = underlines
                .iter()
                .filter_map(|&(span_begin, span_end, color)| {
                    if span_begin >= unincluded_prefix {
                        None
                    } else {
                        Some((span_begin, min(span_end, unincluded_prefix), color))
                    }
                })
                .collect();
            let included_underlines = underlines
                .iter()
                .filter_map(|&(span_begin, span_end, color)| {
                    if span_end <= unincluded_prefix || span_begin >= line.len() - unincluded_suffix
                    {
                        None
                    } else {
                        Some((
                            max(span_begin, unincluded_prefix) - unincluded_prefix,
                            min(span_end, line.len() - unincluded_suffix) - unincluded_prefix,
                            color,
                        ))
                    }
                })
                .collect();
            let unincluded_suffix_underlines = underlines
                .iter()
                .filter_map(|&(span_begin, span_end, color)| {
                    if span_end <= line.len() - unincluded_suffix {
                        None
                    } else {
                        Some((
                            max(span_begin, line.len() - unincluded_suffix)
                                - (line.len() - unincluded_suffix),
                            span_end - (line.len() - unincluded_suffix),
                            color,
                        ))
                    }
                })
                .collect();

            full_refs += &Self::fmt_colored_underlines(
                &line[..unincluded_prefix],
                Some(EXCLUDED_COLOR),
                unincluded_prefix_underlines,
            );
            full_refs += &Self::fmt_colored_underlines(
                &line[unincluded_prefix..line.len() - unincluded_suffix],
                None,
                included_underlines,
            );
            full_refs += &Self::fmt_colored_underlines(
                &line[line.len() - unincluded_suffix..],
                Some(EXCLUDED_COLOR),
                unincluded_suffix_underlines,
            );
            full_refs += "\n";
        } else {
            full_refs += line;
            full_refs += "\n";
            let mut underarrows = " ".to_string().repeat(line_len);
            for (span_begin, span_end, _) in underlines {
                underarrows.replace_range(
                    line_index_to_column[&span_begin]..line_index_to_column[&span_end],
                    &"^".to_string().repeat(
                        line_index_to_column[&span_end] - line_index_to_column[&span_begin],
                    ),
                );
            }
            full_refs += blank_num_prefix;
            full_refs += &underarrows;
            full_refs += "\n";
        }
        full_refs
    }

    /// Puts spaces between the spans that are too close in a single code presentaiton line.
    fn fmt_refs_line_spread_out(
        line: &mut String,
        unincluded_prefix: &mut usize,
        unincluded_suffix: &mut usize,
        annotations: &mut Vec<(usize, usize, String, usize, Color)>,
    ) {
        let mut spaces = vec![];
        let mut last_end = 0;
        for &mut (span_begin, span_end, _, _, _) in &mut *annotations {
            if last_end > span_begin {
                panic!("Overlapping spans in a summary cell");
            }
            if last_end == span_begin && last_end != 0 {
                spaces.push(last_end);
            }
            last_end = span_end;
        }
        if !spaces.is_empty() {
            *unincluded_prefix += spaces
                .binary_search(unincluded_prefix)
                .unwrap_or_else(|x| x);
            *unincluded_suffix += spaces.len()
                - spaces
                    .binary_search(&(line.len() - *unincluded_suffix))
                    .unwrap_or_else(|x| x - 1)
                - 1;

            let mut spaces_index = 0;
            for (span_begin, span_end, _, _, _) in &mut *annotations {
                if spaces_index < spaces.len() && spaces[spaces_index] <= *span_begin {
                    spaces_index += 1;
                }
                *span_begin += spaces_index;
                *span_end += spaces_index;
            }

            let mut spaced_line = String::new();
            let mut spaces_index = 0;
            for (index, char) in line.chars().enumerate() {
                if spaces_index < spaces.len() && spaces[spaces_index] == index {
                    spaced_line += " ";
                    spaces_index += 1;
                }
                spaced_line.push(char);
            }
            *line = spaced_line;
        }
    }

    /// Formats a single code presentation line of the error cell.
    fn fmt_refs_line(
        line: &str,
        selected_prefix: (usize, Color),
        mut unincluded_prefix: usize,
        mut unincluded_suffix: usize,
        line_num: usize,
        line_num_width: usize,
        annotations: LinkedList<(&str, String, Color)>,
    ) -> String {
        let mut annotations = annotations
            .into_iter()
            .map(|(span, message, color)| {
                let message_len = message.chars().count();
                let (span_begin, mut span_end) = get_relative_range(line, span).unwrap();
                if span_begin == span_end {
                    span_end += 1;
                }
                (span_begin, span_end, message, message_len, color)
            })
            .collect::<Vec<_>>();
        let mut line = line.to_string();

        // Taking care of the spans that point to the end of the line
        for &(span_begin, _, _, _, _) in &annotations {
            if span_begin == line.len() {
                line += " ";
                break;
            }
        }

        // Spreading the neighboring spans out so that there are spaces between them
        Self::fmt_refs_line_spread_out(
            &mut line,
            &mut unincluded_prefix,
            &mut unincluded_suffix,
            &mut annotations,
        );

        // Calculating the mapping from span bounds to columns in the output text
        let mut column_cnt = 0;
        let mut line_index_to_column = line
            .char_indices()
            .map(|(index, c)| {
                let curr_column_cnt = column_cnt;
                column_cnt += UnicodeWidthChar::width(c).unwrap_or(0);
                (index, curr_column_cnt)
            })
            .collect::<HashMap<_, _>>();
        line_index_to_column.insert(line.len(), column_cnt);
        let line_index_to_column = line_index_to_column;
        let line_len = column_cnt;

        // Adding inter-annotation arrow lines
        let arrow_lines = annotations.iter().rev().fold(
            LinkedList::<(String, String)>::new(),
            |mut rows, (column, _, _, message_len, color)| {
                let column = line_index_to_column[column];
                let mut line_top = if let Some((row, _)) = rows.front() {
                    row.clone()
                } else {
                    " ".to_string().repeat(line_len)
                };
                let mut line_bottom = line_top.clone();
                line_bottom.replace_range(
                    column..=column,
                    &format!(
                        "{}",
                        (if column <= *message_len { "V" } else { "|" })
                            .color(*color)
                            .bold()
                    ),
                );
                line_top.replace_range(column..=column, &format!("{}", "|".color(*color).bold()));
                rows.push_front((line_top, line_bottom));
                rows
            },
        );

        // Adding left arrows inside messages' lines
        for (span_begin, _, message, message_len, _) in &mut annotations {
            if *message_len < line_index_to_column[span_begin] {
                *message += " <";
                *message_len += 2;
                if *message_len <= line_index_to_column[span_begin] {
                    *message += &"-"
                        .to_string()
                        .repeat(line_index_to_column[span_begin] - *message_len);
                    *message += "/";
                    *message_len = line_index_to_column[span_begin] + 1;
                }
            }
        }

        // Adding arrow lines after the messages
        let message_suffixes = annotations
            .iter()
            .map(|(span_begin, _, _, message_len, _)| {
                let mut message_suffix = String::new();
                let mut message_suffix_len = 0usize;
                for (column, _, _, _, color) in &annotations {
                    let column = line_index_to_column[column];
                    if column >= *message_len && column > *span_begin {
                        message_suffix += &format!(
                            "{}{}",
                            (message_suffix_len + message_len..column)
                                .map(|_| ' ')
                                .collect::<String>(),
                            "|".color(*color).bold()
                        );
                        message_suffix_len = column - message_len + 1;
                    }
                }
                message_suffix
            })
            .collect::<LinkedList<_>>();
        for ((_, _, message, _, color), message_suffix) in zip(&mut annotations, message_suffixes) {
            *message = format!("{}{}", message.color(*color).bold(), &message_suffix);
        }

        // Indents
        let line_num = (line_num + 1).to_string();
        let line_num_indent = format!(
            " {}{} | ",
            " ".to_string().repeat(line_num_width - line_num.len()),
            line_num
        );
        let blank_indent = format!(" {} | ", " ".to_string().repeat(line_num_width));

        // Adding underlines to the code line
        let mut underlines = LinkedList::new();
        if selected_prefix.0 > 0 {
            underlines.push_back((0, selected_prefix.0, selected_prefix.1));
        }
        for &(span_begin, span_end, _, _, color) in &annotations {
            underlines.push_back((span_begin, span_end, color));
        }
        let mut full_refs = Self::fmt_refs_line_underlines(
            &line,
            line_len,
            &line_index_to_column,
            unincluded_prefix,
            unincluded_suffix,
            &line_num_indent,
            &blank_indent,
            underlines,
        );

        // Adding the annotations and messages
        for ((arrows_top, arrows_bottom), (_, _, message, _, _)) in zip(arrow_lines, annotations) {
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

    /// Formats the code presentation section of the error cell.
    #[allow(unused_must_use)]
    fn fmt_refs(
        components: &[&SummaryComponent<'l>],
        span: &str,
        input_locator: &SpanLocator,
    ) -> String {
        let mut annotations = Self::get_annotations(components, input_locator)
            .into_iter()
            .collect::<Vec<_>>();
        if annotations.is_empty() {
            return String::default();
        }
        annotations.sort_by_key(|&(span, _, _)| input_locator.get_range(span).0);

        let mut selected_prefixes = HashMap::<usize, _>::new();
        for &(span, _, color) in &annotations {
            let first_line = input_locator.get_line_num(&span[0..]);
            let last_char_index =
                if let Some(last_char_index) = span.char_indices().map(|(index, _)| index).last() {
                    last_char_index
                } else {
                    continue;
                };
            let (last_line, _) = input_locator.get_coords(&span[last_char_index..]);
            if first_line == last_line {
                continue;
            }
            let (_, span_end) = input_locator.get_range(span);
            for line in first_line + 1..=last_line {
                let (line_begin, line_end) =
                    input_locator.get_range(input_locator.get_input_line(line));
                selected_prefixes.insert(line, (min(line_end, span_end) - line_begin, color));
            }
        }

        let mut annotations = Self::group_annotations(input_locator, annotations);
        for &line_num in selected_prefixes.keys() {
            if !input_locator.get_input_line(line_num).is_empty() {
                annotations.try_insert(line_num, LinkedList::new());
            }
        }
        let mut annotations = annotations.into_iter().collect::<Vec<_>>();
        annotations.sort_by_key(|(line_num, _)| *line_num);

        let max_line = annotations.iter().map(|(index, _)| index).max().unwrap();
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
        let (span_begin, span_end) = input_locator.get_range(span);
        annotations
            .into_iter()
            .map(|(line_num, annotations)| {
                let line = input_locator.get_input_line(line_num);
                let (line_begin, line_end) = input_locator.get_range(line);
                Self::fmt_refs_line(
                    line,
                    *selected_prefixes
                        .get(&line_num)
                        .unwrap_or(&(0, Color::White)),
                    min(
                        max(span_begin as isize - line_begin as isize, 0) as usize,
                        line.len(),
                    ),
                    min(
                        max(line_end as isize - span_end as isize, 0) as usize,
                        line.len(),
                    ),
                    line_num,
                    line_num_width,
                    annotations,
                )
            })
            .intersperse_with(|| separators.next().unwrap())
            .collect()
    }

    /// Formats the notes section of the error cell.
    fn fmt_notes(components: &[&SummaryComponent<'l>]) -> String {
        components
            .iter()
            .enumerate()
            .filter_map(|(index, component)| Some(format!(" {}\n", component.fmt_note(index + 1)?)))
            .collect()
    }

    /// Formats the declarations section of the error cell.
    fn fmt_declarations(
        components: &[&SummaryComponent<'l>],
        input_locator: &SpanLocator<'l>,
        multi_input_locator: &MultiInputLocator<'l>,
    ) -> String {
        components
            .iter()
            .enumerate()
            .filter_map(|(index, component)| {
                if input_locator
                    .get_optional_range(component.declaration?)
                    .is_none()
                {
                    Some(format!(
                        " {}\n",
                        component.fmt_declaration(index + 1, multi_input_locator)?
                    ))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Formats a single error cell.
    fn fmt(&self, multi_input_locator: &MultiInputLocator<'l>) -> String {
        let input_locator = multi_input_locator.get_locator(self.span);
        let filepath = multi_input_locator.get_filepath(self.span);
        let mut components = self.components.iter().collect::<Vec<_>>();

        // sort components by their location
        components.sort_by_key(|component| input_locator.get_range(component.span));

        // if there are no components, or there is a component with no span, use
        // the statement span, otherwise use the minimum coordinate
        let (span_line, span_column) = input_locator.get_coords(
            components
                .first()
                .map_or_else(|| self.span, |comp| comp.span),
        );

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
                    .unwrap_or_default()
            )
            .color(TITLE_COLOR)
            .bold()
        ) + &Self::fmt_messages(&components)
            + &Self::fmt_refs(&components, self.span, input_locator)
            + &Self::fmt_notes(&components)
            + &Self::fmt_declarations(&components, input_locator, multi_input_locator)
    }
}

impl<'l> Summary<'l> {
    fn check_span_relations(&self, multi_input_locator: &MultiInputLocator<'l>) {
        for cell in &self.cells {
            multi_input_locator
                .get_optional_filepath(cell.span)
                .expect("Statement not in any input file");
            for component in &cell.components {
                if let Some(declaration) = component.declaration {
                    multi_input_locator
                        .get_optional_filepath(declaration)
                        .expect("Declaration not in any input file");
                }
            }
        }
    }
}

static MAIN_HEADER_COLOR: Color = Color::Cyan;
impl<'l> Display for Summary<'l> {
    /// Produces a string for the entire summary.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let multi_input_locator = MultiInputLocator::new(&self.input_files);
        #[cfg(debug_assertions)]
        self.check_span_relations(&multi_input_locator);

        let preamble = format!(
            "{} been found during {}\n",
            if self.cells.len() > 1 {
                format!("{} erroneous statements have", self.cells.len())
            } else {
                String::from("An erroneous statement has")
            },
            match self.stage {
                SummaryStage::Parser => "parsing",
                SummaryStage::Semantic => "semantic analysis",
            }
        );
        write!(f, "{}", preamble.color(MAIN_HEADER_COLOR).bold().italic())?;

        let cell_strings = self
            .cells
            .iter()
            .map(|cell| cell.fmt(&multi_input_locator))
            .collect::<LinkedList<_>>();

        let max_cell_width = cell_strings
            .iter()
            .map(|cell_string| {
                cell_string
                    .lines()
                    .map(|line| {
                        line.trim_end_matches(&[' ', '\n', '\t'][..])
                            .chars()
                            .map(|c| UnicodeWidthChar::width(c).unwrap_or(0))
                            .sum::<usize>()
                    })
                    .max()
                    .unwrap()
            })
            .max()
            .unwrap_or(0);
        let max_cell_width = max(max_cell_width, preamble.trim_end().len());

        write!(
            f,
            "{}",
            cell_strings
                .into_iter()
                .intersperse(if let Some(sep) = self.sep {
                    sep.to_string().repeat(max_cell_width) + "\n"
                } else {
                    "\n".to_string()
                })
                .collect::<String>()
        )
    }
}

impl<'l> Summary<'l> {
    fn code(&self) -> u32 {
        self.stage as u32
    }
}

#[cfg(test)]
mod tests {
    use super::{Summary, SummaryCell, SummaryComponent, SummaryStage, SummaryType};

    use colored::control::SHOULD_COLORIZE;
    use indoc::indoc;

    #[allow(clippy::too_many_arguments)]
    fn singleton_summary(
        filepath: &str,
        input: &str,
        stage: SummaryStage,
        stat: &str,
        title: Option<&str>,
        component_type: SummaryType,
        component_code: u32,
        expr: &str,
        message: &str,
        shorthand: &str,
        declaration: Option<&str>,
        note: Option<&str>,
    ) -> String {
        let mut summary = Summary::new(stage);
        summary.add_input_file(input, filepath.to_string());
        let mut cell = SummaryCell::new(stat);
        if let Some(title) = title {
            cell.set_title(title.to_string());
        }
        let mut component =
            SummaryComponent::new(component_type, component_code, expr, message.to_string())
                .set_shorthand(shorthand.to_string());
        if let Some(declaration) = declaration {
            component = component.set_declaration(declaration);
        }
        if let Some(note) = note {
            component = component.set_note(note.to_string());
        }
        cell.add_component(component);
        summary.add_cell(cell);
        format!("{}", summary)
    }

    fn assert_eq_multiline(source: String, target: String) {
        let source = source
            .lines()
            .map(|line| line.trim_end_matches(&[' ', '\n', '\t'][..]).to_string() + "\n")
            .collect::<String>();
        let target = target
            .lines()
            .map(|line| line.trim_end_matches(&[' ', '\n', '\t'][..]).to_string() + "\n")
            .collect::<String>();
        assert_eq!(source, target);
    }

    #[test]
    fn test_code() {
        assert_eq!(Summary::new(SummaryStage::Parser).code(), 100);
        assert_eq!(Summary::new(SummaryStage::Semantic).code(), 200);
    }

    #[test]
    fn test_unicode_alignment() {
        let input = "😊😊😊😊😊";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[4..16],
                None,
                SummaryType::Error,
                200,
                &input[4..12],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:2
                1. error[200]: message
                 1 | 😊😊😊😊😊
                   |   ^^^^
                   |   |
                   |   V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_end_span() {
        let input = "abc";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                input,
                None,
                SummaryType::Error,
                200,
                &input[3..3],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:4
                1. error[200]: message
                 1 | abc
                   |    ^
                   |    |
                   |    V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_multidot_lines() {
        let input = "ab\nc\nd\nef";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[1..8],
                None,
                SummaryType::Error,
                200,
                &input[7..8],
                "message",
                "",
                Some(&input[1..2]),
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:4:1
                1. error[200]: message
                 1 | ab
                   |  ^
                   |  |
                   |  V
                   | [1] first declared here
                 ...
                 4 | ef
                   | ^
                   | |
                   | V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_arrow_layout_1() {
        let input = "abcdef";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..5],
                None,
                SummaryType::Error,
                200,
                &input[3..5],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:4
                1. error[200]: message
                 1 | abcdef
                   |    ^^
                   |    |
                   |    V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_arrow_layout_2() {
        let input = "abcdefgh";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..7],
                None,
                SummaryType::Error,
                200,
                &input[4..5],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:5
                1. error[200]: message
                 1 | abcdefgh
                   |     ^
                   |     |
                   |     |
                   | [1] <
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_arrow_layout_3() {
        let input = "abcdefgh";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..7],
                None,
                SummaryType::Error,
                200,
                &input[5..6],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:6
                1. error[200]: message
                 1 | abcdefgh
                   |      ^
                   |      |
                   |      |
                   | [1] </
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_arrow_layout_4() {
        let input = "abcdefgh";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..7],
                None,
                SummaryType::Error,
                200,
                &input[6..7],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:7
                1. error[200]: message
                 1 | abcdefgh
                   |       ^
                   |       |
                   |       |
                   | [1] <-/
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_filename() {
        let input = "abc";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test/test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..3],
                None,
                SummaryType::Error,
                200,
                &input[0..3],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test/test.wacc:1:1
                1. error[200]: message
                 1 | abc
                   | ^^^
                   | |
                   | V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_stage() {
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            format!("{}", Summary::new(SummaryStage::Parser)),
            indoc! {"
                An erroneous statement has been found during parsing
            "}
            .to_string(),
        );
        assert_eq_multiline(
            format!("{}", Summary::new(SummaryStage::Semantic)),
            indoc! {"
                An erroneous statement has been found during semantic analysis
            "}
            .to_string(),
        );
    }

    #[test]
    fn test_title() {
        let input = "abc";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..3],
                Some("title"),
                SummaryType::Error,
                200,
                &input[0..3],
                "message",
                "",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:1 -> title
                1. error[200]: message
                 1 | abc
                   | ^^^
                   | |
                   | V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_shorthand() {
        let input = "abcdefg";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                input,
                None,
                SummaryType::Error,
                200,
                &input[4..],
                "message",
                "very long shorthand",
                None,
                None,
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:5
                1. error[200]: message
                 1 | abcdefg
                   |     ^^^
                   |     |
                   |     V
                   | [1] very long shorthand
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_note() {
        let input = "abc";
        SHOULD_COLORIZE.set_override(false);
        assert_eq_multiline(
            singleton_summary(
                "test.wacc",
                input,
                SummaryStage::Parser,
                &input[0..3],
                None,
                SummaryType::Error,
                200,
                &input[0..3],
                "message",
                "",
                None,
                Some("note"),
            ),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:1
                1. error[200]: message
                 1 | abc
                   | ^^^
                   | |
                   | V
                   | [1]
                 [1] note: note
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_spacing() {
        let input = "abc";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[0..1],
            String::default(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[1..2],
            String::default(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[2..3],
            String::default(),
        ));
        summary.add_cell(cell);
        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:1
                1. error[200]:
                2. error[200]:
                3. error[200]:
                 1 | a b c
                   | ^ ^ ^
                   | | | |
                   | V | |
                   | [1] |
                   |   | |
                   |   V |
                   | [2] |
                   |     |
                   |     |
                   | [3] <
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_main_message_alignment() {
        let input = "a b c";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[0..1],
            String::new(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Warning,
            200,
            &input[2..3],
            String::new(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            1,
            &input[4..5],
            String::new(),
        ));
        summary.add_cell(cell);
        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:1
                1.   error[200]: 
                2. warning[200]: 
                3.     error[1]: 
                 1 | a b c
                   | ^ ^ ^
                   | | | |
                   | V | |
                   | [1] |
                   |   | |
                   |   V |
                   | [2] |
                   |     |
                   |     |
                   | [3] <
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_multicomponent() {
        let input = "a b\nc";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[0..1],
            String::new(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[2..3],
            String::new(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[4..5],
            String::new(),
        ));
        summary.add_cell(cell);
        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:1
                1. error[200]:
                2. error[200]:
                3. error[200]:
                 1 | a b
                   | ^ ^
                   | | |
                   | V |
                   | [1]
                   |   |
                   |   V
                   | [2]
                   |
                 2 | c
                   | ^
                   | |
                   | V
                   | [3]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_multicell() {
        let input = "a b\nc";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        summary.set_sep('~');
        for _ in 0..3 {
            let mut cell = SummaryCell::new(input);
            cell.add_component(SummaryComponent::new(
                SummaryType::Error,
                200,
                &input[0..1],
                String::new(),
            ));
            summary.add_cell(cell);
        }

        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                3 erroneous statements have been found during parsing
                test.wacc:1:1
                1. error[200]:
                 1 | a b
                   | ^
                   | |
                   | V
                   | [1]
                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                test.wacc:1:1
                1. error[200]:
                 1 | a b
                   | ^
                   | |
                   | V
                   | [1]
                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                test.wacc:1:1
                1. error[200]:
                 1 | a b
                   | ^
                   | |
                   | V
                   | [1]
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_doubleline_expr() {
        let input = "a b\nc d";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[2..5],
            String::new(),
        ));
        summary.add_cell(cell);
        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:3
                1. error[200]:
                 1 | a b
                   |   ^
                   |   |
                   |   V
                   | [1]
                   |
                 2 | c d
                   | ^
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_multiline_expr() {
        let input = "a b\ncd\ne f";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[2..8],
            String::new(),
        ));
        summary.add_cell(cell);
        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                An erroneous statement has been found during parsing
                test.wacc:1:3
                1. error[200]:
                 1 | a b
                   |   ^
                   |   |
                   |   V
                   | [1]
                   |
                 2 | cd
                   | ^^
                   |
                 3 | e f
                   | ^
            "}
            .to_string(),
        )
    }

    #[test]
    fn test_multiple_files() {
        let input1 = "abc";
        let input2 = "def";
        let input3 = "ghi";
        SHOULD_COLORIZE.set_override(false);
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input1, "a.wacc".to_string());
        summary.add_input_file(input2, "b.wacc".to_string());
        summary.add_input_file(input3, "c.wacc".to_string());
        for input in [input1, input2] {
            let mut cell = SummaryCell::new(input);
            cell.add_component(
                SummaryComponent::new(SummaryType::Error, 200, &input[1..], String::new())
                    .set_declaration(&input3[1..]),
            );
            summary.add_cell(cell);
        }

        assert_eq_multiline(
            format!("{}", summary),
            indoc! {"
                2 erroneous statements have been found during parsing
                a.wacc:1:2
                1. error[200]:
                 1 | abc
                   |  ^^
                   |  |
                   |  V
                   | [1]
                 [1] first declared in c.wacc:1:2
                
                b.wacc:1:2
                1. error[200]:
                 1 | def
                   |  ^^
                   |  |
                   |  V
                   | [1]
                 [1] first declared in c.wacc:1:2
            "}
            .to_string(),
        )
    }

    #[test]
    #[should_panic(expected = "Overlapping spans in a summary cell")]
    fn test_overlapping_spans() {
        let input = "abc";
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(input, "test.wacc".to_string());
        let mut cell = SummaryCell::new(input);
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[..2],
            String::new(),
        ));
        cell.add_component(SummaryComponent::new(
            SummaryType::Error,
            200,
            &input[1..],
            String::new(),
        ));
        summary.add_cell(cell);
        format!("{}", summary);
    }

    #[test]
    #[should_panic(expected = "Statement not in any input file")]
    fn test_statement_not_in_input() {
        let input = "ab other";
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(&input[..1], "a".to_string());
        summary.add_input_file(&input[1..2], "b".to_string());
        summary.add_cell(SummaryCell::new(&input[3..]));
        format!("{}", summary);
    }

    #[test]
    #[should_panic(expected = "Declaration not in any input file")]
    fn test_declaration_not_in_input() {
        let input = "ab other";
        let mut summary = Summary::new(SummaryStage::Parser);
        summary.add_input_file(&input[..1], "a".to_string());
        summary.add_input_file(&input[1..2], "b".to_string());
        let mut cell = SummaryCell::new(&input[..1]);
        cell.add_component(
            SummaryComponent::new(SummaryType::Error, 200, &input[..1], String::new())
                .set_declaration(&input[3..]),
        );
        summary.add_cell(cell);
        format!("{}", summary);
    }
}
