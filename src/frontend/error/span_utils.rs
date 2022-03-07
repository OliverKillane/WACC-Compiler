//! Contains utility functions for determining the placement of spans within
//! the original source code string.

use std::{
    cmp::min,
    collections::{BTreeMap, HashMap},
};

use super::InputFile;

/// Returns the range of the indices of the span within the input string, provided
/// the span starts somewhere within the input string.
pub(super) fn get_relative_range(input: &str, span: &str) -> Option<(usize, usize)> {
    let input_begin = input.as_ptr() as usize;
    let input_end = input_begin + input.len();

    let span_begin = span.as_ptr() as usize;
    let span_end = span_begin + span.len();

    if input_begin > span_begin || input_end < span_begin {
        None
    } else {
        Some((
            span_begin - input_begin,
            min(span_end, input_end) - input_begin,
        ))
    }
}

/// Span locator. Manages calculations regarding positioning of a span within
/// the input string, as well as calculating the line at which the span starts.
pub(super) struct SpanLocator<'l> {
    input: &'l str,
    input_lines: Vec<(usize, usize, bool)>,
    input_lines_characters_map: Vec<HashMap<usize, usize>>,
}

impl<'l> SpanLocator<'l> {
    /// Creates a new span locator for a given input.
    pub fn new(input: &'l str) -> Self {
        SpanLocator {
            input,
            input_lines: input
                .lines()
                .map(|line| {
                    let (line_begin, line_end) = get_relative_range(input, line).unwrap();
                    (
                        line_begin,
                        min(line_end + 1, input.len()),
                        line_end < input.len(),
                    ) // Because lines() doesn't include newlines
                })
                .collect(),
            input_lines_characters_map: input
                .lines()
                .map(|line| {
                    let mut characters_map = HashMap::new();
                    let mut characters_count = 0;
                    for (column, (char_index, _)) in line.char_indices().enumerate() {
                        characters_map.insert(char_index, column);
                        characters_count += 1;
                    }
                    characters_map.insert(line.len(), characters_count);
                    characters_map
                })
                .collect(),
        }
    }

    /// Calculates the range of the indices of the span within the input string,
    /// provided the span starts somewhere within the input string.
    pub fn get_optional_range(&self, span: &str) -> Option<(usize, usize)> {
        get_relative_range(self.input, span)
    }

    /// Calculates the range of indices of the span within the input string.
    /// Panics if the span is not within the input string
    pub fn get_range(&self, span: &str) -> (usize, usize) {
        self.get_optional_range(span)
            .expect("Span not within the input string")
    }

    /// Calculates the line number at which the span starts, provided that it
    /// starts somewhere within the input string.
    pub fn get_optional_line_num(&self, span: &str) -> Option<usize> {
        let (input_pos, _) = self.get_optional_range(span)?;
        let line_num = self
            .input_lines
            .binary_search_by_key(&input_pos, |(line_start, _, _)| *line_start)
            .unwrap_or_else(|idx| idx - 1);
        assert!(line_num < self.input_lines.len());
        Some(line_num)
    }

    /// Calculates the line number at which the span starts. Panics if the span
    /// is not within any line.
    pub fn get_line_num(&self, span: &str) -> usize {
        self.get_optional_line_num(span)
            .expect("Span does not correspond to any line in the input string")
    }

    /// Returns a specific line in the input, provided such a line index exists
    /// within the input.
    pub fn get_optional_input_line(&self, line_num: usize) -> Option<&str> {
        let (line_begin, line_end, ends_with_newline) = self.input_lines.get(line_num)?;
        Some(&self.input[*line_begin..*line_end - *ends_with_newline as usize])
    }

    /// Returns a specific line in the input. Panics if the line number is out
    /// of bounds
    pub fn get_input_line(&self, line_num: usize) -> &str {
        self.get_optional_input_line(line_num)
            .expect("Line number out of bounds")
    }

    /// Gets the row and the column at which the span starts within the input string,
    /// provided the span is within the input string.
    pub fn get_optional_coords(&self, span: &str) -> Option<(usize, usize)> {
        let line_num = self.get_optional_line_num(span)?;

        let (line_pos, _) = get_relative_range(self.get_input_line(line_num), span).unwrap();
        Some((
            line_num,
            self.input_lines_characters_map[line_num][&line_pos],
        ))
    }

    /// Gets the row and the column at which the span starts within the input string.
    /// Panics if the span is not within the input string.
    pub fn get_coords(&self, span: &str) -> (usize, usize) {
        self.get_optional_coords(span)
            .expect("Span not within the input string")
    }
}

pub(super) struct MultiInputLocator<'l> {
    locators: Vec<(usize, String, SpanLocator<'l>)>,
}

impl<'l> MultiInputLocator<'l> {
    pub fn new(input_files: &[InputFile<'l>]) -> Self {
        let mut locators: Vec<_> = input_files
            .iter()
            .cloned()
            .map(|InputFile { input, filepath }| {
                (input.as_ptr() as usize, filepath, SpanLocator::new(input))
            })
            .collect();
        locators.sort_by_key(|(input_start, _, _)| *input_start);

        Self { locators }
    }

    fn get_locator_filepath<'s>(
        &'s self,
        span: &'l str,
    ) -> Option<(&'s SpanLocator<'l>, &'s String)> {
        let locator_idx = self
            .locators
            .binary_search_by_key(&(span.as_ptr() as usize), |(input_start, _, _)| {
                *input_start
            })
            .unwrap_or_else(|idx| idx - 1);
        let (_, filename, locator) = self.locators.get(locator_idx)?;
        locator
            .get_optional_range(span)
            .map(|_| (locator, filename))
    }

    pub fn get_optional_locator<'loc>(&'loc self, span: &'l str) -> Option<&'loc SpanLocator<'l>> {
        self.get_locator_filepath(span).map(|(locator, _)| locator)
    }

    pub fn get_locator<'loc>(&'loc self, span: &'l str) -> &'loc SpanLocator<'l> {
        self.get_optional_locator(span)
            .expect("Span not in any input")
    }

    pub fn get_optional_filepath<'path>(&'path self, span: &'l str) -> Option<&'path str> {
        self.get_locator_filepath(span)
            .map(|(_, filepath)| &filepath[..])
    }

    pub fn get_filepath<'path>(&'path self, span: &'l str) -> &'path str {
        self.get_optional_filepath(span)
            .expect("Span not in any input")
    }
}

#[cfg(test)]
mod tests {
    use super::{super::InputFile, get_relative_range, MultiInputLocator, SpanLocator};

    #[test]
    fn get_relative_range_test() {
        let s = "1 2 3 4 5 6 7 8";
        let a = &s[0..5];
        let b = &s[3..7];
        let c = &s[8..15];
        assert_eq!(get_relative_range(s, a).unwrap(), (0, 5));
        assert_eq!(get_relative_range(&s[2..5], b).unwrap(), (1, 3));
        assert_eq!(get_relative_range(s, c).unwrap(), (8, 15));
        assert_eq!(get_relative_range(s, "other"), None);
    }

    #[test]
    fn span_get_range_test() {
        let s = "1 2 3 4 5 6 7 8";
        let a = &s[0..5];
        let b = &s[8..15];
        let locator = SpanLocator::new(s);
        assert_eq!(locator.get_optional_range(a), Some((0, 5)));
        assert_eq!(locator.get_optional_range(b), Some((8, 15)));
        assert_eq!(locator.get_optional_range("other"), None);
    }

    #[test]
    fn span_get_line_num_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[4..7];
        let locator = SpanLocator::new(s);
        assert_eq!(locator.get_optional_line_num(a), Some(0));
        assert_eq!(locator.get_optional_line_num(b), Some(2));
        assert_eq!(locator.get_optional_line_num("other"), None);
    }

    #[test]
    fn span_get_input_line_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[4..7];
        let locator = SpanLocator::new(s);
        assert_eq!(
            locator.get_optional_input_line(locator.get_line_num(a)),
            Some("1")
        );
        assert_eq!(
            locator.get_optional_input_line(locator.get_line_num(b)),
            Some("345")
        );
        assert_eq!(locator.get_optional_input_line(4), None);
    }

    #[test]
    fn span_get_coords_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[5..7];
        let locator = SpanLocator::new(s);
        assert_eq!(locator.get_optional_coords(a), Some((0, 0)));
        assert_eq!(locator.get_optional_coords(b), Some((2, 1)));
        assert_eq!(locator.get_optional_coords("other"), None);
    }

    #[test]
    fn multi_locator_get_filepath() {
        let s = ["a", "b", "c", "d", "e"];
        let input_files = s.map(|s| InputFile::new(s, s.to_string()));
        let multi_locator = MultiInputLocator::new(&input_files);
        for input in s {
            assert_eq!(multi_locator.get_optional_filepath(input), Some(input));
        }
    }
}
