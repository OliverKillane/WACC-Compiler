use std::{cmp::min, slice::SliceIndex};

pub(super) fn get_relative_range(input: &str, span: &str) -> Option<(usize, usize)> {
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

pub(super) struct SpanLocator<'l> {
    input: &'l str,
    input_lines: Vec<(usize, usize)>,
}

impl<'l> SpanLocator<'l> {
    pub fn new(input: &'l str) -> Self {
        SpanLocator {
            input,
            input_lines: input
                .lines()
                .map(|line| get_relative_range(input, line).unwrap())
                .collect(),
        }
    }

    pub fn get_range(&self, span: &str) -> Option<(usize, usize)> {
        get_relative_range(self.input, span)
    }

    pub fn get_line_num(&self, span: &str) -> Option<usize> {
        let (input_pos, _) = self.get_range(span)?;
        let line_num = self
            .input_lines
            .binary_search_by(|(line_start, _)| line_start.cmp(&input_pos))
            .unwrap_or_else(|idx| idx - 1);
        if line_num != usize::MAX {
            Some(line_num)
        } else {
            None
        }
    }

    pub fn get_input_line(&self, line_num: usize) -> Option<&str> {
        let (line_begin, line_end) = self.input_lines.get(line_num)?;
        Some(&self.input[*line_begin..*line_end])
    }

    pub fn get_coords(&self, span: &str) -> Option<(usize, usize)> {
        let line_num = self.get_line_num(span)?;

        let (line_pos, _) = SpanLocator {
            input: &self.input[self.input_lines[line_num].0..self.input_lines[line_num].1],
            input_lines: Vec::new(),
        }
        .get_range(span)?;
        Some((line_num, line_pos))
    }
}

#[cfg(test)]
mod test {
    use super::get_relative_range;
    use super::SpanLocator;

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
        assert_eq!(locator.get_range(a).unwrap(), (0, 5));
        assert_eq!(locator.get_range(b).unwrap(), (8, 15));
        assert_eq!(locator.get_range("other"), None);
    }

    #[test]
    fn span_get_line_num_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[4..7];
        let locator = SpanLocator::new(s);
        assert_eq!(locator.get_line_num(a).unwrap(), 0);
        assert_eq!(locator.get_line_num(b).unwrap(), 2);
        assert_eq!(locator.get_line_num("other"), None);
    }

    #[test]
    fn span_get_input_line_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[4..7];
        let locator = SpanLocator::new(s);
        assert_eq!(
            locator
                .get_input_line(locator.get_line_num(a).unwrap())
                .unwrap(),
            "1"
        );
        assert_eq!(
            locator
                .get_input_line(locator.get_line_num(b).unwrap())
                .unwrap(),
            "345"
        );
        assert_eq!(locator.get_input_line(4), None);
    }

    #[test]
    fn span_get_coords_test() {
        let s = "1\n2\n345\n6\n";
        let a = &s[0..1];
        let b = &s[5..7];
        let locator = SpanLocator::new(s);
        assert_eq!(locator.get_coords(a).unwrap(), (0, 0));
        assert_eq!(locator.get_coords(b).unwrap(), (2, 1));
        assert_eq!(locator.get_coords("other"), None);
    }
}
