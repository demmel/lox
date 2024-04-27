#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        (self.end_line - self.start_line) * 80 + self.end_column - self.start_column
    }
}

impl std::ops::Add<Span> for Span {
    type Output = Span;

    fn add(self, other: Span) -> Span {
        let start = if self.start_line < other.start_line {
            &self
        } else {
            &other
        };
        let end = if self.end_line > other.end_line {
            &self
        } else {
            &other
        };

        Span {
            start_line: start.start_line,
            start_column: start.start_column,
            end_line: end.end_line,
            end_column: end.end_column,
        }
    }
}
