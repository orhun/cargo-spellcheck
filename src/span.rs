//! Span annotation independent of proc_macro2

pub use proc_macro2::LineColumn;

use std::hash::{Hash, Hasher};

/// Relative span in relation
/// to the beginning of a doc comment.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
// TODO ,Eq,PartialEq,PartialOrd,Ord
pub struct RelativeSpan {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Hash for RelativeSpan {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.line.hash(state);
        self.start.column.hash(state);
        self.end.line.hash(state);
        self.end.column.hash(state);
    }
}

// Span in relation to a full Document
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.line.hash(state);
        self.start.column.hash(state);
        self.end.line.hash(state);
        self.end.column.hash(state);
    }
}

use std::convert::TryInto;

use crate::Range;

impl TryInto<Range> for Span {
    type Error = anyhow::Error;
    fn try_into(self) -> Result<Range, Self::Error> {
        if self.start.line == self.end.line {
            Ok(Range {start : self.start.column, end: self.end.column})
        } else {
            Err(anyhow::anyhow!("Start and end are not in the same line {} vs {}", self.start.line, self.end.line))
        }
    }
}