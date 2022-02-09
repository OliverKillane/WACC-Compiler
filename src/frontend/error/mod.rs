//! Error display and formatting.
//!
//! All errors thrown by the parser and semantic analyser are displayed in a
//! consistent style highly reminiscent of Rust's own error messages.
//! - Can display multiple errors as highlighted sections of a statement.
//! - Can display arrows from errors to descriptions
//! - Can show original definitions
//! - Provides space for complex messages
//! - Supports use of all utf8 characters (e.g emojis) in error messages
mod fmt;
mod prelude;
mod span_utils;

pub use prelude::*;
