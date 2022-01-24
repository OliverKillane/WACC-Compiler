mod ast;
mod error;
mod parser;
mod semantic;

pub struct ParseError {
    exit_code: i32,
    message: String,
}

impl crate::ErrorType for ParseError {
    fn exit_code(&self) -> i32 {
        self.exit_code
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn analyse(source_code: &str) -> Result<(), ParseError> {
    Ok(())
}
