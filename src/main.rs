use std::process::exit;

mod backend;
mod frontend;
mod intermediate;

trait ErrorType {
    fn exit_code(&self) -> i32;
}

fn main() {
    println!("Hello, world!");

    frontend::analyse("Source code").unwrap_or_else(|error| {
        println!("{}", error);
        exit(error.exit_code())
    });
}
