# WACC Compiler - Group 33 
The WACC compiler project for group 33 composed of
- Jordan Hall
- Bartłomiej Cieślar
- Panayiotis Gavriil
- Oliver Killane

## Source Code
`/src` Contains the source code for the project, split into the `frontend` `backend` and 
an `intermediate` modules.

We recommend using [rust analyser for vscode](https://marketplace.visualstudio.com/items?itemName=matklad.rust-analyzer)
when viewing source as it provides type inferences from the compiler.

This project uses nightly rust, which allows for use of some unstable language 
features. This allows us to make use of mostly-stable `box` keyword, otherwise 
unstable features are avoided.

## Tests

Unit tests for modules are contained in nested modules. To run them simply use `cargo test`.
```rust
//! This is the module example.rs

// Some code here
...

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn example_test_1() {
    ...
  }
  ...

}
```

## Build
- To build without serious optimisations `cargo build`
- To build for release with optimisation `cargo build --release`

## Run
The compiler can be build and run with the `cargo run` command. 

When building for release, the binary will be `target/compile`

## Documentation
The documentation of the current master is available [here](https://lab2122_spring.pages.doc.ic.ac.uk/WACC_33) and is updated by the CI.

To build the documentation run `cargo doc`, this will create a website at 
`target/doc/compile/index.html` containing all documentation generated from 
the doc comments placed in our source.
