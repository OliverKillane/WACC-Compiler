#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![allow(dead_code)]
//! The group 33 WACC Compiler project.
//!
//! Lovingly developed by:
//! - Jordan Hall
//! - Bartłomiej Cieślar
//! - Panayiotis Gavriil
//! - Oliver Killane

mod backend;
mod frontend;
mod intermediate;

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
fn main() {
    todo!()
}
