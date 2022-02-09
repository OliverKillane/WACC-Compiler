#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(map_try_insert)]
#![feature(iter_intersperse)]
#![allow(dead_code)]
//! The group 33 WACC Compiler project.
//!
//! Lovingly developed by:
//! - Jordan Hall
//! - Bartłomiej Cieślar
//! - Panayiotis Gavriil
//! - Oliver Killane

#[macro_use]
extern crate lazy_static;

mod backend;
mod frontend;
mod intermediate;

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
fn main() {
    todo!()
}
