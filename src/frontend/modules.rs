use super::error::SpanLocator;
use super::parser::parse_imports;
use path_absolutize::Absolutize;
use std::hash::Hash;
use std::{
    collections::{HashSet, LinkedList},
    fs,
    path::{Path, PathBuf},
};

/// Gathered input wacc file
#[derive(Eq)]
pub struct InputFile {
    /// Absolute path to the file, with no going up the directory strucutre.
    pub filepath: PathBuf,
    /// Contents of the entire file.
    pub contents: String,
    /// The index in the contents of the file where the module parsing stopped.
    to_parse_start_idx: usize,
}

impl InputFile {
    /// The entire contents of the file, from start to end
    pub fn contents(&self) -> &str {
        &self.contents
    }

    /// The part of the file contents that the main parser parses
    pub(super) fn to_parse_contents(&self) -> &str {
        &self.contents[self.to_parse_start_idx..]
    }
}

impl PartialEq<InputFile> for InputFile {
    fn eq(&self, other: &InputFile) -> bool {
        self.filepath == other.filepath
    }
}

impl Hash for InputFile {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.filepath.hash(state);
    }
}

/// Error type for problems with gathering modules.
#[derive(Debug)]
pub enum GatherModulesError {
    /// Main file is not present at all
    MainFileNotPresent,
    /// No actual code besides imports in the main file
    MainFileNoCode,
    /// A module is not encoded with a utf-8 encoding.
    InvalidEncoding(PathBuf),
    /// A mod declaration in a specific file, line and column is wrongly formatted.
    InvalidModDecl(PathBuf, usize, usize),
    /// A module tries to import another module that does not exist under this
    /// relative path.
    ModuleNotPresent(PathBuf, PathBuf),
}

/// Extracts import paths from a single component
fn process_component(filepath: PathBuf) -> Result<(InputFile, Vec<PathBuf>), GatherModulesError> {
    let contents =
        String::from_utf8(fs::read(&filepath).map_err(|_| GatherModulesError::MainFileNotPresent)?)
            .map_err(|_| GatherModulesError::InvalidEncoding(filepath.to_owned()))?;
    let contents_locator = SpanLocator::new(&contents);
    let (remaining, mods) = parse_imports(&contents).map_err(|loc| {
        let (line, column) = contents_locator.get_coords(loc);
        GatherModulesError::InvalidModDecl(filepath.to_owned(), line, column)
    })?;
    let (remaining_start_idx, _) = contents_locator.get_range(remaining);
    println!(
        "remaining idx {}, len {}",
        remaining_start_idx,
        contents.len()
    );
    Ok((
        InputFile {
            filepath,
            contents,
            to_parse_start_idx: remaining_start_idx,
        },
        mods,
    ))
}

/// Chains the main module path with all import paths, making them relative to the
/// main module.
pub fn chain_modules(module: &Path, imports: Vec<PathBuf>) -> LinkedList<(PathBuf, PathBuf)> {
    let mut module = module.to_owned();
    module.pop();
    imports
        .into_iter()
        .map(|import| {
            (
                module.clone(),
                module.join(import).absolutize().unwrap().into_owned(),
            )
        })
        .collect()
}

/// Gathers all the modules imported directly or indirectly from the main file.
pub fn gather_modules(
    main_file_path: &Path,
) -> Result<(InputFile, Vec<InputFile>), GatherModulesError> {
    let main_file_path = main_file_path
        .absolutize()
        .map_err(|_| GatherModulesError::MainFileNotPresent)?
        .into_owned();
    let (main_input_file, main_imports) = process_component(main_file_path)?;
    if main_input_file.to_parse_contents().is_empty() {
        return Err(GatherModulesError::MainFileNoCode);
    }
    let mut analyzed_modules = Vec::new();
    let mut analyzed_modules_paths = HashSet::new();
    let mut module_analyze_queue = chain_modules(&main_input_file.filepath, main_imports);
    while let Some((prev_module, next_module)) = module_analyze_queue.pop_front() {
        if analyzed_modules_paths.contains(&next_module) {
            continue;
        }
        let (input_file, imports) = process_component(next_module.clone()).map_err(|err| {
            if let GatherModulesError::MainFileNotPresent = err {
                GatherModulesError::ModuleNotPresent(prev_module, next_module.clone())
            } else {
                err
            }
        })?;
        analyzed_modules.push(input_file);
        module_analyze_queue.append(&mut chain_modules(&next_module, imports));
        analyzed_modules_paths.insert(next_module);
    }

    Ok((main_input_file, analyzed_modules.into_iter().collect()))
}
