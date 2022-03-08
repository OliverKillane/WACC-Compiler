use super::error::SpanLocator;
use super::parser::parse_modules;
use path_absolutize::Absolutize;
use std::hash::Hash;
use std::{
    collections::{HashSet, LinkedList},
    fs,
    path::{Path, PathBuf},
};

#[derive(Eq)]
pub struct InputFile {
    pub filepath: PathBuf,
    contents: String,
    to_parse_start_idx: usize,
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

pub enum GatherModulesError {
    InvalidEncoding(PathBuf),
    MainFileNotPresent,
    ModuleNotPresent(PathBuf, PathBuf),
    InvalidModDecl(PathBuf, usize, usize),
}

fn process_component(filepath: PathBuf) -> Result<(InputFile, Vec<PathBuf>), GatherModulesError> {
    let contents =
        String::from_utf8(fs::read(&filepath).map_err(|_| GatherModulesError::MainFileNotPresent)?)
            .map_err(|_| GatherModulesError::InvalidEncoding(filepath.to_owned()))?;
    let contents_locator = SpanLocator::new(&contents);
    let (remaining, mods) = parse_modules(&contents).map_err(|loc| {
        let (line, column) = contents_locator.get_coords(loc);
        GatherModulesError::InvalidModDecl(filepath.to_owned(), line, column)
    })?;
    let (remaining_start_idx, _) = contents_locator.get_range(remaining);
    Ok((
        InputFile {
            filepath,
            contents,
            to_parse_start_idx: remaining_start_idx,
        },
        mods.into_iter()
            .map(|path| path.absolutize().unwrap().into_owned())
            .collect(),
    ))
}

pub fn gather_modules(
    main_file_path: String,
) -> Result<(InputFile, Vec<InputFile>), GatherModulesError> {
    let (main_input_file, module_analyze_queue) =
        process_component(Path::new(&main_file_path).to_owned())?;
    let mut analyzed_modules = HashSet::new();
    let mut module_analyze_queue = module_analyze_queue.into_iter().collect::<LinkedList<_>>();
    while let Some(next_module) = module_analyze_queue.pop_front() {
        let (input_file, modules) = process_component(next_module)?;
        analyzed_modules.insert(input_file);
        module_analyze_queue.append(&mut modules.into_iter().collect());
    }

    Ok((main_input_file, analyzed_modules.into_iter().collect()))
}
