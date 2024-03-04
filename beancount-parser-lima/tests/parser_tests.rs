use rstest::rstest;
use std::path::PathBuf;

#[rstest]
fn test_parser(#[files("../test-cases/*.beancount")] path: PathBuf) {
    let test_name = path.file_stem().unwrap().to_string_lossy();
    check_parse(test_name);
}

use helpers::check_parse;
mod helpers;
