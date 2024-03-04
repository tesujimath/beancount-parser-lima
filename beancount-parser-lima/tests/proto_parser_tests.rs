use rstest::rstest;
use std::path::PathBuf;

#[rstest]
fn test_parser(#[files("../test-cases/*.beancount")] path: PathBuf) {
    let test_name = path.file_stem().unwrap().to_string_lossy();
    check_parse(test_name);
    // panic!("testing {} from {:?}", test_name, &path);
}

use proto::check_parse;

mod proto;
