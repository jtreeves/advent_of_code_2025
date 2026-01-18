use std::fs;
use std::path::Path;

/// Input reading utilities for Rust solutions.

/// Read input from input.txt for the given day.
/// Returns lines as a vector of strings.
pub fn get_input(day: i32) -> Result<Vec<String>, std::io::Error> {
    let path = get_input_path(day);
    read_input(&path)
}

/// Read input from test_N.txt for the given day and test number.
/// Returns lines as a vector of strings.
pub fn get_test_input(day: i32, test_num: i32) -> Result<Vec<String>, std::io::Error> {
    let path = get_test_input_path(day, test_num);
    read_input(&path)
}

/// Return the path to input.txt for the given day.
/// Solutions are in days/day_NN/solutions/
/// Input is in days/day_NN/data/input.txt
pub fn get_input_path(day: i32) -> String {
    "../data/input.txt".to_string()
}

/// Return the path to test_N.txt for the given day and test number.
pub fn get_test_input_path(day: i32, test_num: i32) -> String {
    format!("../data/test_{}.txt", test_num)
}

/// Read input file and return lines as a vector of strings.
pub fn read_input<P: AsRef<Path>>(file_path: P) -> Result<Vec<String>, std::io::Error> {
    let content = fs::read_to_string(file_path)?;
    Ok(content
        .trim()
        .lines()
        .map(|s| s.trim().to_string())
        .collect())
}

/// Read input file and return raw content as a string.
pub fn read_input_raw<P: AsRef<Path>>(file_path: P) -> Result<String, std::io::Error> {
    fs::read_to_string(file_path)
}
