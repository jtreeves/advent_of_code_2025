use std::fs;
use std::path::Path;

/// Common utility functions for Rust solutions.
pub fn read_input<P: AsRef<Path>>(file_path: P) -> Result<Vec<String>, std::io::Error> {
    let content = fs::read_to_string(file_path)?;
    Ok(content.trim().lines().map(String::from).collect())
}

pub fn read_input_raw<P: AsRef<Path>>(file_path: P) -> Result<String, std::io::Error> {
    fs::read_to_string(file_path)
}

pub fn parse_ints(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect()
}

// Placeholder for additional common utilities
