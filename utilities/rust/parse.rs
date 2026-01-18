/// Parsing utilities for Rust solutions.

/// Parse integers from a line of text.
/// Splits the line by whitespace and extracts all integers.
pub fn parse_ints(line: &str) -> Vec<i64> {
    line.split_whitespace()
        .filter_map(|s| s.parse().ok())
        .collect()
}
