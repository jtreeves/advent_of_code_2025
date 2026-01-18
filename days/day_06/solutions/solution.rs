mod utils {
    include!("../../../utilities/rust/get_input.rs");
}

use std::fs;

struct Problem {
    start_col: usize,
    end_col: usize,
    op: char,
}

fn read_input_raw(file_path: &str) -> String {
    fs::read_to_string(file_path).expect("Failed to read file")
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    if lines.is_empty() {
        return ("0".to_string(), "0".to_string());
    }

    // Find maximum line length and pad all lines
    let max_len = lines.iter().map(|line| line.len()).max().unwrap_or(0);
    let mut padded_lines: Vec<String> = lines.iter()
        .map(|line| format!("{:<width$}", line, width = max_len))
        .collect();

    // Operator row is the last row
    let op_row_idx = padded_lines.len() - 1;
    let op_row: Vec<char> = padded_lines[op_row_idx].chars().collect();
    let num_rows: Vec<Vec<char>> = padded_lines[..op_row_idx]
        .iter()
        .map(|line| line.chars().collect())
        .collect();

    // Part 1: Parse horizontally
    let mut part1_total: i64 = 0;

    // Find problem boundaries (columns that are all spaces)
    let is_space_col: Vec<bool> = (0..max_len)
        .map(|col| padded_lines.iter().all(|line| {
            line.chars().nth(col).unwrap_or(' ') == ' '
        }))
        .collect();

    // Group columns into problems
    let mut problems: Vec<Problem> = Vec::new();
    let mut i = 0;
    while i < max_len {
        if !is_space_col[i] {
            // Start of a problem
            let start_col = i;
            while i < max_len && !is_space_col[i] {
                i += 1;
            }
            let end_col = i;
            // Extract operator for this problem
            let mut op: Option<char> = None;
            for j in start_col..end_col {
                if j < op_row.len() && (op_row[j] == '+' || op_row[j] == '*') {
                    op = Some(op_row[j]);
                    break;
                }
            }
            if let Some(op_char) = op {
                problems.push(Problem {
                    start_col,
                    end_col,
                    op: op_char,
                });
            }
        } else {
            i += 1;
        }
    }

    // Solve Part 1: Extract numbers horizontally
    for prob in &problems {
        let mut numbers: Vec<i64> = Vec::new();
        // Extract numbers from each row in this problem area
        for row in &num_rows {
            if prob.end_col > row.len() {
                continue;
            }
            // Extract the substring for this problem
            let problem_str: String = row[prob.start_col..prob.end_col]
                .iter()
                .collect::<String>()
                .trim()
                .to_string();
            // Split by spaces and parse numbers
            for part in problem_str.split_whitespace() {
                if let Ok(num) = part.parse::<i64>() {
                    numbers.push(num);
                }
            }
        }

        // Apply operator
        if !numbers.is_empty() {
            let result: i64 = if prob.op == '+' {
                numbers.iter().sum()
            } else {
                numbers.iter().product()
            };
            part1_total += result;
        }
    }

    // Part 2: Parse vertically (columns, right-to-left)
    // Approach: Use iterator chains to transpose and parse
    let mut part2_total: i64 = 0;
    
    for prob in &problems {
        // Collect column strings (transposed rows)
        let mut col_strings: Vec<String> = Vec::new();
        for col in prob.start_col..prob.end_col {
            if col < is_space_col.len() && is_space_col[col] {
                continue;
            }
            let col_str: String = num_rows.iter()
                .filter_map(|row| row.get(col).copied())
                .filter(|&ch| ch.is_ascii_digit() || ch == ' ')
                .collect();
            if !col_str.trim().is_empty() {
                col_strings.push(col_str.trim().to_string());
            }
        }
        
        // Parse numbers from column strings (right-to-left means reverse order)
        let numbers: Vec<i64> = col_strings.iter()
            .rev()
            .filter_map(|s| {
                let digits: String = s.chars().filter(|c| c.is_ascii_digit()).collect();
                digits.parse::<i64>().ok()
            })
            .collect();
        
        // Apply operator
        if !numbers.is_empty() {
            let result: i64 = if prob.op == '+' {
                numbers.iter().sum()
            } else {
                numbers.iter().product()
            };
            part2_total += result;
        }
    }

    (part1_total.to_string(), part2_total.to_string())
}

fn main() {
    let content = read_input_raw("../data/input.txt");
    let (part1, part2) = solve(&content);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
