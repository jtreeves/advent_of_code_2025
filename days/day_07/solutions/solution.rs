mod utils {
    include!("../../../utilities/rust/get_input.rs");
}

use std::fs;
use std::collections::HashSet;

fn read_input_raw(file_path: &str) -> String {
    fs::read_to_string(file_path).expect("Failed to read file")
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    if lines.is_empty() {
        return ("0".to_string(), "0".to_string());
    }
    
    let grid: Vec<Vec<char>> = lines.iter().map(|line| line.chars().collect()).collect();
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };
    
    // Find starting position S
    let mut start_row = 0;
    let mut start_col = 0;
    let mut found = false;
    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == 'S' {
                start_row = r;
                start_col = c;
                found = true;
                break;
            }
        }
        if found {
            break;
        }
    }
    
    if !found {
        return ("0".to_string(), "0".to_string());
    }
    
    // Part 1: Count total splits
    let mut split_count = 0;
    let mut active_beams = HashSet::new();
    active_beams.insert(start_col);
    
    // Process each row starting from the row after S
    for r in (start_row + 1)..rows {
        let mut next_beams = HashSet::new();
        for &col in &active_beams {
            if grid[r][col] == '.' {
                // Beam continues down
                next_beams.insert(col);
            } else if grid[r][col] == '^' {
                // Beam splits
                split_count += 1;
                // Add beams to left and right
                if col > 0 {
                    next_beams.insert(col - 1);
                }
                if col + 1 < cols {
                    next_beams.insert(col + 1);
                }
            }
        }
        active_beams = next_beams;
    }
    
    // Part 2: Count beams reaching bottom row
    let mut beam_counts = vec![vec![0i64; cols]; rows];
    beam_counts[start_row][start_col] = 1; // Start with 1 beam at S
    
    // Process each row starting from the row after S
    for r in (start_row + 1)..rows {
        for c in 0..cols {
            let prev_count = beam_counts[r - 1][c];
            if prev_count > 0 {
                if grid[r][c] == '.' {
                    // Beam continues down
                    beam_counts[r][c] += prev_count;
                } else if grid[r][c] == '^' {
                    // Beam splits into left and right
                    if c > 0 {
                        beam_counts[r][c - 1] += prev_count;
                    }
                    if c + 1 < cols {
                        beam_counts[r][c + 1] += prev_count;
                    }
                }
            }
        }
    }
    
    // Sum all beams in bottom row
    let bottom_beam_count: i64 = beam_counts[rows - 1].iter().sum();
    
    (split_count.to_string(), bottom_beam_count.to_string())
}

fn main() {
    let content = read_input_raw("../data/input.txt");
    let (part1, part2) = solve(&content);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
