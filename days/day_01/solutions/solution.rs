mod utils {
    include!("../../../utilities/rust/get_input.rs");
}

use std::fs;

fn read_input_raw(file_path: &str) -> String {
    fs::read_to_string(file_path).expect("Failed to read file")
}

fn solve(lines: Vec<String>) -> (i32, i32) {
    // Part 1: Count times dial ends at 0 after a rotation
    let mut position = 50;
    let mut count_part1 = 0;
    
    for line in &lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        
        let direction = trimmed.chars().next().unwrap();
        let distance: i32 = trimmed[1..].parse().unwrap();
        
        // Apply rotation
        if direction == 'L' {
            position = ((position - distance) % 100 + 100) % 100;
        } else { // direction == 'R'
            position = (position + distance) % 100;
        }
        
        // Check if ended at 0
        if position == 0 {
            count_part1 += 1;
        }
    }
    
    // Part 2: Count times dial is at 0 during entire process
    position = 50;
    let mut count_part2 = 0;
    
    for line in &lines {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        
        let direction = trimmed.chars().next().unwrap();
        let distance: i32 = trimmed[1..].parse().unwrap();
        
        let start_pos = position;
        
        // Check each click position during rotation
        for click in 1..=distance {
            let click_pos = if direction == 'L' {
                ((start_pos - click) % 100 + 100) % 100
            } else { // direction == 'R'
                (start_pos + click) % 100
            };
            
            if click_pos == 0 {
                count_part2 += 1;
            }
        }
        
        // Update position after rotation
        if direction == 'L' {
            position = ((position - distance) % 100 + 100) % 100;
        } else {
            position = (position + distance) % 100;
        }
    }
    
    (count_part1, count_part2)
}

fn main() {
    let content = read_input_raw("../data/input.txt");
    let lines: Vec<String> = content
        .trim()
        .lines()
        .map(|s| s.trim().to_string())
        .collect();
    
    let (part1, part2) = solve(lines);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
