use std::fs;

// Placeholder for Day 7 Rust solution
fn solve(input_data: &str) -> (String, String) {
    println!("Day 7 Rust placeholder");
    let lines: Vec<&str> = input_data.trim().lines().collect();
    
    // Part 1
    let part1_result = "TODO".to_string();
    
    // Part 2
    let part2_result = "TODO".to_string();
    
    (part1_result, part2_result)
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Error reading file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
