use std::fs;

// Check if ID is invalid for Part 1: exactly two identical sequences
fn is_invalid_part1(id_str: &str) -> bool {
    let n = id_str.len();
    if n % 2 != 0 {
        return false;
    }
    let half = n / 2;
    &id_str[..half] == &id_str[half..]
}

// Check if ID is invalid for Part 2: sequence repeated 2+ times
fn is_invalid_part2(id_str: &str) -> bool {
    let n = id_str.len();
    for k in 2..=n {
        if n % k == 0 {
            let seq_len = n / k;
            let pattern = &id_str[..seq_len];
            let repeated: String = pattern.repeat(k);
            if id_str == repeated {
                return true;
            }
        }
    }
    false
}

// Parse a range string like "start-end"
fn parse_range(range_str: &str) -> (i64, i64) {
    let parts: Vec<&str> = range_str.split('-').collect();
    let start: i64 = parts[0].trim().parse().unwrap_or(0);
    let end: i64 = parts[1].trim().parse().unwrap_or(0);
    (start, end)
}

// Parse a line of comma-separated ranges
fn parse_ranges(line: &str) -> Vec<(i64, i64)> {
    line.split(',')
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(parse_range)
        .collect()
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    
    let mut part1_sum: i64 = 0;
    let mut part2_sum: i64 = 0;
    
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        let ranges = parse_ranges(line);
        
        for (start, end) in ranges {
            for num in start..=end {
                let id_str = num.to_string();
                
                if is_invalid_part1(&id_str) {
                    part1_sum += num;
                }
                
                if is_invalid_part2(&id_str) {
                    part2_sum += num;
                }
            }
        }
    }
    
    (part1_sum.to_string(), part2_sum.to_string())
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Error reading file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
