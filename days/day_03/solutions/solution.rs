use std::fs;

// Find the largest N-digit number by selecting N digits in order from bank
fn find_largest_subsequence(bank: &str, n: usize) -> u64 {
    let bank_len = bank.len();
    if bank_len < n {
        return 0;
    }
    
    let mut result = Vec::new();
    let mut start = 0;
    
    for i in 0..n {
        let remaining_needed = n - i - 1;
        let end = bank_len - remaining_needed;
        
        let mut max_digit = bank.chars().nth(start).unwrap();
        let mut max_pos = start;
        for j in (start + 1)..end {
            let ch = bank.chars().nth(j).unwrap();
            if ch > max_digit {
                max_digit = ch;
                max_pos = j;
            }
        }
        
        result.push(max_digit);
        start = max_pos + 1;
    }
    
    result.iter().collect::<String>().parse::<u64>().unwrap_or(0)
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    
    let mut part1_sum: u64 = 0;
    let mut part2_sum: u64 = 0;
    
    for line in lines {
        let bank = line.trim();
        if bank.is_empty() {
            continue;
        }
        
        part1_sum += find_largest_subsequence(bank, 2);
        
        if bank.len() >= 12 {
            part2_sum += find_largest_subsequence(bank, 12);
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
