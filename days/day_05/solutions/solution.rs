use std::fs;

fn solve(input_data: &str) -> (i64, i64) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    
    // Find blank line separator
    let blank_idx = lines.iter().position(|line| line.trim().is_empty()).unwrap_or(lines.len());
    
    // Parse ranges (first section)
    let mut ranges: Vec<(i64, i64)> = Vec::new();
    for i in 0..blank_idx {
        let line = lines[i].trim();
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split('-').collect();
        let start: i64 = parts[0].parse().unwrap();
        let end: i64 = parts[1].parse().unwrap();
        ranges.push((start, end));
    }
    
    // Parse IDs to check (second section)
    let mut ids: Vec<i64> = Vec::new();
    for i in (blank_idx + 1)..lines.len() {
        let line = lines[i].trim();
        if line.is_empty() {
            continue;
        }
        let id: i64 = line.parse().unwrap();
        ids.push(id);
    }
    
    // Part 1: Count how many IDs fall into any range
    let mut part1_count = 0;
    for id_val in &ids {
        for (start, end) in &ranges {
            if *start <= *id_val && *id_val <= *end {
                part1_count += 1;
                break;
            }
        }
    }
    
    // Part 2: Merge ranges and count total unique IDs covered
    // Sort ranges by start value
    ranges.sort_by_key(|r| r.0);
    
    // Merge overlapping/adjacent ranges
    let mut merged: Vec<(i64, i64)> = Vec::new();
    if !ranges.is_empty() {
        merged.push(ranges[0]);
        for (start, end) in ranges.iter().skip(1) {
            let last = merged.last_mut().unwrap();
            // Check if overlaps or is adjacent (start <= last.1 + 1)
            if *start <= last.1 + 1 {
                // Merge: update end to max of both ends
                last.1 = last.1.max(*end);
            } else {
                // No overlap, add as new range
                merged.push((*start, *end));
            }
        }
    }
    
    // Calculate total unique IDs covered
    let part2_total: i64 = merged.iter().map(|(start, end)| end - start + 1).sum();
    
    (part1_count, part2_total)
}

fn main() {
    let content = fs::read_to_string("../data/input.txt").expect("Failed to read file");
    let (part1, part2) = solve(&content);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
