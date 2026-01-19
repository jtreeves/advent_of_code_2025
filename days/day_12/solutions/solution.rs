use std::fs;

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    if lines.is_empty() {
        return ("0".to_string(), "Final star".to_string());
    }
    
    // Parse shapes (first 6 shapes, numbered 0-5)
    let mut shape_areas = vec![0; 6];
    let mut i = 0;
    let mut shape_idx = 0;
    
    while i < lines.len() && shape_idx < 6 {
        let line = lines[i].trim();
        // Check if this is a shape header (format: "number:")
        if line.ends_with(':') {
            if let Ok(shape_num) = line[..line.len()-1].parse::<usize>() {
                if shape_num == shape_idx {
                    // Read the next 3 lines for the shape grid
                    let mut shape_grid = Vec::new();
                    for j in 0..3 {
                        if i + 1 + j < lines.len() {
                            shape_grid.push(lines[i + 1 + j].trim());
                        } else {
                            shape_grid.push("");
                        }
                    }
                    
                    // Count '#' characters in the shape
                    let mut area = 0;
                    for row in &shape_grid {
                        area += row.chars().filter(|&c| c == '#').count();
                    }
                    shape_areas[shape_idx] = area;
                    shape_idx += 1;
                    i += 4; // Skip shape header + 3 grid lines + empty line (if present)
                    continue;
                }
            }
        }
        i += 1;
    }
    
    // Find where queries start (skip empty lines after shapes)
    let mut query_start = i;
    while query_start < lines.len() && lines[query_start].trim().is_empty() {
        query_start += 1;
    }
    
    // Parse queries
    let mut possible_count = 0;
    for line_idx in query_start..lines.len() {
        let line = lines[line_idx].trim();
        if line.is_empty() {
            continue;
        }
        
        // Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        if !line.contains(':') {
            continue;
        }
        
        let parts: Vec<&str> = line.splitn(2, ':').collect();
        if parts.len() != 2 {
            continue;
        }
        
        // Parse dimensions
        let dims = parts[0].trim();
        if !dims.contains('x') {
            continue;
        }
        
        let dim_parts: Vec<&str> = dims.split('x').collect();
        if dim_parts.len() != 2 {
            continue;
        }
        
        let width: i64 = match dim_parts[0].parse() {
            Ok(w) => w,
            Err(_) => continue,
        };
        let height: i64 = match dim_parts[1].parse() {
            Ok(h) => h,
            Err(_) => continue,
        };
        
        // Parse counts
        let count_parts: Vec<&str> = parts[1].trim().split_whitespace().collect();
        if count_parts.len() != 6 {
            continue;
        }
        
        let mut counts = vec![0; 6];
        let mut valid_counts = true;
        for (j, c) in count_parts.iter().enumerate() {
            match c.parse::<i64>() {
                Ok(count) => counts[j] = count,
                Err(_) => {
                    valid_counts = false;
                    break;
                }
            }
        }
        
        if !valid_counts {
            continue;
        }
        
        // Calculate area check
        let region_area = width * height;
        let mut required_area: i64 = 0;
        for j in 0..6 {
            required_area += shape_areas[j] as i64 * counts[j];
        }
        
        if required_area <= region_area {
            possible_count += 1;
        }
    }
    
    // Part 2: Final star (no computation needed)
    let part2 = "Final star".to_string();
    
    (possible_count.to_string(), part2)
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Failed to read file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
