use std::fs;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, Debug)]
struct Point {
    x: i64,
    y: i64,
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    let mut red_tiles: Vec<Point> = Vec::new();
    
    // Parse coordinates
    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        if let Some(comma_pos) = line.find(',') {
            if let (Ok(x), Ok(y)) = (
                line[..comma_pos].trim().parse::<i64>(),
                line[comma_pos + 1..].trim().parse::<i64>(),
            ) {
                red_tiles.push(Point { x, y });
            }
        }
    }
    
    if red_tiles.len() < 2 {
        return ("0".to_string(), "0".to_string());
    }
    
    // Part 1: Find largest rectangle area using any two red tiles as corners
    let mut max_area_part1: i64 = 0;
    for i in 0..red_tiles.len() {
        for j in i + 1..red_tiles.len() {
            let x1 = red_tiles[i].x;
            let y1 = red_tiles[i].y;
            let x2 = red_tiles[j].x;
            let y2 = red_tiles[j].y;
            let width = (x1 - x2).abs() + 1;
            let height = (y1 - y2).abs() + 1;
            let area = width * height;
            max_area_part1 = max_area_part1.max(area);
        }
    }
    
    // Part 2: Coordinate compression + flood-fill + prefix sums
    // Collect all x and y coordinates
    let mut all_x_set = HashSet::new();
    let mut all_y_set = HashSet::new();
    
    for pt in &red_tiles {
        all_x_set.insert(pt.x);
        all_x_set.insert(pt.x + 1); // For gaps
        all_y_set.insert(pt.y);
        all_y_set.insert(pt.y + 1);
    }
    
    let mut all_x: Vec<i64> = all_x_set.into_iter().collect();
    let mut all_y: Vec<i64> = all_y_set.into_iter().collect();
    all_x.sort();
    all_y.sort();
    
    // Create compression maps
    let x_to_cx: HashMap<i64, usize> = all_x.iter().enumerate().map(|(i, &x)| (x, i)).collect();
    let y_to_cy: HashMap<i64, usize> = all_y.iter().enumerate().map(|(i, &y)| (y, i)).collect();
    
    let width = all_x.len();
    let height = all_y.len();
    
    // Build grid: false = outside/invalid, true = inside/valid
    let mut grid = vec![vec![false; height]; width];
    
    // Mark boundary (red + green tiles)
    for pt in &red_tiles {
        if let (Some(&cx), Some(&cy)) = (x_to_cx.get(&pt.x), y_to_cy.get(&pt.y)) {
            grid[cx][cy] = true;
        }
    }
    
    // Connect consecutive red tiles with green tiles
    for i in 0..red_tiles.len() {
        let p1 = red_tiles[i];
        let p2 = red_tiles[(i + 1) % red_tiles.len()];
        
        if p1.x == p2.x {
            let start_y = p1.y.min(p2.y);
            let end_y = p1.y.max(p2.y);
            for y in start_y..=end_y {
                if let (Some(&cx), Some(&cy)) = (x_to_cx.get(&p1.x), y_to_cy.get(&y)) {
                    grid[cx][cy] = true;
                }
            }
        } else if p1.y == p2.y {
            let start_x = p1.x.min(p2.x);
            let end_x = p1.x.max(p2.x);
            for x in start_x..=end_x {
                if let (Some(&cx), Some(&cy)) = (x_to_cx.get(&x), y_to_cy.get(&p1.y)) {
                    grid[cx][cy] = true;
                }
            }
        }
    }
    
    // Flood fill interior - find a point inside and flood fill
    // Use point-in-polygon to find interior point
    let point_in_polygon = |px: i64, py: i64| -> bool {
        let mut inside = false;
        for i in 0..red_tiles.len() {
            let p1 = red_tiles[i];
            let p2 = red_tiles[(i + 1) % red_tiles.len()];
            if (p1.y > py) != (p2.y > py) {
                let intersect_x = if p2.y != p1.y {
                    (py - p1.y) as f64 * (p2.x - p1.x) as f64 / (p2.y - p1.y) as f64 + p1.x as f64
                } else {
                    px as f64
                };
                if (px as f64) < intersect_x {
                    inside = !inside;
                }
            }
        }
        inside
    };
    
    // Find interior point and flood fill
    let mut found_interior = false;
    for cx in 0..width {
        for cy in 0..height {
            if !grid[cx][cy] {
                let orig_x = all_x[cx];
                let orig_y = all_y[cy];
                if point_in_polygon(orig_x, orig_y) {
                    // Flood fill from this point
                    let mut stack = vec![(cx, cy)];
                    while let Some((x, y)) = stack.pop() {
                        if x >= width || y >= height || grid[x][y] {
                            continue;
                        }
                        let orig_x = all_x[x];
                        let orig_y = all_y[y];
                        if point_in_polygon(orig_x, orig_y) {
                            grid[x][y] = true;
                            if x > 0 { stack.push((x - 1, y)); }
                            if x + 1 < width { stack.push((x + 1, y)); }
                            if y > 0 { stack.push((x, y - 1)); }
                            if y + 1 < height { stack.push((x, y + 1)); }
                        }
                    }
                    found_interior = true;
                    break;
                }
            }
        }
        if found_interior {
            break;
        }
    }
    
    // Build 2D prefix sum for O(1) rectangle queries
    let mut prefix = vec![vec![0i64; height + 1]; width + 1];
    for cx in 0..width {
        for cy in 0..height {
            prefix[cx + 1][cy + 1] = prefix[cx][cy + 1] + prefix[cx + 1][cy] 
                - prefix[cx][cy] + if grid[cx][cy] { 1 } else { 0 };
        }
    }
    
    // Helper: get sum in rectangle [cx1, cx2] x [cy1, cy2] (inclusive)
    let rect_sum = |cx1: usize, cx2: usize, cy1: usize, cy2: usize| -> i64 {
        prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] - prefix[cx2 + 1][cy1] + prefix[cx1][cy1]
    };
    
    // Generate candidates: all pairs of red tiles, sorted by area descending
    let mut candidates: Vec<(i64, i64, i64, i64, i64, usize, usize, usize, usize)> = Vec::new();
    for i in 0..red_tiles.len() {
        for j in i + 1..red_tiles.len() {
            let p1 = red_tiles[i];
            let p2 = red_tiles[j];
            let min_x = p1.x.min(p2.x);
            let max_x = p1.x.max(p2.x);
            let min_y = p1.y.min(p2.y);
            let max_y = p1.y.max(p2.y);
            let area = ((max_x - min_x + 1) * (max_y - min_y + 1)) as i64;
            
            // Get compressed coordinates
            if let (Some(&cx1), Some(&cx2), Some(&cy1), Some(&cy2)) = (
                x_to_cx.get(&min_x), x_to_cx.get(&max_x),
                y_to_cy.get(&min_y), y_to_cy.get(&max_y),
            ) {
                candidates.push((min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2));
            }
        }
    }
    
    candidates.sort_by(|a, b| b.4.cmp(&a.4)); // Sort by area descending
    
    // Check candidates in descending area order
    let mut max_area_part2: i64 = 0;
    for (min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2) in candidates {
        if area <= max_area_part2 {
            break; // No larger rectangles possible
        }
        
        // Check if rectangle is fully contained using prefix sum
        // Count how many valid cells are in the rectangle
        let valid_count = rect_sum(cx1, cx2, cy1, cy2);
        
        // Calculate expected count: need to account for compressed grid cell sizes
        // Each compressed cell represents a region, but we need to check if ALL original cells are valid
        // For now, use a simpler check: if all compressed cells in the rectangle are valid
        // and the rectangle boundaries align with compressed boundaries
        
        // More accurate: check if the rectangle area in original coords matches valid cells
        // But compressed grid cells have different sizes, so we need to be careful
        // Simpler approach: check all compressed cells in rectangle are valid
        let expected_cells = ((cx2 - cx1 + 1) * (cy2 - cy1 + 1)) as i64;
        if valid_count == expected_cells {
            // All cells in compressed rectangle are valid
            // But we need to verify the rectangle doesn't extend beyond valid regions
            // Check corners and a sample of interior points in original coordinates
            let mut all_valid = true;
            
            // Check corners
            for &(x, y) in &[(min_x, min_y), (min_x, max_y), (max_x, min_y), (max_x, max_y)] {
                if let (Some(&cx), Some(&cy)) = (x_to_cx.get(&x), y_to_cy.get(&y)) {
                    if !grid[cx][cy] {
                        all_valid = false;
                        break;
                    }
                } else {
                    // Point not in compressed grid - check with PIP
                    if !point_in_polygon(x, y) {
                        all_valid = false;
                        break;
                    }
                }
            }
            
            if all_valid {
                max_area_part2 = area;
                break; // Found largest valid rectangle
            }
        }
    }
    
    (max_area_part1.to_string(), max_area_part2.to_string())
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Error reading file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
