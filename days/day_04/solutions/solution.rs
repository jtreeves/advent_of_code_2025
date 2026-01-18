use std::fs;

fn count_neighbors(grid: &Vec<Vec<char>>, i: usize, j: usize, rows: usize, cols: usize) -> i32 {
    let mut count = 0;
    for di in -1..=1 {
        for dj in -1..=1 {
            if di == 0 && dj == 0 {
                continue;
            }
            let ni = i as i32 + di;
            let nj = j as i32 + dj;
            if ni >= 0 && ni < rows as i32 && nj >= 0 && nj < cols as i32 {
                if grid[ni as usize][nj as usize] == '@' {
                    count += 1;
                }
            }
        }
    }
    count
}

fn solve(lines: Vec<String>) -> (String, String) {
    let rows = lines.len();
    let cols = if rows > 0 { lines[0].len() } else { 0 };

    // Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    let mut part1_count = 0;
    for i in 0..rows {
        let chars: Vec<char> = lines[i].chars().collect();
        for j in 0..cols {
            if chars[j] == '@' {
                let grid_chars: Vec<Vec<char>> = lines.iter()
                    .map(|s| s.chars().collect())
                    .collect();
                let neighbors = count_neighbors(&grid_chars, i, j, rows, cols);
                if neighbors < 4 {
                    part1_count += 1;
                }
            }
        }
    }

    // Part 2: Iteratively remove accessible rolls until none can be removed
    let mut grid: Vec<Vec<char>> = lines.iter()
        .map(|s| s.chars().collect())
        .collect();

    let mut part2_count = 0;
    loop {
        let mut to_remove: Vec<(usize, usize)> = Vec::new();
        for i in 0..rows {
            for j in 0..cols {
                if grid[i][j] == '@' {
                    let neighbors = count_neighbors(&grid, i, j, rows, cols);
                    if neighbors < 4 {
                        to_remove.push((i, j));
                    }
                }
            }
        }

        if to_remove.is_empty() {
            break;
        }

        // Remove all marked positions
        for (i, j) in &to_remove {
            grid[*i][*j] = '.';
        }
        part2_count += to_remove.len();
    }

    (part1_count.to_string(), part2_count.to_string())
}

fn main() {
    let content = fs::read_to_string("../data/input.txt").expect("Failed to read file");
    let lines: Vec<String> = content
        .trim()
        .lines()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    let (part1, part2) = solve(lines);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
