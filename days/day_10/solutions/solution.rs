use std::fs;
use regex::Regex;

struct ParseResult {
    target_pattern: Vec<bool>,
    buttons: Vec<Vec<i32>>,
    joltages: Vec<i32>,
}

fn parse_line(line: &str) -> ParseResult {
    let mut result = ParseResult {
        target_pattern: Vec::new(),
        buttons: Vec::new(),
        joltages: Vec::new(),
    };
    
    // Extract pattern: [.##.]
    let pattern_re = Regex::new(r"\[([.#]+)\]").unwrap();
    if let Some(caps) = pattern_re.captures(line) {
        let pattern_str = caps.get(1).unwrap().as_str();
        result.target_pattern = pattern_str.chars().map(|c| c == '#').collect();
    }
    
    // Extract buttons: (1,3) (2) etc.
    let button_re = Regex::new(r"\(([^)]*)\)").unwrap();
    for caps in button_re.captures_iter(line) {
        let btn_str = caps.get(1).unwrap().as_str().trim();
        if btn_str.is_empty() {
            result.buttons.push(Vec::new());
        } else {
            let lights: Vec<i32> = btn_str.split(',')
                .map(|s| s.trim().parse().unwrap_or(0))
                .collect();
            result.buttons.push(lights);
        }
    }
    
    // Extract joltages: {3,5,4,7}
    let joltage_re = Regex::new(r"\{([^}]+)\}").unwrap();
    if let Some(caps) = joltage_re.captures(line) {
        let joltage_str = caps.get(1).unwrap().as_str();
        result.joltages = joltage_str.split(',')
            .map(|s| s.trim().parse().unwrap_or(0))
            .collect();
    }
    
    result
}

fn gaussian_elimination_gf2(matrix: &[Vec<bool>], target: &[bool]) -> Option<i32> {
    let num_buttons = matrix.len();
    let num_lights = target.len();
    
    // Create augmented matrix [A | b]
    let mut aug: Vec<Vec<bool>> = Vec::new();
    for i in 0..num_lights {
        let mut row = Vec::new();
        for j in 0..num_buttons {
            if j < matrix.len() && i < matrix[j].len() {
                row.push(matrix[j][i]);
            } else {
                row.push(false);
            }
        }
        row.push(target[i]);
        aug.push(row);
    }
    
    // Gaussian elimination mod 2
    let mut pivot_row = 0;
    let mut pivot_col = 0;
    
    while pivot_row < num_lights && pivot_col < num_buttons {
        // Find pivot
        let mut pivot_idx: Option<usize> = None;
        for i in pivot_row..num_lights {
            if aug[i][pivot_col] {
                pivot_idx = Some(i);
                break;
            }
        }
        
        if let Some(idx) = pivot_idx {
            // Swap rows
            if idx != pivot_row {
                aug.swap(pivot_row, idx);
            }
            
            // Eliminate
            for i in (pivot_row + 1)..num_lights {
                if aug[i][pivot_col] {
                    for j in 0..=num_buttons {
                        aug[i][j] ^= aug[pivot_row][j];
                    }
                }
            }
            
            pivot_row += 1;
        }
        pivot_col += 1;
    }
    
    // Check for inconsistency (0 = 1)
    for i in pivot_row..num_lights {
        if aug[i][num_buttons] && !aug[i][..num_buttons].iter().any(|&x| x) {
            return None; // No solution
        }
    }
    
    // Back substitution to find solution (set free variables to 0 to minimize)
    let mut solution = vec![false; num_buttons];
    let mut used_rows = std::collections::HashSet::new();
    
    // Process from bottom up
    for i in (0..num_lights).rev() {
        // Find first non-zero column
        let mut pivot_col_idx: Option<usize> = None;
        for j in 0..num_buttons {
            if aug[i][j] && !used_rows.contains(&j) {
                pivot_col_idx = Some(j);
                used_rows.insert(j);
                break;
            }
        }
        
        if let Some(idx) = pivot_col_idx {
            // Calculate value
            let mut val = aug[i][num_buttons];
            for j in (idx + 1)..num_buttons {
                if aug[i][j] && solution[j] {
                    val ^= true;
                }
            }
            solution[idx] = val;
        }
    }
    
    // Count number of presses
    Some(solution.iter().filter(|&&x| x).count() as i32)
}

fn solve_part2_ilp(buttons: &[Vec<i32>], joltages: &[i32], target_pattern: &[bool]) -> Option<i32> {
    let num_buttons = buttons.len();
    let num_lights = joltages.len();
    
    // Base case: all joltages are 0
    if joltages.iter().all(|&j| j == 0) {
        return Some(0);
    }
    
    // Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
    // that achieve the required parity, then divide by 2 and recurse
    let mut button_matrix = vec![vec![false; num_lights]; num_buttons];
    for (i, btn) in buttons.iter().enumerate() {
        for &light in btn {
            if light >= 0 && (light as usize) < num_lights {
                button_matrix[i][light as usize] = true;
            }
        }
    }
    
    // Required parity: pattern ON = odd, pattern OFF = even
    let required_parity: Vec<bool> = target_pattern.to_vec();
    
    // Find minimum parity presses needed (Part 1 style)
    let parity_toggles = required_parity.clone();
    let min_parity_presses = gaussian_elimination_gf2(&button_matrix, &parity_toggles);
    
    if let Some(min_presses) = min_parity_presses {
        // Helper to generate combinations
        fn generate_combinations(n: usize, k: usize) -> Vec<Vec<usize>> {
            if k == 0 {
                return vec![vec![]];
            }
            if k > n {
                return vec![];
            }
            if k == n {
                return vec![(0..n).collect()];
            }
            let mut result = Vec::new();
            // Include first element
            for mut combo in generate_combinations(n - 1, k - 1) {
                combo.insert(0, 0);
                for i in 1..combo.len() {
                    combo[i] += 1;
                }
                result.push(combo);
            }
            // Exclude first element
            for mut combo in generate_combinations(n - 1, k) {
                for i in 0..combo.len() {
                    combo[i] += 1;
                }
                result.push(combo);
            }
            result
        }
        
        let mut best_result: Option<i32> = None;
        let max_parity_to_try = num_buttons;
        
        for k in min_presses as usize..=max_parity_to_try {
            // Pruning
            if let Some(best) = best_result {
                if k > best as usize {
                    break;
                }
            }
            
            for button_combo in generate_combinations(num_buttons, k) {
                // Simulate pressing these buttons once
                let mut resulting_joltages = joltages.to_vec();
                for &btn_idx in &button_combo {
                    for &light in &buttons[btn_idx] {
                        if light >= 0 && (light as usize) < num_lights {
                            resulting_joltages[light as usize] += 1;
                        }
                    }
                }
                
                // Check if parity matches
                let resulting_parity: Vec<bool> = resulting_joltages.iter().map(|&j| j % 2 == 1).collect();
                if resulting_parity == required_parity {
                    // Check if all remaining are even (can divide by 2)
                    if resulting_joltages.iter().all(|&j| j % 2 == 0) {
                        // Divide by 2 and recurse
                        let halved_joltages: Vec<i32> = resulting_joltages.iter().map(|&j| j / 2).collect();
                        if let Some(remaining_result) = solve_part2_ilp(buttons, &halved_joltages, target_pattern) {
                            let total = k as i32 + remaining_result * 2;
                            best_result = Some(best_result.map(|b| b.min(total)).unwrap_or(total));
                        }
                    }
                }
            }
        }
        
        if best_result.is_some() {
            return best_result;
        }
    }
    
    // Fallback: divide-by-2 optimization
    if joltages.iter().all(|&j| j % 2 == 0) && joltages.iter().any(|&j| j > 0) {
        let halved_joltages: Vec<i32> = joltages.iter().map(|&j| j / 2).collect();
        if let Some(result) = solve_part2_ilp(buttons, &halved_joltages, target_pattern) {
            return Some(result * 2);
        }
        return None;
    }
    
    // Final fallback: bounded DFS
    let max_joltage = joltages.iter().max().copied().unwrap_or(0);
    
    fn dfs(
        button_idx: usize,
        current_joltages: &[i32],
        presses_so_far: i32,
        best: Option<i32>,
        buttons: &[Vec<i32>],
        joltages: &[i32],
    ) -> Option<i32> {
        if button_idx >= buttons.len() {
            if current_joltages.iter().zip(joltages.iter()).all(|(a, b)| a == b) {
                return Some(best.map(|b| b.min(presses_so_far)).unwrap_or(presses_so_far));
            }
            return best;
        }
        
        if let Some(b) = best {
            if presses_so_far >= b {
                return best;
            }
        }
        
        let mut best_result = best;
        for presses in 0..=max_joltage {
            if let Some(b) = best {
                if presses_so_far + presses >= b {
                    break;
                }
            }
            
            let mut new_joltages = current_joltages.to_vec();
            for &light in &buttons[button_idx] {
                if light >= 0 && (light as usize) < joltages.len() {
                    new_joltages[light as usize] += presses;
                }
            }
            
            if new_joltages.iter().zip(joltages.iter()).any(|(a, b)| a > b) {
                continue;
            }
            
            let result = dfs(button_idx + 1, &new_joltages, presses_so_far + presses, best_result, buttons, joltages);
            if let Some(r) = result {
                best_result = Some(best_result.map(|b| b.min(r)).unwrap_or(r));
            }
        }
        
        best_result
    }
    
    let initial_joltages = vec![0; num_lights];
    dfs(0, &initial_joltages, 0, None, buttons, joltages)
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    if lines.is_empty() {
        return ("0".to_string(), "0".to_string());
    }
    
    let mut part1_total = 0;
    let mut part2_total = 0;
    
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        
        let parsed = parse_line(line);
        if parsed.target_pattern.is_empty() {
            continue;
        }
        
        let num_lights = parsed.target_pattern.len();
        
        // Part 1: GF(2) linear system
        // Build incidence matrix: matrix[i][j] = true if button i toggles light j
        let mut button_matrix: Vec<Vec<bool>> = vec![vec![false; num_lights]; parsed.buttons.len()];
        for (i, btn) in parsed.buttons.iter().enumerate() {
            for &light in btn {
                if light >= 0 && (light as usize) < num_lights {
                    button_matrix[i][light as usize] = true;
                }
            }
        }
        
        // Target: all start OFF, need to toggle to match pattern
        // Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        let required_toggles = parsed.target_pattern.clone();
        
        if let Some(result) = gaussian_elimination_gf2(&button_matrix, &required_toggles) {
            part1_total += result;
        }
        
        // Part 2: Integer Linear Programming
        if parsed.joltages.len() == num_lights {
            if let Some(result2) = solve_part2_ilp(&parsed.buttons, &parsed.joltages, &parsed.target_pattern) {
                part2_total += result2;
            }
        }
    }
    
    (part1_total.to_string(), part2_total.to_string())
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Error reading file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
