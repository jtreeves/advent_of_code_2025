use std::collections::HashMap;
use std::fs;

fn solve(input_data: String) -> (i64, i64) {
    let lines: Vec<String> = input_data
        .trim()
        .lines()
        .map(|s| s.trim().to_string())
        .collect();
    
    // Build graph: device -> list of outputs
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    for line in &lines {
        if line.is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() != 2 {
            continue;
        }
        let device = parts[0].trim().to_string();
        let outputs_str = parts[1].trim();
        let outputs: Vec<String> = if outputs_str.is_empty() {
            vec![]
        } else {
            outputs_str.split_whitespace().map(|s| s.to_string()).collect()
        };
        graph.insert(device, outputs);
    }
    
    // Part 1: Count paths from "you" to "out"
    fn count_paths_part1(
        node: &str,
        graph: &HashMap<String, Vec<String>>,
        memo: &mut HashMap<String, i64>,
    ) -> i64 {
        if node == "out" {
            return 1;
        }
        if let Some(&val) = memo.get(node) {
            return val;
        }
        
        let mut count = 0i64;
        if let Some(neighbors) = graph.get(node) {
            for neighbor in neighbors {
                count += count_paths_part1(neighbor, graph, memo);
            }
        }
        
        memo.insert(node.to_string(), count);
        count
    }
    
    let mut part1_memo: HashMap<String, i64> = HashMap::new();
    let part1_count = if graph.contains_key("you") {
        count_paths_part1("you", &graph, &mut part1_memo)
    } else {
        0
    };
    
    // Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    #[derive(Hash, Eq, PartialEq, Clone)]
    struct MemoKey {
        node: String,
        visited_fft: bool,
        visited_dac: bool,
    }
    
    fn count_paths_part2(
        node: &str,
        visited_fft: bool,
        visited_dac: bool,
        graph: &HashMap<String, Vec<String>>,
        memo: &mut HashMap<MemoKey, i64>,
    ) -> i64 {
        if node == "out" {
            return if visited_fft && visited_dac { 1 } else { 0 };
        }
        
        let key = MemoKey {
            node: node.to_string(),
            visited_fft,
            visited_dac,
        };
        if let Some(&val) = memo.get(&key) {
            return val;
        }
        
        // Update flags when visiting fft or dac
        let new_visited_fft = visited_fft || (node == "fft");
        let new_visited_dac = visited_dac || (node == "dac");
        
        let mut count = 0i64;
        if let Some(neighbors) = graph.get(node) {
            for neighbor in neighbors {
                count += count_paths_part2(neighbor, new_visited_fft, new_visited_dac, graph, memo);
            }
        }
        
        memo.insert(key, count);
        count
    }
    
    let mut part2_memo: HashMap<MemoKey, i64> = HashMap::new();
    let part2_count = if graph.contains_key("svr") {
        count_paths_part2("svr", false, false, &graph, &mut part2_memo)
    } else {
        0
    };
    
    (part1_count, part2_count)
}

fn main() {
    let content = fs::read_to_string("../data/input.txt").expect("Failed to read file");
    let (part1, part2) = solve(content);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
