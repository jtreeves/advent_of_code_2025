use std::fs;
use std::collections::HashMap;

type Coord = (i64, i64, i64);
type Pair = (usize, usize, i64);

struct UnionFind {
    parent: Vec<usize>,
    size: Vec<i64>,
    component_count: usize,
}

impl UnionFind {
    fn new(n: usize) -> Self {
        UnionFind {
            parent: (0..n).collect(),
            size: vec![1; n],
            component_count: n,
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            self.parent[x] = self.find(self.parent[x]);
        }
        self.parent[x]
    }

    fn union(&mut self, x: usize, y: usize) -> bool {
        let root_x = self.find(x);
        let root_y = self.find(y);
        if root_x == root_y {
            return false;
        }
        let (root_x, root_y) = if self.size[root_x] < self.size[root_y] {
            (root_y, root_x)
        } else {
            (root_x, root_y)
        };
        self.parent[root_y] = root_x;
        self.size[root_x] += self.size[root_y];
        self.component_count -= 1;
        true
    }
}

fn parse_coordinates(lines: &[&str]) -> Vec<Coord> {
    lines.iter()
        .filter_map(|line| {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                return None;
            }
            let parts: Vec<&str> = trimmed.split(',').collect();
            if parts.len() != 3 {
                return None;
            }
            let x: i64 = parts[0].parse().ok()?;
            let y: i64 = parts[1].parse().ok()?;
            let z: i64 = parts[2].parse().ok()?;
            Some((x, y, z))
        })
        .collect()
}

fn squared_distance(p1: Coord, p2: Coord) -> i64 {
    let dx = p2.0 - p1.0;
    let dy = p2.1 - p1.1;
    let dz = p2.2 - p1.2;
    dx * dx + dy * dy + dz * dz
}

fn solve(input_data: &str) -> (String, String) {
    let lines: Vec<&str> = input_data.trim().lines().collect();
    let coords = parse_coordinates(&lines);

    let n = coords.len();
    if n == 0 {
        return ("0".to_string(), "0".to_string());
    }

    // Generate all pairs with squared distances
    let mut pairs: Vec<Pair> = Vec::new();
    for i in 0..n {
        for j in (i + 1)..n {
            let dist_sq = squared_distance(coords[i], coords[j]);
            pairs.push((i, j, dist_sq));
        }
    }

    // Sort by distance
    pairs.sort_by_key(|&(_, _, dist_sq)| dist_sq);

    // Part 1: Connect first 1000 pairs
    let mut uf1 = UnionFind::new(n);
    let mut connections_made = 0;
    for &(i, j, _) in &pairs {
        if connections_made >= 1000 {
            break;
        }
        uf1.union(i, j);
        connections_made += 1;
    }

    // Get component sizes
    let mut component_sizes: HashMap<usize, i64> = HashMap::new();
    for i in 0..n {
        let root = uf1.find(i);
        component_sizes.insert(root, uf1.size[root]);
    }

    let mut sizes: Vec<i64> = component_sizes.values().copied().collect();
    sizes.sort_by(|a, b| b.cmp(a));
    let part1 = if sizes.len() >= 3 {
        sizes[0] * sizes[1] * sizes[2]
    } else {
        0
    };

    // Part 2: Connect until all in one circuit
    let mut uf2 = UnionFind::new(n);
    let mut final_pair: Option<(usize, usize)> = None;
    for &(i, j, _) in &pairs {
        if uf2.component_count == 1 {
            break;
        }
        if uf2.union(i, j) {
            if uf2.component_count == 1 {
                final_pair = Some((i, j));
                break;
            }
        }
    }

    let part2 = if let Some((i, j)) = final_pair {
        coords[i].0 * coords[j].0
    } else {
        0
    };

    (part1.to_string(), part2.to_string())
}

fn main() {
    let data = fs::read_to_string("../data/input.txt").expect("Error reading file");
    let (part1, part2) = solve(&data);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
