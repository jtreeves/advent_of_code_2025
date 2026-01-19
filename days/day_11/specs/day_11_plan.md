# Technical Plan: Day 11

## Algorithms and Data Structures

### Algorithm Overview

The problem requires counting paths in a directed graph. Part 1 counts all paths from a source to a destination. Part 2 counts paths that visit two required nodes (`fft` and `dac`) along the way.

**Part 1**: Count all simple paths from `you` to `out` in a directed acyclic graph (or handle cycles appropriately).

**Part 2**: Count all simple paths from `svr` to `out` that include both `fft` and `dac` in the path (in any order, though input guarantees `fft` before `dac`).

### Key Algorithms

1. **Input Parsing**:
   - Read input file line by line
   - Parse each line: `<device>: <output1> <output2> ...`
   - Build an adjacency list representation of the graph
   - Map device names to lists of their output devices

2. **Graph Representation**:
   - Use a map/dictionary: `device_name → [list of output devices]`
   - This forms a directed graph where edges go from a device to its outputs

3. **Path Counting (Part 1)**:
   - Use Depth-First Search (DFS) with memoization
   - Start from `you`, explore all paths to `out`
   - **Important**: The graph may have cycles, but cycles cannot be part of valid paths
   - Use memoization to cache path counts from each node to `out`
   - If a node has no path to `out`, memoize 0
   - Recurrence: `paths(node) = sum(paths(neighbor) for each neighbor of node)`
   - Base case: `paths(out) = 1`

4. **Path Counting (Part 2)**:
   - Similar DFS approach, but track whether `fft` and `dac` have been visited
   - Use state: `(current_node, visited_fft, visited_dac)`
   - Count paths from `svr` to `out` where both flags are true at `out`
   - Memoization key: `(node, visited_fft, visited_dac)` → path count
   - Recurrence: For each neighbor, explore with updated flags:
     - If neighbor is `fft`, set `visited_fft = true`
     - If neighbor is `dac`, set `visited_dac = true`
   - Base case: At `out`, if both flags are true, return 1, else 0

### Data Structures

- **Graph**: Map/Dictionary from string (device name) to list of strings (output devices)
  - In C: `char* → char**` (array of strings)
  - In higher-level languages: `Map<String, List<String>>` or similar
  
- **Memoization**: Cache to avoid recomputing path counts
  - Part 1: `Map<String, Integer>` (node → path count to `out`)
  - Part 2: `Map<(String, bool, bool), Integer>` (node + flags → path count)
  - In C: use a hash table or similar structure

- **DFS Stack/Tracking**:
  - Part 1: Track visited nodes in current path to detect cycles (though cycles should be disallowed in valid paths)
  - Part 2: Track visited flags for `fft` and `dac`

### Why This Approach

- **DFS with Memoization**: Efficiently counts paths without enumerating all paths explicitly. Memoization ensures each subproblem is solved once.

- **State Tracking for Part 2**: Need to track which required nodes have been visited, so we use state flags. The memoization key includes these flags.

- **Cycle Handling**: The problem states data flows forward only, and cycles shouldn't be part of valid paths. During DFS, if we encounter a node we've already visited in the current path, we can skip it (or memoize 0 if we detect a cycle that doesn't lead to `out`).

### Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(V + E) where V = number of devices, E = number of edges, but with memoization we visit each node once, so effectively O(V + E)
  - Part 2: O(V + E) with state space of 4 (2 boolean flags = 4 states), so effectively O(4*(V + E)) = O(V + E)

- **Space Complexity**: 
  - Graph storage: O(V + E)
  - Memoization: O(V) for Part 1, O(4*V) = O(V) for Part 2 (4 states per node)
  - DFS recursion stack: O(V) in worst case (longest path)
  - Total: O(V + E)

## Approach

### Part 1

1. **Parse Input**:
   - Read input file
   - Build adjacency list: `graph[device] = [list of outputs]`
   - Find `you` and `out` devices

2. **Count Paths**:
   - Define recursive function `countPaths(node, memo)`
   - If `node == out`, return 1
   - If `node` in memo, return memoized value
   - Initialize count = 0
   - For each neighbor in `graph[node]`:
     - count += `countPaths(neighbor, memo)`
   - Memoize and return count

3. **Handle Edge Cases**:
   - If `you` not found or `out` not found, return 0
   - If node has no path to `out`, memoize 0 to avoid infinite loops

### Part 2

1. **Parse Input**:
   - Same as Part 1
   - Find `svr`, `fft`, `dac`, and `out` devices

2. **Count Paths with Constraints**:
   - Define recursive function `countPaths(node, visited_fft, visited_dac, memo)`
   - If `node == out`:
     - If `visited_fft && visited_dac`, return 1
     - Else return 0
   - If `(node, visited_fft, visited_dac)` in memo, return memoized value
   - Update flags: if `node == fft`, set `visited_fft = true`; if `node == dac`, set `visited_dac = true`
   - Initialize count = 0
   - For each neighbor in `graph[node]`:
     - count += `countPaths(neighbor, visited_fft, visited_dac, memo)`
   - Memoize and return count

3. **Optimization**:
   - Since input guarantees `fft` before `dac`, we could optimize by only setting `visited_dac = true` if `visited_fft == true`, but the general approach works regardless.

## Implementation Notes

- **Memoization is crucial**: Without it, exponential time complexity. With it, linear time.
- **Cycle Detection**: Since cycles can't be part of valid paths, if we revisit a node in the same DFS path, we can skip it (memo will handle it if it leads to `out`, or we can detect cycles explicitly).
- **Large Numbers**: Path counts can be very large. Use `long`, `int64`, `BigInt`, or appropriate big integer types.
- **Missing Devices**: Handle gracefully if required devices (`you`, `svr`, `out`, `fft`, `dac`) are not found.
- **Empty Outputs**: Devices with no outputs should map to empty lists.

## Edge Cases

- Graph with no path from source to destination
- Graph with cycles (handle by disallowing cycles in paths)
- Single node path (`you: out` → 1 path)
- Multiple paths through same intermediate nodes
- Very large path counts requiring big integers
- Missing device labels in input
- Devices with no outputs (dead ends)
