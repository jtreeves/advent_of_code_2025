# Technical Plan: Day 08

## Algorithms and Data Structures

### Algorithm Overview

The problem requires connecting 3D junction boxes into circuits using a greedy approach (closest pairs first) and tracking connected components.

**Part 1**: Connect the first 1000 closest pairs, then compute the product of sizes of the three largest circuits.

**Part 2**: Continue connecting pairs until all boxes are in one circuit, then compute the product of X coordinates of the final connection pair.

### Key Algorithms

1. **Input Parsing**:
   - Read all lines from input file
   - Parse each line as three comma-separated integers (x, y, z)
   - Store coordinates in a list/array of 3D points

2. **Distance Computation**:
   - For all pairs of boxes (i, j) where i < j:
     - Compute squared Euclidean distance: (x₂-x₁)² + (y₂-y₁)² + (z₂-z₁)²
     - Use squared distances to avoid floating-point precision issues (no need for sqrt)
   - Store pairs with their squared distances

3. **Pair Sorting**:
   - Sort all pairs by squared distance (ascending order)
   - This gives us the order of connections to process

4. **Union-Find (Disjoint Set Union) Data Structure**:
   - Use Union-Find to efficiently track connected components (circuits)
   - Operations:
     - `find(x)`: Find the root of component containing x
     - `union(x, y)`: Merge components containing x and y
   - Track component sizes for efficient querying
   - Path compression and union by rank/size for optimization

5. **Part 1 Processing**:
   - Initialize Union-Find with all boxes as separate components
   - Process first 1000 pairs from sorted list:
     - For each pair (even if already in same component):
       - Call `union()` operation
       - Increment connection counter
     - Stop after exactly 1000 connections (or when pairs run out)
   - Collect sizes of all components
   - Sort component sizes in descending order
   - Take the three largest sizes
   - Multiply them together
   - Return product

6. **Part 2 Processing**:
   - Initialize Union-Find with all boxes as separate components
   - Process pairs from sorted list in order:
     - For each pair:
       - Check if boxes are in different components
       - If different, perform `union()`
       - Store the pair that caused the final merge (when component count becomes 1)
     - Stop when all boxes are in one component
   - Extract the final connection pair
   - Multiply X coordinates of the two boxes
   - Return product

### Data Structures

- **Coordinates**: List/array of 3D points - `List[(x, y, z)]` or `Array[Point3D]`
- **Pairs**: List of tuples `(box1_idx, box2_idx, squared_distance)`
- **Union-Find**:
  - `parent`: Array mapping each box to its parent in the tree
  - `size`: Array storing size of each component
  - `component_count`: Counter for number of connected components
- **Part 2 tracking**: Store the final pair `(box1, box2)` when merging completes

### Why This Approach

- **Union-Find**: Efficient O(α(n)) amortized time per operation (α is inverse Ackermann, effectively constant). Much better than maintaining adjacency lists for this use case.
- **Squared distances**: Avoid floating-point precision issues and sqrt computation overhead. Distances only need to be compared, not computed exactly.
- **Greedy approach**: Connecting closest pairs first is the natural algorithm for this problem.

### Complexity Analysis

- **Time Complexity**: 
  - Parsing: O(n) where n = number of boxes
  - Distance computation: O(n²) - need to compute all pairwise distances
  - Sorting: O(n² log n²) = O(n² log n) - sorting n(n-1)/2 pairs
  - Union-Find operations: O(n² · α(n)) ≈ O(n²) - up to n² union operations, each nearly constant time
  - Overall: O(n² log n) dominated by sorting
- **Space Complexity**: 
  - O(n) for coordinates and Union-Find data structures
  - O(n²) for storing all pairs and distances
  - Overall: O(n²)

## Approach

### Part 1

1. **Parse Input**:
   - Read input file line by line
   - Parse comma-separated integers: x, y, z
   - Store in array/list of coordinate tuples

2. **Compute All Pairs**:
   - Generate all pairs (i, j) where 0 ≤ i < j < n
   - For each pair, compute squared distance
   - Store as list of (i, j, distance²)

3. **Sort Pairs**:
   - Sort list by squared distance (ascending)

4. **Initialize Union-Find**:
   - Create parent array: parent[i] = i for all i
   - Create size array: size[i] = 1 for all i
   - component_count = n

5. **Process First 1000 Connections**:
   - For each of first 1000 pairs (or until list exhausted):
     - Extract box indices i, j
     - Call union(i, j) - this counts as a connection even if already connected
     - Increment connection counter
   - Stop after exactly 1000 connections

6. **Calculate Answer**:
   - For each box, find its root
   - Group boxes by root to get component sizes
   - Sort sizes in descending order
   - Take first three sizes
   - Multiply them together
   - Return product

### Part 2

1. **Parse Input** (same as Part 1)

2. **Compute and Sort Pairs** (same as Part 1)

3. **Initialize Union-Find** (same as Part 1)

4. **Connect Until Complete**:
   - For each pair in sorted order:
     - Extract box indices i, j
     - If find(i) ≠ find(j):
       - This connection will merge two components
       - Store this pair as potential final pair
       - Call union(i, j)
       - If component_count == 1 after union, this was the final connection
       - If component_count == 1, break
   - Extract final connection pair (box1_idx, box2_idx)

5. **Calculate Answer**:
   - Get coordinates of final pair: boxes[box1_idx] and boxes[box2_idx]
   - Extract x coordinates: x1 and x2
   - Return x1 * x2

## Implementation Details

### Union-Find Operations

```pseudocode
function find(x):
    if parent[x] != x:
        parent[x] = find(parent[x])  # Path compression
    return parent[x]

function union(x, y):
    root_x = find(x)
    root_y = find(y)
    if root_x == root_y:
        return  # Already connected
    # Union by size (or rank)
    if size[root_x] < size[root_y]:
        swap(root_x, root_y)
    parent[root_y] = root_x
    size[root_x] += size[root_y]
    component_count--
```

### Edge Cases

- Fewer than 1000 pairs total: Process all available pairs in Part 1
- All boxes already in one component before 1000 connections: Still count all 1000 connection attempts
- Negative coordinates: Handle normally, distance formula works with negatives
- Large coordinates: Use appropriate integer types to avoid overflow in distance squared calculation

### Optimizations

- Use integer types for squared distances (avoid floating point)
- Union by size/rank for better Union-Find performance
- Path compression for O(α(n)) amortized find operations
- Early termination in Part 2 when component_count == 1
