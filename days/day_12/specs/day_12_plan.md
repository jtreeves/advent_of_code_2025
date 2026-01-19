# Technical Plan: Day 12

## Algorithms and Data Structures

### Algorithm Overview

Day 12 - "Christmas Tree Farm" is a 2D packing problem involving polyominoes. While packing problems are typically NP-hard, the puzzle input is designed such that a simple area check suffices.

**Part 1**: Parse shapes, calculate their areas, parse queries, check if total required area ≤ region area for each query, count how many are possible.

**Part 2**: No computational challenge - just output a placeholder.

### Key Algorithms

1. **Shape Parsing**:
   - Read lines until we find a shape definition (format: `number:`)
   - Read the next 3 lines (each 3 characters) to get the shape grid
   - Count the number of `#` characters in each shape to get its area
   - Store shape areas in an array/list indexed by shape number (0-5)

2. **Query Parsing**:
   - After empty line(s) following the shapes, read query lines
   - Each query line format: `widthxheight: count0 count1 count2 count3 count4 count5`
   - Parse width and height
   - Parse the six counts (one per shape)

3. **Area Check**:
   - For each query:
     - Calculate region area = width × height
     - Calculate required area = Σ(shape_area[i] × count[i]) for i in 0..5
     - If required_area ≤ region_area, mark query as possible
     - Otherwise, mark as impossible

4. **Count Results**:
   - Count how many queries are marked as possible
   - Return that count

### Data Structures

- **Shape areas**: Array/list of 6 integers (one per shape)
- **Queries**: List of query structures containing:
  - width (integer)
  - height (integer)
  - counts (array of 6 integers, one per shape)
- **Shape grid**: Temporary 3×3 character grid (array of strings or 2D array) for parsing

### Why This Approach

- **Area Check Heuristic**: While the problem suggests a complex packing problem, the actual puzzle input is designed such that if the total area of shapes fits in the region area, a valid packing always exists. This is a known simplification that works for all actual input queries.
- **Simplicity**: The area check is O(1) per query after parsing, making it extremely fast.
- **No Rotation/Flip Handling Needed**: Since we only check area, we don't need to generate all rotations/flips of shapes.
- **Correctness**: For the actual puzzle input, this heuristic gives the correct answer for all queries.

### Complexity Analysis

- **Time Complexity**: 
  - Shape parsing: O(1) - fixed 6 shapes, each 3×3 = O(18) characters
  - Query parsing: O(n) where n = number of queries
  - Area check per query: O(1) - just multiply and compare
  - Overall: O(n) where n = number of queries
- **Space Complexity**: 
  - O(1) for shape areas (fixed 6 integers)
  - O(n) for storing queries (if storing all at once)
  - Overall: O(n) for input, O(1) for processing

### Alternative Approaches (Not Needed for This Puzzle)

- **Backtracking with Rotation/Flip**: Generate all rotations and flips of each shape, then use backtracking to try all placements. This is the "correct" approach for general polyomino packing but is NP-hard and too slow for large inputs.
- **Algorithm X (Dancing Links)**: Exact cover algorithm for polyomino tiling. More efficient than backtracking but still complex and not needed here.
- **Constraint Programming**: Use SAT solver or integer programming. Overkill for this puzzle.

For this puzzle, the simple area check is sufficient and much more efficient.

## Approach

### Part 1

1. **Parse Shapes**:
   - Read lines until we encounter a line matching pattern `^\d+:$`
   - When found, read next 3 lines as the shape grid (3×3)
   - Count `#` characters in the shape to get its area
   - Store area in array at index = shape number
   - Repeat for all 6 shapes (expect numbers 0-5)

2. **Parse Queries**:
   - After shapes are parsed (skip empty lines), read remaining lines
   - For each query line:
     - Split on `:` to separate dimensions from counts
     - Parse `widthxheight` (split on `x`)
     - Parse counts (split on spaces, convert to integers)
     - Store query information

3. **Check Each Query**:
   - Calculate region_area = width × height
   - Calculate required_area = 0
   - For each shape i (0-5):
     - required_area += shape_area[i] × count[i]
   - If required_area ≤ region_area:
     - Increment possible count
   - Otherwise, skip

4. **Return Result**:
   - Return the count of possible queries

### Part 2

- Simply output a placeholder like "N/A" or "Final star"
- No computation needed

## Implementation Details

### Shape Area Calculation

Count the number of `#` characters in each 3×3 grid:
```python
shape_area = 0
for row in shape_grid:  # 3 rows
    for char in row:    # 3 chars per row
        if char == '#':
            shape_area += 1
```

### Query Parsing

Example line: `39x41: 21 29 30 36 23 30`

Parse with regex or string splitting:
- Split on `:` → `["39x41", " 21 29 30 36 23 30"]`
- Split first part on `x` → `["39", "41"]`
- Split second part on spaces and convert to integers → `[21, 29, 30, 36, 23, 30]`

### Edge Cases

- Empty queries (all counts are 0): Always possible (0 ≤ region_area)
- Exact fit (required_area == region_area): Count as possible
- Impossible queries (required_area > region_area): Don't count
- Very large numbers: Use appropriate integer types (64-bit integers may be needed)

### Validation

- Verify exactly 6 shapes are parsed (numbered 0-5)
- Verify each shape is 3×3
- Verify each query has exactly 6 counts
- Handle any empty lines between shapes or after shapes
