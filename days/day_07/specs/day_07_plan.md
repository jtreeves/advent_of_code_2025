# Technical Plan: Day 07

## Algorithms and Data Structures

### Algorithm Overview

The problem requires simulating tachyon beam propagation through a grid with splitters. Beams move downward and split when encountering a `^` character.

**Part 1**: Count the total number of times beams are split (i.e., how many times a splitter is encountered).

**Part 2**: Count the total number of distinct beams that reach the bottom row (beams can merge, so we need to track counts per cell).

### Key Algorithms

1. **Grid Parsing**:
   - Read all lines from input file
   - Parse into 2D grid of characters
   - Locate the starting position `S` (exactly one)
   - Identify all splitter positions (`^`)

2. **Beam Propagation (Part 1)**:
   - Start with a single beam at position `S`
   - Process row by row from top to bottom
   - For each row, track active beam positions (columns)
   - When a beam hits a splitter:
     - Increment split counter
     - Remove the beam from current column
     - Add new beams to left and right columns
   - Continue until all beams exit the grid (go below last row)

3. **Beam Propagation (Part 2)**:
   - Similar to Part 1, but track beam counts per cell instead of just presence
   - Use a 2D array to store beam counts for each cell
   - When beams merge (multiple beams reach same cell), sum the counts
   - At the end, sum all beam counts in the bottom row

### Data Structures

- **Grid**: 2D array/char matrix (rows × columns)
- **Part 1 - Active Beams**: Set or list of column indices where beams are currently active, OR map row → set of columns with active beams
- **Part 1 - Split Counter**: Integer counter incremented on each split
- **Part 2 - Beam Counts**: 2D array/matrix of integers (same size as grid) to track beam count per cell
- **Part 1 Processing**: Can process row-by-row, tracking active columns per row
- **Part 2 Processing**: Process row-by-row, accumulating beam counts in the 2D count matrix

### Why This Approach

- **Row-by-row processing**: Beams only move downward, so we can process the grid row-by-row without needing complex path tracking
- **Part 1 uses set/list**: We only need to track which columns have active beams, not counts
- **Part 2 uses count matrix**: We need to track actual beam counts since beams can merge (multiple beams can occupy the same cell)
- **Efficient**: O(rows × columns) time complexity, which is optimal since we need to examine each cell

### Complexity Analysis

- **Time Complexity**: O(rows × columns) where rows = grid height, columns = grid width
  - We process each row once
  - For each row, we process all active beam positions
  - Maximum active beams is bounded by grid width (beams can't exceed grid boundaries)
- **Space Complexity**: 
  - Part 1: O(columns) for tracking active beam positions per row
  - Part 2: O(rows × columns) for the beam count matrix

## Approach

### Part 1: Count Total Splits

1. **Parse Grid**:
   - Read input file into a 2D grid
   - Find starting position `S` (store row and column)

2. **Initialize**:
   - Split counter = 0
   - Current active beams = set containing the column of `S`

3. **Process Row by Row** (starting from row after `S`):
   - For each row:
     - Create new set for next row's active beams
     - For each active beam in current row:
       - If cell at (row, column) is `.` (empty):
         - Beam continues down → add this column to next row's active beams
       - If cell at (row, column) is `^` (splitter):
         - Increment split counter
         - Remove beam from current column
         - Add left column (column - 1) to next row's active beams (if valid)
         - Add right column (column + 1) to next row's active beams (if valid)
     - Move to next row with updated active beams set
   - Continue until we've processed all rows

4. **Return**: Total split counter

### Part 2: Count Beams at Bottom Row

1. **Parse Grid**: Same as Part 1

2. **Initialize**:
   - Create 2D count matrix (rows × columns), initialized to 0
   - Set count at `S` position to 1 (one beam starts)

3. **Process Row by Row** (starting from row after `S`):
   - For each row:
     - Create new count row (for this row)
     - For each column that had beams in previous row:
       - If cell at (row, column) is `.` (empty):
         - Beam continues down → add beam count to new count row at same column
       - If cell at (row, column) is `^` (splitter):
         - Add beam count to left column (column - 1) in new count row (if valid)
         - Add beam count to right column (column + 1) in new count row (if valid)
     - Update count matrix for current row
     - Continue to next row

4. **Sum Bottom Row**:
   - Sum all values in the last row of the count matrix
   - Return this sum

## Implementation Notes

- **Boundary checking**: Always validate column indices (must be >= 0 and < grid width)
- **Beam merging**: In Part 2, when multiple beams reach the same cell, add their counts
- **Row-by-row processing**: Can use a single pass through rows, updating beam positions/counts incrementally
- **Grid representation**: Use 2D array/list of strings or 2D char array depending on language
- **Part 1 optimization**: Can use a set/map per row instead of full 2D matrix (only need presence, not counts)

## Edge Cases

- Single row grid (just `S`, no other rows)
- No splitters (Part 1 = 0)
- All splitters (every row has splitters)
- Very wide grid (many columns)
- Beams that would go out of bounds (left/right of splitter at grid edges)
- Multiple beams merging at same cell (Part 2)
