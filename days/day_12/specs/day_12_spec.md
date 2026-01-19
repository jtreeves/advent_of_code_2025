# Specification: Day 12

## Problem Statement

Day 12 - "Christmas Tree Farm" is the finale of Advent of Code 2025. The puzzle involves fitting irregular present shapes (polyominoes) into rectangular regions under Christmas trees. Shapes can be rotated and flipped.

## Requirements

### Part 1
- Parse six (6) distinct present shapes from the input, each defined as a polyomino in a 3×3 grid
- Each shape uses `#` to represent filled cells and `.` for empty cells within the 3×3 bounding box
- Shapes can be rotated and flipped (reflected)
- Parse a list of queries, each specifying:
  1. A rectangular region size: width × height
  2. For each of the 6 shapes, how many copies of that shape must be placed inside the rectangle
- For each query, determine whether it's possible to pack all the given shapes into the specified rectangular region without overlapping, allowing rotations and reflections
- Count how many queries (regions) are possible to satisfy
- Return the count of possible regions

### Part 2
- There is no traditional Part 2 challenge for Day 12
- Day 12 is the finale, and after completing Part 1 and having collected all stars from previous days, you earn the final star
- The solution should output a placeholder or note for Part 2

## Input Format

The input file consists of two parts:

1. **Shape Definitions**: Six shapes, numbered 0 through 5, each defined in a 3×3 grid
   - Format: `shape_number:` followed by 3 lines, each containing 3 characters (`#` or `.`)
   - Shapes are separated by empty lines

2. **Region Queries**: Multiple queries, one per line
   - Format: `widthxheight: count0 count1 count2 count3 count4 count5`
   - Example: `39x41: 21 29 30 36 23 30`
   - This means a 39×41 rectangle needs 21 copies of shape 0, 29 copies of shape 1, 30 copies of shape 2, 36 copies of shape 3, 23 copies of shape 4, and 30 copies of shape 5

Example input:
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

39x41: 21 29 30 36 23 30
35x36: 38 28 29 41 25 33
```

## Output Format

For Part 1: A single integer representing the number of queries (regions) that are possible to satisfy.

For Part 2: A placeholder (e.g., "N/A" or "Final star") since there's no computational challenge.

Programs should output:
```
Part 1: <number>
Part 2: <placeholder>
```

## Constraints

- Each shape is defined in a 3×3 grid, but may be sparse (contain `.` cells)
- Shapes can be rotated (90°, 180°, 270°) and flipped (reflected horizontally or vertically)
- In each query, shapes must not overlap when placed in the region
- All required shapes must fit within the rectangular region boundaries
- While 2D packing/polyomino tiling is NP-hard in general, for the actual puzzle input, a simple area check suffices:
  - If total area of all required shapes (sum of each shape's area × its count) ≤ area of the region, then the packing is possible
  - Otherwise, it's impossible
- Shape area is the number of `#` cells in the shape
- Region area = width × height
- Results can be large (potentially exceeding 32-bit integer limits) - use appropriate data types

## Test Cases

### Test Case 1 (Simple Example)
**Input** (`test_1.txt`):
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

5x5: 1 0 0 0 0 0
6x6: 2 0 0 0 0 0
```

**Expected Output Part 1**: 2

**Explanation**: 
- Shape 0 has area 7 (7 `#` cells)
- First query (5×5 = 25 area): needs 1 copy of shape 0 (area 7). Since 7 ≤ 25, it's possible.
- Second query (6×6 = 36 area): needs 2 copies of shape 0 (area 14). Since 14 ≤ 36, it's possible.
- Both queries are possible, so answer is 2.

**Expected Output Part 2**: N/A (or "Final star")

### Test Case 2 (Area Check)
**Input** (`test_2.txt`):
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

4x4: 1 1 0 0 0 0
3x3: 2 0 0 0 0 0
```

**Expected Output Part 1**: 1

**Explanation**:
- Shape 0 has area 7, Shape 1 has area 5
- First query (4×4 = 16 area): needs shape 0 (7) + shape 1 (5) = 12 total. Since 12 ≤ 16, it's possible.
- Second query (3×3 = 9 area): needs 2×shape 0 = 14 total. Since 14 > 9, it's impossible.
- Only 1 query is possible.

### Test Case 3 (All Shapes)
**Input** (`test_3.txt`):
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

10x10: 1 1 1 1 1 1
5x5: 10 0 0 0 0 0
```

**Expected Output Part 1**: 1

**Explanation**:
- Need to calculate area of each shape:
  - Shape 0: 7 cells
  - Shape 1: 5 cells  
  - Shape 2: 7 cells
  - Shape 3: 6 cells
  - Shape 4: 7 cells
  - Shape 5: 7 cells
- First query (10×10 = 100 area): needs 1 of each = 7+5+7+6+7+7 = 39 total. Since 39 ≤ 100, possible.
- Second query (5×5 = 25 area): needs 10×shape 0 = 70 total. Since 70 > 25, impossible.
- Only 1 query is possible.

### Test Case 4 (Empty Query)
**Input** (`test_4.txt`):
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

8x8: 0 0 0 0 0 0
```

**Expected Output Part 1**: 1

**Explanation**: An empty query (no shapes needed) is always possible since 0 ≤ 64.

### Test Case 5 (Exact Fit)
**Input** (`test_5.txt`):
```
0:
###
..#
###

1:
..#
.##
##.

2:
##.
###
#.#

3:
###
.##
..#

4:
#.#
###
#.#

5:
..#
###
###

7x1: 1 0 0 0 0 0
```

**Expected Output Part 1**: 1

**Explanation**:
- Shape 0 has area 7
- Query has area 7×1 = 7
- 7 ≤ 7, so it's possible (exact fit).

**Note**: For the actual puzzle input, the area check heuristic works for all queries. More complex packing algorithms are not required.
