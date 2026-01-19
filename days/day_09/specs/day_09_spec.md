# Specification: Day 09

## Problem Statement

The Elves have set up a movie theater at the North Pole, and they're planning to show some special holiday films. To make the viewing experience great, they've marked certain seats with red tiles that indicate the best viewing spots.

You're given a list of coordinates representing red tiles placed on a 2D grid. Each coordinate is provided as `x,y` on a single line, where both `x` and `y` are integers.

## Requirements

### Part 1: Largest Rectangle from Red Tiles

- Parse a list of red tile coordinates from the input file (one `x,y` coordinate per line)
- Find all pairs of red tiles that can serve as diagonally opposite corners of a rectangle
- The rectangle must be axis-aligned (sides parallel to x and y axes)
- Calculate the area of each rectangle using the formula: `(|x1 - x2| + 1) * (|y1 - y2| + 1)`
- Return the largest area among all such rectangles

**Important**: Both corner tiles must be red tiles from the input. The interior points of the rectangle do not need to be red tiles - only the two corners matter.

### Part 2: Constrained by Enclosed Region

- Parse the same list of red tile coordinates
- Connect sequential red tiles (in the order they appear in the input) with horizontal and vertical line segments made of green tiles
- This forms a closed polygon boundary (the region inside and on the boundary is valid)
- Find all pairs of red tiles that can serve as diagonally opposite corners of a rectangle
- For each rectangle, check that **all tiles** (interior and edges) are within or on the polygon boundary
- Return the largest area among all valid rectangles

**Important**: The entire rectangle must be contained within or on the boundary - no part can extend outside the polygon.

## Input Format

A text file with one coordinate per line in the format `x,y`, where `x` and `y` are integers.

Example:
```
0,0
3,0
0,2
3,2
```

This represents 4 red tiles at coordinates (0,0), (3,0), (0,2), and (3,2).

## Output Format

For Part 1: A single integer representing the largest rectangle area.

For Part 2: A single integer representing the largest rectangle area that is fully contained within the polygon boundary.

## Constraints

- Coordinates can be large (positive or negative integers)
- Number of red tiles can be significant (hundreds or more)
- For Part 1: Only the two corner tiles must be red; interior points don't matter
- For Part 2: All tiles in the rectangle must be within or on the polygon boundary
- The polygon is formed by connecting sequential red tiles in input order with straight horizontal/vertical segments
- Rectangle area formula: `(|x1 - x2| + 1) * (|y1 - y2| + 1)` - the `+1` is critical and must be outside the absolute value

## Test Cases

### Test Case 1 (Simple Square - Part 1)
**Input** (`test_1.txt`):
```
0,0
3,0
0,3
3,3
```

**Expected Output Part 1**: 16

**Explanation**: 
- Rectangle with corners (0,0) and (3,3) has area `(3 - 0 + 1) * (3 - 0 + 1) = 4 * 4 = 16`
- This is the largest possible rectangle

### Test Case 2 (Larger Rectangle - Part 1)
**Input** (`test_2.txt`):
```
0,0
0,5
10,0
10,5
5,2
```

**Expected Output Part 1**: 66

**Explanation**:
- Rectangle with corners (0,0) and (10,5) has area `(10 - 0 + 1) * (5 - 0 + 1) = 11 * 6 = 66`
- Other rectangles are smaller

### Test Case 3 (Part 2 - Simple Boundary)
**Input** (`test_3.txt`):
```
0,0
5,0
5,5
0,5
```

**Expected Output Part 2**: 36

**Explanation**:
- The red tiles form a square boundary when connected sequentially: (0,0) → (5,0) → (5,5) → (0,5) → back to (0,0)
- This creates a closed polygon containing all points from (0,0) to (5,5) inclusive
- The largest rectangle with red corners is from (0,0) to (5,5) with area `(5 - 0 + 1) * (5 - 0 + 1) = 6 * 6 = 36`
- All points of this rectangle are within the polygon

### Test Case 4 (Part 2 - L-Shape Boundary)
**Input** (`test_4.txt`):
```
0,0
3,0
3,1
5,1
5,3
0,3
```

**Expected Output Part 2**: 4

**Explanation**:
- The boundary forms an L-shape when connected sequentially
- The largest valid rectangle might be smaller due to the L-shape constraining available space
- This tests containment logic for non-rectangular polygons

### Test Case 5 (Part 2 - More Complex Shape)
**Input** (`test_5.txt`):
```
0,0
4,0
4,2
2,2
2,4
0,4
```

**Expected Output Part 2**: 15

**Explanation**:
- Creates a U-shaped boundary
- Tests that rectangles must be fully contained within the boundary
- The largest valid rectangle will depend on which corners can form valid rectangles within the U-shape

## Edge Cases

- Single red tile: No valid rectangle (return 0 or handle appropriately)
- Two red tiles: Form a 1x1 or 1xN rectangle
- All tiles in a line: May form only narrow rectangles
- Overlapping rectangles: Return the maximum area
- Negative coordinates: Handle correctly in area calculations

## Notes

- The area formula `(|x1 - x2| + 1) * (|y1 - y2| + 1)` is critical - the `+1` must be outside the absolute value
- For Part 2, careful polygon containment checking is required - all points of a rectangle must be inside or on the boundary
- Performance considerations: With many red tiles, checking all pairs can be O(n²), and for Part 2, containment checking adds significant overhead
- Optimization strategies: Sort candidates by area descending, use coordinate compression, or spatial data structures for Part 2
