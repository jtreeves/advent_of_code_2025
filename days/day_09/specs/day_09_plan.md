# Technical Plan: Day 09

## Algorithms and Data Structures

### Algorithm Overview

The problem requires finding the largest rectangle area that can be formed using red tiles as opposite corners, with Part 1 having no additional constraints and Part 2 requiring the rectangle to be fully contained within a polygon boundary.

**Part 1**: Enumerate all pairs of red tiles as opposite corners, calculate rectangle areas, return the maximum.

**Part 2**: Build a polygon from sequential red tiles, then for each rectangle candidate, verify full containment within the polygon before considering its area.

### Key Algorithms

1. **Input Parsing**:
   - Read all lines from input file
   - Parse each line as `x,y` coordinates
   - Store coordinates in a list/array structure

2. **Part 1: Find Largest Rectangle**:
   - For all pairs of red tiles `(i, j)` where `i < j`:
     - Treat them as diagonally opposite corners
     - Calculate area: `(|x_i - x_j| + 1) * (|y_i - y_j| + 1)`
     - Track maximum area
   - Return maximum area

3. **Part 2: Build Polygon Boundary**:
   - Connect sequential red tiles (in input order) with horizontal/vertical line segments
   - Determine which tiles are inside/on the boundary:
     - Build a set/grid of all red and green tiles
     - Green tiles fill the gaps between consecutive red tiles along horizontal/vertical paths
   - The polygon includes all tiles inside or on this boundary

4. **Part 2: Containment Check**:
   - For each rectangle candidate (from red tile pairs):
     - Extract all tiles that belong to the rectangle (interior + edges)
     - Check that every tile is either:
       - A red tile, or
       - A green tile (part of the boundary), or
       - An interior point within the boundary polygon
     - Use point-in-polygon or ray-casting algorithms for interior checks
     - Alternatively, precompute a grid/set of valid tiles and check membership

5. **Part 2: Optimization**:
   - Sort rectangle candidates by area in descending order
   - Stop early once we find a valid rectangle and can guarantee no larger valid ones exist
   - Use coordinate compression to reduce the search space
   - Precompute valid tile set for fast containment checks

### Data Structures

- **Red tiles**: List/array of coordinate pairs `(x, y)`
- **Valid tiles set**: Set/grid containing red tiles + green tiles (for Part 2)
- **Polygon edges**: List of edge segments for containment checking (for Part 2)
- **Rectangle candidates**: List of `(tile1, tile2, area)` tuples, optionally sorted by area

### Why This Approach

- **Part 1**: Brute force is straightforward - O(n²) to check all pairs, O(1) per area calculation. Simple and correct.
- **Part 2**: Requires polygon construction and containment checking. The polygon boundary is explicit (red tiles + connecting green tiles), so we can build a valid-tile set and check membership, or use geometric algorithms for containment.

### Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n²) where n = number of red tiles - need to check all pairs
  - Part 2: O(n² * R) where R is the rectangle size for containment checks, or O(n² * log n) with optimized containment
    - Building polygon: O(n * M) where M is max coordinate range for green tiles
    - Containment checks: O(rectangle_area) per check if using set membership, or O(log n) with spatial structures
- **Space Complexity**: 
  - Part 1: O(n) for storing coordinates
  - Part 2: O(n * M) for storing valid tile set/grid, where M is the coordinate range

## Approach

### Part 1

1. **Parse Input**:
   - Read all lines, parse `x,y` coordinates
   - Store in list: `[(x1, y1), (x2, y2), ...]`

2. **Find Maximum Area**:
   - Initialize `max_area = 0`
   - For each pair `(i, j)` where `i < j`:
     - Calculate `width = abs(coords[i].x - coords[j].x) + 1`
     - Calculate `height = abs(coords[i].y - coords[j].y) + 1`
     - Calculate `area = width * height`
     - Update `max_area = max(max_area, area)`

3. **Return Result**:
   - Return `max_area`

### Part 2

1. **Parse Input** (same as Part 1):
   - Read and store red tile coordinates

2. **Build Polygon Boundary**:
   - Create a set/grid to mark valid tiles
   - Add all red tiles to the valid set
   - For each consecutive pair of red tiles (in input order):
     - If same x: fill vertical line from `min(y1, y2)` to `max(y1, y2)` with green tiles
     - If same y: fill horizontal line from `min(x1, x2)` to `max(x1, x2)` with green tiles
     - If different x and y: this shouldn't happen if connecting sequentially - handle as needed
     - Add all green tiles to the valid set
   - Determine interior region:
     - Use flood-fill from a known interior point, or
     - Use point-in-polygon test (ray casting or winding number)

3. **Find Maximum Valid Rectangle**:
   - For all pairs of red tiles `(i, j)`:
     - Calculate rectangle area
     - Determine all tiles in the rectangle (corners + edges + interior)
     - Check that all these tiles are in the valid set (red or green) or interior
     - If valid, track maximum area
   - Return maximum valid area

4. **Optimization Strategies**:
   - Sort candidates by area descending - check largest first, stop early when appropriate
   - Precompute valid tile grid for O(1) membership checks
   - Use bounding box checks before detailed containment verification

## Implementation Notes

### Area Calculation
The formula `(|x1 - x2| + 1) * (|y1 - y2| + 1)` is critical. The `+1` must be outside the absolute value:
- Correct: `(abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)`
- Incorrect: `abs(x1 - x2 + 1) * abs(y1 - y2 + 1)`

### Part 2 Polygon Construction
When connecting sequential red tiles:
- The polygon is formed by following the input order
- Each connection is a straight line (horizontal or vertical only based on the problem description)
- All tiles on these connecting lines are "green tiles" and part of the valid region
- Need to determine which region is "inside" vs "outside" the boundary

### Part 2 Containment Testing
Options for checking if a rectangle is fully contained:
1. **Grid/Set Membership**: Build a set of all valid tiles (red + green + interior). Check every tile in rectangle is in the set.
2. **Point-in-Polygon**: For each tile in rectangle, test if it's inside/on the polygon using ray casting.
3. **Edge Intersection**: Check if rectangle edges intersect polygon edges incorrectly.

The problem states "all tiles in the rectangle must be within or on the boundary", so membership-based checking may be simplest if we can efficiently build the valid tile set.

### Edge Cases
- Handle negative coordinates correctly
- Ensure area calculations don't overflow for large coordinates
- Handle cases where polygon is self-intersecting (if possible in input)
- Handle degenerate rectangles (width or height = 1)

## Testing Strategy

- Test Part 1 with simple examples (2-4 tiles)
- Test Part 2 with simple boundaries (square, rectangle)
- Test Part 2 with complex boundaries (L-shape, U-shape)
- Test edge cases: single tile, two tiles, colinear tiles
- Verify area formula with known examples
- Test with negative coordinates if applicable
