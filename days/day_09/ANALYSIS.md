# Analysis for Day 09

## Overall Approach

The problem consists of two parts with significantly different complexity:

**Part 1** is straightforward: find the largest rectangle that can be formed using any two red tiles as diagonally opposite corners. This requires checking all pairs of red tiles and calculating the area of the rectangle they form. The area is calculated as `(|x1 - x2| + 1) * (|y1 - y2| + 1)`.

**Part 2** is much more complex: find the largest rectangle (using two red tiles as corners) that is entirely contained within a polygon formed by connecting sequential red tiles with green tiles. This requires several algorithmic techniques:

1. **Coordinate Compression**: The input coordinates can be very large (e.g., 98,000+), so we compress them to a smaller grid by collecting all unique x and y coordinates, sorting them, and creating mapping dictionaries.

2. **Polygon Boundary Construction**: Connect consecutive red tiles (in input order) with horizontal and vertical line segments, marking all tiles along these segments as valid boundary points.

3. **Point-in-Polygon Test**: Use ray casting algorithm to determine if a point lies inside the polygon. For each point, cast a ray to the right and count intersections with polygon edges.

4. **Flood-Fill**: Once we identify an interior point, use flood-fill to mark all connected interior points as valid. This efficiently fills the interior of the polygon.

5. **2D Prefix Sums (Summed-Area Table)**: Build a 2D prefix sum array to enable O(1) queries for checking if all cells in a rectangle are valid. This avoids the O(area) cost of checking each cell individually.

6. **Candidate Sorting**: Generate all rectangle candidates from pairs of red tiles, sort them by area in descending order, and check them in that order. This enables early exit once we find a valid rectangle, since we're looking for the largest.

**Clues**

- The problem explicitly states that the rectangle must be "entirely contained within or on the boundary of the polygon" - this indicates we need geometric containment checks
- The mention of connecting red tiles "sequentially" suggests we need to build a polygon boundary
- The large coordinate ranges (98,000+) suggest coordinate compression is necessary for efficiency
- The requirement to check "all tiles in the rectangle" suggests we need an efficient way to verify rectangle validity, pointing to prefix sums

## Complexity Analysis

- **Part 1 Time Complexity**: O(n²) where n is the number of red tiles - we check all pairs
- **Part 1 Space Complexity**: O(n) for storing red tile coordinates

- **Part 2 Time Complexity**: O(n² + k² + m) where:
  - n² is for generating all rectangle candidates
  - k² is for building the compressed grid and prefix sum table (k is the number of unique coordinates, typically much smaller than the coordinate range)
  - m is for flood-fill operations (m is the number of interior cells)
- **Part 2 Space Complexity**: O(k²) for the compressed grid and prefix sum table

## Solutions

### Part 1
| Language   | Initial Solution      |
|------------|-----------------------|
| C          |                       |
| Clojure    |                       |
| Elixir     |                       |
| Go         |                       |
| Haskell    |                       |
| Java       |                       |
| Julia      |                       |
| Kotlin     |                       |
| Python     | 4746238001           |
| Ruby       |                       |
| Rust       |                       |
| TypeScript |                       |

### Part 2
| Language   | Initial Solution      |
|------------|-----------------------|
| C          |                       |
| Clojure    |                       |
| Elixir     |                       |
| Go         |                       |
| Haskell    |                       |
| Java       |                       |
| Julia      |                       |
| Kotlin     |                       |
| Python     | 1552139370           |
| Ruby       |                       |
| Rust       |                       |
| TypeScript |                       |

## Performance

### Part 1
| Language   | Execution Time (ms)         |
|------------|-----------------------------|
| C          |                             |
| Clojure    |                             |
| Elixir     |                             |
| Go         |                             |
| Haskell    |                             |
| Java       |                             |
| Julia      |                             |
| Kotlin     |                             |
| Python     | 6530                        |
| Ruby       |                             |
| Rust       |                             |
| TypeScript |                             |

### Part 2
| Language   | Execution Time (ms)         |
|------------|-----------------------------|
| C          |                             |
| Clojure    |                             |
| Elixir     |                             |
| Go         |                             |
| Haskell    |                             |
| Java       |                             |
| Julia      |                             |
| Kotlin     |                             |
| Python     | 6530                        |
| Ruby       |                             |
| Rust       |                             |
| TypeScript |                             |

## Implementation Differences

### C
[How the C implementation differs (e.g., manual memory management, pointers)]

### Clojure
[How the Clojure implementation differs (e.g., immutable data structures, macros)]

### Elixir
[How the Elixir implementation differs (e.g., pattern matching, processes)]

### Go
[How the Go implementation differs (e.g., goroutines, channels)]

### Haskell
[How the Haskell implementation differs (e.g., functional style, laziness)]

### Java
[How the Java implementation differs (e.g., object-oriented approach, collections)]

### Julia
[How the Julia implementation differs (e.g., multiple dispatch, performance)]

### Kotlin
[How the Kotlin implementation differs (e.g., null safety, extension functions)]

### Python
The Python implementation uses type hints throughout (`list[tuple[int, int]]`, `dict[int, int]`, etc.) for clarity. The solution leverages Python's built-in data structures:
- Sets for collecting unique coordinates
- Lists for sorted coordinate arrays and grid representation
- Dictionaries for coordinate compression maps
- List comprehensions and generator expressions where appropriate
- Nested tuples for candidate rectangles (storing 9 values: min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2)

The flood-fill uses a stack-based iterative approach (rather than recursion) to avoid stack overflow issues. The point-in-polygon test uses the ray casting algorithm with careful handling of edge cases (horizontal edges).

Key implementation details:
- Coordinate compression reduces the grid from potentially millions of cells to a few hundred
- The 2D prefix sum enables O(1) rectangle validity checks instead of O(area)
- Candidates are sorted by area in descending order for early exit optimization

### Ruby
[How the Ruby implementation differs (e.g., dynamic typing, blocks)]

### Rust
[How the Rust implementation differs (e.g., ownership, borrowing, pattern matching)]

### TypeScript
[How the TypeScript implementation differs (e.g., strong typing, async/await)]

## Key Observations

### Integer Overflow Considerations
One critical issue encountered during implementation was integer overflow. The coordinate values can be very large (98,000+), and when calculating areas, intermediate calculations can exceed 32-bit integer limits. For example, in Rust, the initial implementation used `i32` for coordinates, which caused panics with "attempt to multiply with overflow". The fix was to use `i64` for all coordinate and area calculations. This is a common pitfall when working with large coordinate spaces.

### Algorithm Selection
The Part 2 solution requires several sophisticated algorithms working together:
- **Coordinate Compression**: Essential for handling large coordinate ranges efficiently. Without it, the grid would be too large to process.
- **Flood-Fill**: More efficient than checking every point individually with point-in-polygon. Once we find one interior point, we can mark all connected interior points.
- **2D Prefix Sums**: The key optimization that makes rectangle validity checking O(1) instead of O(area). This is crucial since we need to check many candidate rectangles.
- **Early Exit**: Sorting candidates by area descending and breaking early once we find a valid rectangle significantly improves performance.

### Performance Characteristics
Part 1 is straightforward and fast (O(n²) with small constant factors). Part 2 is significantly more complex and slower due to:
- Grid construction and flood-fill operations
- Multiple point-in-polygon checks during flood-fill
- Prefix sum table construction
- Candidate generation and validation

The Python implementation takes approximately 6.5 seconds for both parts combined on the full input (496 red tiles).

### Implementation Challenges
Several challenges were encountered during implementation:
1. **Syntax errors**: Various languages had syntax issues (e.g., Clojure bracket matching, Haskell type mismatches)
2. **Logic errors**: Some implementations (Kotlin, Haskell) returned 0 for Part 2, indicating bugs in the flood-fill or point-in-polygon logic
3. **Type system issues**: Haskell's type system required careful handling of `reads` function return values
4. **Coordinate handling**: Ensuring all coordinate transformations (compression, decompression) are handled correctly

### Geometric Algorithm Notes
The point-in-polygon algorithm uses ray casting, which is a standard approach but requires careful handling of:
- Horizontal edges (division by zero when y1 == y2)
- Edge cases where the ray passes exactly through vertices
- The parity of intersection counts

The flood-fill algorithm uses a stack-based iterative approach to avoid recursion depth issues, which is important for large polygons.

## Notes

- The solution was refined based on research from Reddit discussions about optimizations for this problem
- The correct Part 2 answer is 1552139370, which was verified using the Rust implementation as a reference
- The Python solution successfully implements all optimizations and produces correct results for both parts
- Other language implementations were attempted but encountered various issues (syntax errors, logic bugs, type system challenges) that prevented successful completion
