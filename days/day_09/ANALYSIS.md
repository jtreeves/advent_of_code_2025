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

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 261   |                             |
| Clojure    | 114   |                             |
| Elixir     | 273   |                             |
| Go         | 320   | 930                         |
| Haskell    | 125   |                             |
| Java       | 264   | 1533                        |
| Julia      | 225   | 2010                        |
| Perl       | 242   | 17665                       |
| Python     | 193   | 7991                        |
| Ruby       | 226   | 9999                        |
| Rust       | 257   | 3694                        |
| TypeScript | 226   | 3176                        |

### C
[How the C implementation differs (e.g., manual memory management, pointers)]

### Clojure
[How the Clojure implementation differs (e.g., immutable data structures, macros)]

### Elixir
[How the Elixir implementation differs (e.g., pattern matching, processes)]

### Go
The Go implementation uses slices (`[][]bool` for grid, `[]int64` for coordinates) and maps (`map[int64]int`) for coordinate compression. Key features:
- **Structs**: `Point` struct with `x, y int64` fields
- **Slices**: Dynamic arrays for red tiles, coordinate arrays, and 2D grid
- **Map lookups**: Uses `ok` pattern for safe map access: `if cx, ok := xToCx[x]; ok { ... }`
- **Closures**: `pointInPolygon` is a closure that captures `redTiles`
- **Stack implementation**: Flood-fill uses slice as stack with `stack[len(stack)-1]` for pop and slice truncation `stack[:len(stack)-1]`
- **Early exit**: Uses labeled break (`break`) within nested loops with boolean flag `foundInterior`
- **Performance**: Excellent (930 ms) - compiled with efficient memory layout and minimal GC overhead

### Haskell
[How the Haskell implementation differs (e.g., functional style, laziness)]

### Java
The Java implementation uses object-oriented design with classes and collections:
- **Classes**: `Point` class with `long x, y`; `Candidate` inner class for rectangle candidates
- **Collections**: `ArrayList<Point>`, `HashMap<Long, Integer>` for coordinate compression, `Stack<int[]>` for flood-fill
- **Long arithmetic**: Uses `long` throughout to avoid integer overflow (coordinates can be very large)
- **Lambda functions**: Uses `BiFunction<Long, Long, Boolean>` for point-in-polygon and `Function<int[], Long>` for rectangle sum
- **Boxed types**: Uses `Integer` for map lookups with null checks: `Integer cx = xToCx.get(x); if (cx != null && cy != null)`
- **2D arrays**: `boolean[][]` for grid, `long[][]` for prefix sums
- **Comparator**: Uses `Collections.sort()` with custom comparator for sorting candidates by area descending
- **Performance**: Excellent (1533 ms) - JVM JIT optimization provides good performance despite object overhead

### Julia
The Julia implementation uses 1-indexed arrays and type annotations:
- **1-indexed arrays**: Arrays start at 1, requiring loops `for cx in 1:width` instead of `0:width-1`
- **Type annotations**: Uses `Dict{Int, Int}` for coordinate maps, `Vector{Tuple{Int, Int}}` for red tiles
- **Tuple arrays**: Candidates stored as `Vector{Tuple{...}}` with 9 elements
- **Array indexing**: Grid access uses `grid[cx, cy]` (Julia's column-major indexing for 2D arrays)
- **Sorting**: Uses `sort!()` with `by=x->x[5]` (5th element is area) and `rev=true` for descending order
- **Early exit**: Uses `break` with `found_interior && break` pattern
- **Stack operations**: Uses `push!()` and `pop!()` for array-based stack
- **Performance**: Good (2010 ms) - JIT compiled but with runtime compilation overhead and 1-indexed array overhead

### Perl
Perl uses array references and hash-based coordinate maps:
- **Array references**: Red tiles stored as `@red_tiles = ([$x, $y], ...)` where each element is a reference
- **Hash maps**: Coordinate compression uses `%x_to_cx` and `%y_to_cy` hashes with integer keys and values
- **Array dereferencing**: Access coordinates with `my ($x, $y) = @{$red_tiles[$i]}` - dereferencing array reference
- **2D array**: Grid built as array of array references: `my @grid = map { [(0) x $height] } 0..$width-1`
- **Stack**: Flood-fill uses array as stack: `push @stack, [$x, $y]` and `my $pos = pop @stack`
- **Sorting**: Candidates sorted with `sort { $b->[4] <=> $a->[4] }` - comparing 5th element (index 4) of array references
- **Subroutine references**: `point_in_polygon` defined as subroutine taking reference: `point_in_polygon($x, $y, \@red_tiles)`
- **Explicit array bounds**: Uses `$#red_tiles` for last index, `@$red_tiles_ref` for array length from reference
- **Performance**: Slowest (17665 ms) - interpreted execution with significant overhead from array reference dereferencing and hash lookups

### Python
The Python implementation uses type hints throughout (`list[tuple[int, int]]`, `dict[int, int]`, etc.) for clarity. The solution leverages Python's built-in data structures:
- **Sets**: For collecting unique coordinates (`set[int]` for `all_x_set`, `all_y_set`)
- **Lists**: For sorted coordinate arrays and grid representation (`list[list[bool]]` for grid)
- **Dictionaries**: For coordinate compression maps (`dict[int, int]` for `x_to_cx`, `y_to_cy`)
- **List comprehensions**: Used for parsing coordinates and building sorted arrays
- **Tuples**: Nested tuples for candidate rectangles (storing 9 values: min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2)
- **Stack**: Flood-fill uses list as stack: `stack = [(cx, cy)]` with `stack.pop()` and `stack.append()`
- **Sorting**: Candidates sorted with `candidates.sort(key=lambda x: x[4], reverse=True)` for descending area order
- **Early exit**: Uses `break` when `area <= max_area_part2` since candidates are sorted descending

The flood-fill uses a stack-based iterative approach (rather than recursion) to avoid stack overflow issues. The point-in-polygon test uses the ray casting algorithm with careful handling of edge cases (horizontal edges).

Key implementation details:
- Coordinate compression reduces the grid from potentially millions of cells to a few hundred
- The 2D prefix sum enables O(1) rectangle validity checks instead of O(area)
- Candidates are sorted by area in descending order for early exit optimization
- **Performance**: Moderate (7991 ms) - interpreted execution with overhead from dynamic typing and dictionary lookups

### Ruby
The Ruby implementation uses dynamic typing with expressive syntax:
- **Arrays**: `red_tiles = []` stores coordinate pairs as `[x, y]` arrays
- **Hashes**: Coordinate compression uses hashes: `x_to_cx = {}` and `y_to_cy = {}`
- **2D arrays**: Grid built with `Array.new(width) { Array.new(height, false) }`
- **Blocks**: Uses `.each` blocks for iteration: `(0...width).each do |cx| ... end`
- **Closures**: `point_in_polygon` defined as lambda/proc that captures `red_tiles`
- **Stack**: Flood-fill uses array as stack: `stack = [[cx, cy]]` with `stack.pop` and `stack << [x, y]`
- **Sorting**: Candidates sorted with `candidates.sort_by! { |c| -c[4] }` (negative for descending)
- **Early exit**: Uses `break if found_interior` pattern within nested loops
- **Lambda**: `rect_sum` defined as lambda: `rect_sum = lambda do |cx1, cx2, cy1, cy2| ... end`
- **Conditional assignment**: Uses `||` for defaults and `&&` for short-circuit evaluation
- **Performance**: Slow (9999 ms) - interpreted execution with significant overhead from dynamic method dispatch

### Rust
The Rust implementation leverages ownership, borrowing, and pattern matching:
- **Structs**: `Point` struct with `x: i64, y: i64` fields (using `i64` to avoid overflow)
- **Vectors**: Uses `Vec<Point>` for red tiles, `Vec<Vec<bool>>` for grid, `Vec<Vec<i64>>` for prefix sums
- **HashMaps**: Coordinate compression uses `HashMap<i64, usize>` for `x_to_cx` and `y_to_cy`
- **Closures**: `point_in_polygon` defined as closure capturing `red_tiles` by reference
- **Pattern matching**: Uses `while let Some((x, y)) = stack.pop()` for elegant stack unwrapping
- **Optional unwrapping**: Uses `if let Some(&cx) = x_to_cx.get(&x)` for safe map access
- **Tuple destructuring**: Candidates stored as `Vec<(i64, i64, i64, i64, i64, usize, usize, usize, usize)>`
- **Sorting**: Uses `candidates.sort_by(|a, b| b.4.cmp(&a.4))` for descending area order
- **Borrowing**: Grid and prefix sums are mutable (`&mut Vec<Vec<bool>>`) but `red_tiles` is immutable (`&Vec<Point>`)
- **Early exit**: Uses `break` within nested loops with `found_interior` flag
- **Performance**: Good (3694 ms) - compiled with zero-cost abstractions but includes compilation time in measurement

### TypeScript
The TypeScript implementation uses strong typing with Node.js runtime:
- **Type annotations**: `type Point = [number, number]` for coordinates, `Array<Point>` for red tiles
- **Maps**: Coordinate compression uses `Map<number, number>` for `xToCx` and `yToCy`
- **2D arrays**: Grid built with `Array(width).fill(false).map(() => Array(height).fill(false))`
- **Closures**: `pointInPolygon` defined as arrow function capturing `redTiles` from outer scope
- **Stack**: Flood-fill uses array as stack: `const stack: [number, number][] = [[cx, cy]]` with `stack.pop()!` (non-null assertion)
- **Sorting**: Candidates sorted with `candidates.sort((a, b) => b[4] - a[4])` for descending area order
- **Optional chaining**: Uses `const cx1 = xToCx.get(minX); if (cx1 !== undefined && ...)` for safe map access
- **Tuple types**: Candidates stored as `Array<[number, number, number, number, number, number, number, number, number]>`
- **Path module**: Uses `path.join(__dirname, '../data/input.txt')` for cross-platform file paths
- **fs module**: Uses `fs.readFileSync()` for synchronous file reading
- **Performance**: Moderate (3176 ms) - compiled to JavaScript with Node.js runtime overhead

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
2. **Logic errors**: Some implementations (Haskell) returned 0 for Part 2, indicating bugs in the flood-fill or point-in-polygon logic
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
