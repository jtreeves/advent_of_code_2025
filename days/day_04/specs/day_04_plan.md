# Technical Plan: Day 04

## Algorithms and Data Structures

### Algorithm Overview

The problem requires processing a 2D grid where each cell is either empty (`.`) or contains a roll of paper (`@`). 

**Part 1**: Count cells with `@` that have fewer than 4 neighbors (out of 8 possible) that are also `@`.

**Part 2**: Iteratively remove all accessible rolls until no more can be removed, counting total removals.

### Key Algorithms

1. **Grid Parsing**: Read the input file line by line, convert to a 2D grid (array of strings or 2D array of characters).

2. **Neighbor Counting**: For a cell at position `(i, j)`, check the 8 neighbors:
   - `(i-1, j-1)`, `(i-1, j)`, `(i-1, j+1)`
   - `(i, j-1)`, `(i, j+1)`
   - `(i+1, j-1)`, `(i+1, j)`, `(i+1, j+1)`
   - Count how many of these neighbors contain `@`
   - Handle boundary conditions (don't check out-of-bounds positions)

3. **Part 1 Implementation**:
   - Parse grid from input
   - For each cell containing `@`:
     - Count neighbors that are `@` (check all 8 directions, respecting boundaries)
     - If count < 4, increment accessible counter
   - Return total count of accessible rolls

4. **Part 2 Implementation**:
   - Parse grid from input
   - Initialize removed counter = 0
   - Repeat until no changes:
     - Mark all accessible rolls (those with < 4 neighbors that are `@`) for removal
     - Remove all marked rolls, increment counter by number removed
     - Continue until a round produces no removals
   - Return total removed counter

### Data Structures

- **Grid representation**: 
  - Option 1: 2D array/vector of characters (`char[][]` or `Vec<Vec<char>>`)
  - Option 2: Array/vector of strings (`string[]` or `Vec<String>`)
  - Option 3: Flat array with index calculation `grid[i * cols + j]`
  
- **Boundary checking**: Store grid dimensions (rows, cols) to check bounds before accessing neighbors

- **Part 2 removal**: 
  - Option 1: In-place removal (set to `.`) and check again
  - Option 2: Track to-remove positions in a set/list, remove all at once
  - Option 2 is cleaner and avoids issues with checking newly removed cells

### Why This Approach is Optimal

- **Time Complexity**: 
  - Part 1: O(rows * cols) - must check each cell and its 8 neighbors
  - Part 2: O(rows * cols * rounds) where rounds is number of iterations until convergence
  - This is optimal since we must examine each cell and its neighbors
  
- **Space Complexity**: O(rows * cols) - need to store the grid

- The problem inherently requires checking every cell's neighbors, so the algorithm cannot be more efficient

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(rows * cols) where rows and cols are grid dimensions
  - Part 2: O(rows * cols * k) where k is the number of removal rounds (typically small, worst case could be up to rows*cols if removing one at a time)
- **Space Complexity**: O(rows * cols) - storage for the grid

## Approach

1. **Parse Input**: Read input file, convert lines to grid representation (2D array or array of strings)

2. **Part 1**:
   - Iterate through all grid cells
   - For each `@` cell, count neighbors that are `@`
   - If neighbor count < 4, increment accessible counter
   - Return total accessible count

3. **Part 2**:
   - Copy grid (since we'll modify it)
   - While there are changes:
     - Identify all accessible rolls (those with < 4 neighbors that are `@`)
     - Remove them all at once (set to `.`)
     - Increment removed counter
     - Repeat until a round produces no removals
   - Return total removed count

### Neighbor Checking Helper Function

Create a helper function `countNeighbors(grid, i, j)` that:
- Checks all 8 directions from position (i, j)
- Returns count of neighbors that are `@`
- Handles boundary checking (don't access out-of-bounds)

### Part 2 Iteration Strategy

1. Use a flag to track if any removals occurred in a round
2. Scan entire grid, identify all accessible positions
3. Collect positions to remove (store in list/set)
4. Remove all marked positions at once
5. If removals occurred, continue; else stop

## Implementations

### C
- Use 2D char array or array of strings
- Manual boundary checking with `if (i >= 0 && i < rows && j >= 0 && j < cols)`
- Step-by-step:
  1. Read file, count lines and max line length
  2. Allocate grid memory
  3. Read lines into grid
  4. For Part 1: iterate, count neighbors, check < 4
  5. For Part 2: loop until no changes, mark positions, remove, repeat
  6. Return count as int

### Clojure
- Use vector of vectors of characters or vector of strings
- Use `get-in` with bounds checking
- Step-by-step:
  1. Read file with `slurp`, split by lines
  2. Convert to vector of vectors/strings
  3. Use `for` or `map` to iterate through positions
  4. Helper function `count-neighbors` with `get-in` checks
  5. For Part 2: use `loop` with immutable updates (replace accessible with `.`)
  6. Return count

### Elixir
- Use list of lists or list of strings (or use `MapSet` for positions)
- Pattern matching for bounds checking
- Step-by-step:
  1. Read file with `File.read!`, split by lines
  2. Convert to list of lists or use tuples for positions `{row, col}`
  3. Use `Enum` pipelines with `with` for bounds checking
  4. Helper function with `case` for valid neighbors
  5. For Part 2: use recursion or `Stream.iterate` until no changes
  6. Return count

### Go
- Use `[][]byte` or `[]string`
- Simple bounds checking with `if i >= 0 && i < rows`
- Step-by-step:
  1. Read file with `bufio.Scanner`, build slice of strings
  2. Store rows and cols
  3. Iterate with nested loops, check neighbors
  4. Helper function `countNeighbors(grid [][]byte, i, j int) int`
  5. For Part 2: use `for` loop with `changed` flag, mark positions, remove, repeat
  6. Return count as int

### Haskell
- Use `[[Char]]` or `Vector (Vector Char)`
- Use guards or `Maybe` for bounds checking
- Step-by-step:
  1. Read file with `readFile`, split by lines
  2. Convert to list of lists
  3. Use list comprehensions or `map` with `zip` for indexing
  4. Helper function with `filter` on valid neighbor positions
  5. For Part 2: use recursion or `until` to iterate removal
  6. Return count

### Java
- Use `char[][]` or `List<String>`
- Array bounds checking with `if` statements
- Step-by-step:
  1. Read file with `Files.readAllLines` or `BufferedReader`
  2. Convert to 2D char array or list
  3. Use nested loops with bounds checking
  4. Helper method `countNeighbors(char[][] grid, int i, int j)`
  5. For Part 2: use `while` loop with `boolean changed`, mark positions, remove, repeat
  6. Return count as int

### Julia
- Use `Matrix{Char}` or `Vector{String}`
- Simple indexing with bounds checking
- Step-by-step:
  1. Read file with `readlines`, convert to matrix or vector of strings
  2. Use `eachindex` or nested loops
  3. Helper function with `checkbounds` or manual checks
  4. For Part 2: use `while` loop with flag, mark positions, remove, repeat
  5. Return count

### Perl
- Use array of strings `@lines` initially, then convert to 2D array of arrays: `@grid = map { [split //, $_] } @lines`
- Character access: `substr($grid->[$ni], $nj, 1)` or `$grid[$i][$j]` for array-of-arrays
- String array reference: pass `\@lines` or `\@grid_str` to functions
- Grid rebuilding: convert back to strings with `map { join('', @$_) } @grid`
- Step-by-step implementation:
  1. Read lines and convert to 2D array: `map { [split //, $_] } @lines`
  2. For Part 1, iterate through grid checking each cell with `substr($lines[$i], $j, 1)`
  3. Count 8-directional neighbors with nested loops `for my $di (-1..1)`
  4. For Part 2, use `while (1)` loop with `last unless @to_remove` to continue until no changes
  5. Build removal list as array of array references: `push @to_remove, [$i, $j]`
  6. Modify grid in-place: `$grid[$i][$j] = '.'`
- Step-by-step:
  1. Read file with `File.readLines()`
  2. Convert to list of strings or 2D array
  3. Use `forEachIndexed` or nested loops
  4. Helper function with bounds checking
  5. For Part 2: use `while` loop with `var changed`, mark positions, remove, repeat
  6. Return count as Int

### Python
- Use `List[str]` or `List[List[str]]`
- Simple indexing with bounds checking
- Step-by-step:
  1. Read file, split by lines into list of strings
  2. Store rows and cols
  3. Use nested loops or list comprehension
  4. Helper function `count_neighbors(grid, i, j)` with bounds checking
  5. For Part 2: use `while True` with `changed` flag, collect positions, remove all, repeat
  6. Return count as int

### Ruby
- Use `Array<String>` or `Array<Array<String>>`
- Bounds checking with `if` and array length
- Step-by-step:
  1. Read file with `File.readlines` or `File.read.split("\n")`
  2. Convert to array of strings or 2D array
  3. Use `each_with_index` or nested loops
  4. Helper method with bounds checking
  5. For Part 2: use `loop` with `changed` flag, mark positions, remove all, repeat
  6. Return count

### Rust
- Use `Vec<Vec<char>>` or `Vec<String>`
- Bounds checking with `get()` method (returns `Option`)
- Step-by-step:
  1. Read file with `fs::read_to_string`, split by lines
  2. Convert to `Vec<Vec<char>>` or `Vec<String>`
  3. Use iterators with `enumerate` or nested loops
  4. Helper function using `get()` for safe indexing
  5. For Part 2: use `loop` with `bool` flag, collect positions, remove all, repeat
  6. Return count as `usize` or `i32`

### TypeScript
- Use `string[]` or `string[][]`
- Bounds checking with `if` statements
- Step-by-step:
  1. Read file with `fs.readFileSync`, split by lines
  2. Convert to array of strings
  3. Use nested loops or `forEach` with index
  4. Helper function with bounds checking
  5. For Part 2: use `while` loop with flag, collect positions, remove all, repeat
  6. Return count as number
