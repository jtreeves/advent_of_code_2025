# Analysis for Day 07

## Overall Approach

The solution processes the grid row-by-row, simulating tachyon beam propagation. For Part 1, we track active beam positions using a set and count how many times beams encounter splitters. For Part 2, we use a 2D count matrix to track beam counts per cell, allowing us to handle beam merging correctly. The key insight is that beams only move downward, so we can process the grid sequentially without complex path tracking.

**Clues**

- Beams always move downward, enabling row-by-row processing
- Splitters create two new beams from adjacent cells, requiring careful tracking
- Beams can merge (multiple beams reach same cell), necessitating count tracking for Part 2

## Complexity Analysis

- **Time Complexity**: O(rows × cols) - we process each cell at most once per row
- **Space Complexity**: O(rows × cols) for Part 2 (beam count matrix), O(cols) for Part 1 (active beam set)

## Solutions

### Part 1
| Language   | Initial Solution |
|------------|------------------|
| C          | 1622             |
| Clojure    | 1622             |
| Elixir     | 1622             |
| Go         | 1622             |
| Haskell    | 1622             |
| Java       | 1622             |
| Julia      | 1622             |
| Kotlin     | 1622             |
| Python     | 1622             |
| Ruby       | 1622             |
| Rust       | 1622             |
| TypeScript | 1622             |

### Part 2
| Language   | Initial Solution    |
|------------|---------------------|
| C          | 10357305916520      |
| Clojure    | 10357305916520      |
| Elixir     | 10357305916520      |
| Go         | 10357305916520      |
| Haskell    | 10357305916520      |
| Java       | 10357305916520      |
| Julia      | 10357305916520      |
| Kotlin     | 10357305916520      |
| Python     | 10357305916520      |
| Ruby       | 10357305916520      |
| Rust       | 10357305916520      |
| TypeScript | 10357305916520      |

## Performance

### Part 1
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 255                 |
| Clojure    | 1167                |
| Elixir     | 526                 |
| Go         | 518                 |
| Haskell    | 615                 |
| Java       | 266                 |
| Julia      | 790                 |
| Kotlin     | 321                 |
| Python     | 74                  |
| Ruby       | 197                 |
| Rust       | 323                 |
| TypeScript | 2137                |

### Part 2
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 255                 |
| Clojure    | 1167                |
| Elixir     | 526                 |
| Go         | 518                 |
| Haskell    | 615                 |
| Java       | 266                 |
| Julia      | 790                 |
| Kotlin     | 321                 |
| Python     | 74                  |
| Ruby       | 197                 |
| Rust       | 323                 |
| TypeScript | 2137                |

## Implementation Differences

### C
The C implementation uses manual memory management with `malloc`/`realloc` for dynamic arrays. For Part 1, active beams are tracked using integer arrays (`calloc`) where each index represents a column. For Part 2, uses a 2D `long long` array (`beam_counts`) allocated row-by-row for tracking beam counts. String parsing manually splits content by replacing newlines with null terminators. Uses `calloc` for zero-initialization of arrays. Manual cleanup with `free()` for all allocated memory.

### Clojure
Clojure uses immutable data structures and functional transformations. Part 1 uses `loop/recur` for tail-recursive row processing with immutable sets (`#{...}`) for active beams. Part 2 uses nested `reduce` operations to build immutable vectors representing the beam count matrix. String indexing uses `get` and `nth` since strings can be indexed directly in Clojure. The `cond->` threading macro elegantly handles conditional beam addition based on bounds checking. All data structures are immutable throughout.

### Elixir
Elixir uses pattern matching and `MapSet` for Part 1 active beam tracking. Part 2 uses nested lists (`for` comprehensions) with `Enum.at` for access and `List.replace_at` for updates (immutable operations). Pattern matching on function heads handles edge cases (e.g., `find_start(_grid, r, _c, rows, _cols) when r >= rows`). Uses anonymous functions within pipes (`|>`) to handle conditional beam addition. String-to-charlist conversion (`String.to_charlist/1`) enables character-level indexing.

### Go
Go uses `map[int]bool` for Part 1 active beam tracking, creating a new map each row. Part 2 uses `[][]int` slices allocated with `make`. Simple imperative style with explicit bounds checking (`col-1 >= 0`). Uses byte slices (`[]byte`) for grid representation converted from strings. Error handling via early returns. The implementation is straightforward with no special Go features like goroutines needed.

### Haskell
Haskell uses pure functional recursion with `Data.Set` for Part 1 active beam tracking. Part 2 uses nested lists of `Integer` with helper function `update2D` to handle immutable updates. Uses guards (`| r >= rows = ...`) for pattern matching. Grid is represented as `[String]` with direct indexing `!!`. The `go` helper functions use tail recursion via pattern matching on row indices. Type annotations ensure `Integer` for Part 2 large values.

### Java
Java uses `Set<Integer>` for Part 1 active beam tracking with standard collection operations. Part 2 uses `long[][]` 2D arrays with direct indexing and assignment. Character grid is `char[][]` converted from string arrays. Uses standard Java collections (`HashSet`) and array operations. The implementation is straightforward OOP style with no advanced Java features.

### Julia
Julia uses `Set{Int}` for Part 1 active beam tracking. Part 2 uses 2D arrays (`zeros(Int64, rows, cols)`) with 1-based indexing (`1:rows`, `1:cols`). Uses `collect` to convert strings to character arrays. Array operations are straightforward with `sum` for aggregation. Note the 1-based indexing throughout (Julia convention) versus 0-based in most other languages.

### Kotlin
Kotlin uses `MutableSet<Int>` for Part 1 active beam tracking with standard collection operations. Part 2 uses `Array<LongArray>` for the 2D count matrix. Uses `grid[r][c]` for character access and `grid.map { it.toCharArray() }` for conversion. The implementation is concise with Kotlin's collection methods but requires explicit type declarations for arrays.

### Python
Python uses `set` for Part 1 active beam tracking. Part 2 uses list comprehensions `[[0] * cols for _ in range(rows)]` for 2D initialization. Grid is `[list(line) for line in lines]` converting strings to lists. Simple and readable with Python's list and set operations. Type hints (`tuple[str, str]`) provide clarity but don't affect runtime.

### Ruby
Ruby uses `Set` (requires `require 'set'`) for Part 1 active beam tracking. Part 2 uses nested arrays `Array.new(rows) { Array.new(cols, 0) }` for 2D initialization. Uses `lines.map { |line| line.chars }` for grid conversion. Ruby's elegant collection methods (`each`, `sum`) make the code concise. Note the explicit `Set` requirement.

### Rust
Rust uses `HashSet<usize>` for Part 1 active beam tracking, creating new sets each iteration. Part 2 uses `Vec<Vec<i64>>` for the 2D count matrix with bounds checking. Character grid is `Vec<Vec<char>>` with ownership management. Uses iterator methods (`for r in (start_row + 1)..rows`) for ranges. The `!found` pattern is used for early returns. Standard Rust ownership and borrowing rules apply.

### TypeScript
TypeScript uses `Set<number>` for Part 1 active beam tracking with standard JavaScript Set operations. Part 2 uses `number[][]` 2D arrays with type annotations. Grid is `lines.map(line => line.split(''))` creating string arrays. Uses standard JavaScript array methods (`reduce`, `sum`). Type annotations provide clarity but runtime behavior is identical to JavaScript.

## Key Observations

The most significant difference across implementations is how languages handle data structure updates for Part 2:

- **Mutable updates (C, Go, Java, TypeScript, Python, Kotlin, Ruby, Julia)**: These languages directly modify array/matrix elements in-place (`beam_counts[r][c] += prev_count`). This is efficient and straightforward, with C using pointers and others using array indexing.

- **Immutable updates (Clojure, Haskell, Elixir)**: These languages build new data structures during updates. Clojure uses `reduce` to construct new vectors. Haskell uses helper functions like `update2D` to create new lists. Elixir uses `List.replace_at` which creates new lists. This is more functional but potentially less memory-efficient.

- **Set operations for Part 1**: All languages use set-like structures (sets, hash sets, maps) for tracking active beam columns, as we only need presence, not counts. The uniformity demonstrates this is the optimal approach.

**Performance observations:**
- Python (74ms) is surprisingly fast, likely due to optimized set operations and list comprehensions
- TypeScript (2137ms) is significantly slower, likely due to Node.js interpretation overhead and dynamic typing
- Compiled languages (C 255ms, Rust 323ms, Java 266ms) perform well, with C being fastest
- Functional languages (Clojure 1167ms, Haskell 615ms, Elixir 526ms) show varied performance, with Elixir performing best among them
- Ruby (197ms) performs surprisingly well, outperforming several compiled languages
- Julia (790ms) is slower than expected for a JIT-compiled language, possibly due to compilation overhead

**Row-by-row processing pattern:**
All implementations follow the same core algorithm: process rows sequentially from start to bottom, tracking beam state per row. This uniform approach across languages demonstrates that the sequential processing is optimal for this problem structure.

**Beam merging in Part 2:**
The Part 2 requirement to count merged beams (multiple beams reaching the same cell) is elegantly handled by using a count matrix rather than tracking individual beam paths. All implementations converge on this approach, demonstrating the optimal solution strategy.

## Notes

[To be filled by dev personally]
