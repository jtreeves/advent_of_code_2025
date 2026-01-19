# Analysis for Day 04

## Overall Approach

The problem requires processing a 2D grid where each cell is either empty (`.`) or contains a roll of paper (`@`). For Part 1, we count cells with `@` that have fewer than 4 neighbors (out of 8 possible adjacent positions) that are also `@`. For Part 2, we iteratively remove all accessible rolls (those with < 4 neighbors that are `@`) until no more can be removed, counting the total number removed across all rounds.

The core algorithm involves neighbor counting using 8-directional checks (N, S, E, W, and 4 diagonals) with boundary checking. Part 1 is a straightforward single-pass scan. Part 2 requires iteration: each round identifies all accessible rolls, removes them all at once, then repeats until no changes occur in a round.

**Clues**

- The problem states "fewer than four rolls of paper in the eight adjacent positions" - explicitly indicating we need 8-directional neighbor counting with a threshold of 4
- Part 2's phrase "once the forklifts access and remove some rolls, other rolls might become accessible" indicates iterative removal is required
- The requirement to "keep removing accessible rolls until no more can be removed" confirms we need a loop that continues until convergence

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(rows × cols) where rows and cols are grid dimensions. We must examine each cell and count its 8 neighbors, which is constant time per cell.
  - Part 2: O(rows × cols × k) where k is the number of removal rounds. Each round scans the entire grid to identify accessible rolls, and rounds continue until convergence (typically small, but worst case could approach rows×cols if removing one at a time).
- **Space Complexity**: O(rows × cols) - we need to store the grid. For Part 2, we also need temporary storage for positions to remove, but this is bounded by O(rows × cols).

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 153   | 257                         |
| Clojure    | 62    | [Not measured]              |
| Elixir     | 66    | [Not measured]              |
| Go         | 105   | 1747                        |
| Haskell    | 55    | [Not measured]              |
| Java       | 92    | 994                         |
| Julia      | 71    | 9216                        |
| Perl       | 78    | 1313                        |
| Python     | 60    | 29290                       |
| Ruby       | 65    | 910                         |
| Rust       | 88    | 3541                        |
| TypeScript | 78    | 1055                        |

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

### Perl
Perl filters out empty lines using `grep { $_ =~ /\S/ } split /\n/` before processing. Part 1 works directly with string arrays (`@lines`), accessing characters with `substr($lines[$i], $j, 1)`. For Part 2, Perl converts the grid to a 2D array of arrays using `@grid = map { [split //, $_] } @lines`, where `split //` splits a string into individual characters. The 8-directional neighbor counting function receives a reference to the grid: `count_neighbors(\@lines, ...)` and accesses with `$grid->[$ni]`. Character access uses `substr($grid->[$ni], $nj, 1)` when working with string references. The 8-directional neighbor check uses nested loops `for my $di (-1..1)` with explicit bounds checking. For Part 2, the iterative removal uses a `while (1)` loop with `last unless @to_remove` for early termination (Perl's `last` is equivalent to `break`). Positions to remove are stored as array references: `push @to_remove, [$i, $j]`, which are later dereferenced with `my ($i, $j) = @$pos`. The grid is modified in-place with direct assignment: `$grid[$i][$j] = '.'`. Perl rebuilds string arrays when needed: `my @grid_str = map { join('', @$_) } @grid` to convert back to strings for the neighbor counting function, demonstrating Perl's flexible conversion between string and array representations.

### Python
[How the Python implementation differs (e.g., type hints, list comprehensions)]

### Ruby
[How the Ruby implementation differs (e.g., dynamic typing, blocks)]

### Rust
[How the Rust implementation differs (e.g., ownership, borrowing, pattern matching)]

### TypeScript
[How the TypeScript implementation differs (e.g., strong typing, async/await)]

## Key Observations

[Notable differences, trade-offs, or interesting aspects across implementations, with emphasis on how language paradigms necessitating such differences]

## Notes

Still annoyingly kludgy.
