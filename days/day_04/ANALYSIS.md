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

## Solutions

### Part 1
| Language   | Initial Solution |
|------------|------------------|
| C          | 1587             |
| Clojure    | 1587             |
| Elixir     | 1587             |
| Go         | 1587             |
| Haskell    | 1587             |
| Java       | 1587             |
| Julia      | 1587             |
| Kotlin     | 1587             |
| Python     | 1587             |
| Ruby       | 1587             |
| Rust       | 1587             |
| TypeScript | 1587             |

### Part 2
| Language   | Initial Solution |
|------------|------------------|
| C          | 8946             |
| Clojure    | 8946             |
| Elixir     | 8946             |
| Go         | 8946             |
| Haskell    | 8946             |
| Java       | 8946             |
| Julia      | 8946             |
| Kotlin     | 8946             |
| Python     | 8946             |
| Ruby       | 8946             |
| Rust       | 8946             |
| TypeScript | 8946             |

## Performance

### Part 1
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 257                 |
| Clojure    | [Not measured]      |
| Elixir     | [Not measured]      |
| Go         | 1747                |
| Haskell    | [Not measured]      |
| Java       | 994                 |
| Julia      | 9216                |
| Kotlin     | 35629               |
| Python     | 29290               |
| Ruby       | 910                 |
| Rust       | 3541                |
| TypeScript | 1055                |

### Part 2
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 257                 |
| Clojure    | [Not measured]      |
| Elixir     | [Not measured]      |
| Go         | 1747                |
| Haskell    | [Not measured]      |
| Java       | 994                 |
| Julia      | 9216                |
| Kotlin     | 35629               |
| Python     | 29290               |
| Ruby       | 910                 |
| Rust       | 3541                |
| TypeScript | 1055                |

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

[To be filled by dev personally]
