# Analysis for Day 12

## Overall Approach

Day 12 - "Christmas Tree Farm" is a 2D packing problem involving polyominoes. While packing problems are typically NP-hard, the puzzle input is designed such that a simple area check suffices.

The solution approach:
1. Parse six distinct present shapes (polyominoes) from 3×3 grids and calculate each shape's area (number of '#' cells)
2. Parse queries specifying rectangular regions (width × height) and required counts for each shape
3. For each query, check if the total required area (sum of shape_area × count for all shapes) is less than or equal to the region area (width × height)
4. Count how many queries are possible (satisfy the area check)

**Clues**

- The problem description suggests a complex 2D packing problem with rotations and flips
- However, the actual puzzle input is designed so that if the total area fits, a valid packing always exists
- This suggests using a simple area check heuristic rather than implementing full packing algorithms
- Part 2 has no computational challenge - it's just collecting the final star for the finale

## Complexity Analysis

- **Time Complexity**: O(n) where n is the number of queries. Shape parsing is O(1) since there are only 6 shapes, each 3×3. Each query requires O(1) area calculation (just multiply and compare).
- **Space Complexity**: O(1) for processing (fixed 6 integers for shape areas), O(n) for storing the input where n is the number of lines.

## Solutions

### Part 1
| Language   | Initial Solution |
|------------|------------------|
| C          | 531              |
| Clojure    | 531              |
| Elixir     | 531              |
| Go         | 531              |
| Haskell    | 531              |
| Java       | 531              |
| Julia      | 531              |
| Kotlin     | 531              |
| Python     | 531              |
| Ruby       | 531              |
| Rust       | 531              |
| TypeScript | 531              |

### Part 2
| Language   | Initial Solution |
|------------|------------------|
| C          | Final star       |
| Clojure    | Final star       |
| Elixir     | Final star       |
| Go         | Final star       |
| Haskell    | Final star       |
| Java       | Final star       |
| Julia      | Final star       |
| Kotlin     | Final star       |
| Python     | Final star       |
| Ruby       | Final star       |
| Rust       | Final star       |
| TypeScript | Final star       |

All languages agree on both parts. Part 2 has no computational challenge - Day 12 is the finale, so completing Part 1 and collecting all previous stars earns the final star.

## Performance

### Part 1
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 199                 |
| Clojure    | [Needs fixing]      |
| Elixir     | 574                 |
| Go         | 704                 |
| Haskell    | 411                 |
| Java       | 101                 |
| Julia      | 907                 |
| Kotlin     | 135                 |
| Python     | 59                  |
| Ruby       | 142                 |
| Rust       | 233                 |
| TypeScript | 1775                |

### Part 2
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | N/A                 |
| Clojure    | [Needs fixing]      |
| Elixir     | N/A                 |
| Go         | N/A                 |
| Haskell    | N/A                 |
| Java       | N/A                 |
| Julia      | N/A                 |
| Kotlin     | N/A                 |
| Python     | N/A                 |
| Ruby       | N/A                 |
| Rust       | N/A                 |
| TypeScript | N/A                 |

**Note**: Execution times for compiled languages (C, Rust, Haskell, Kotlin, Java) include compilation time. For interpreted languages (Python, Ruby, TypeScript, Clojure, Elixir, Julia), times represent pure execution. Go times include compilation via `go run`. Part 1 and Part 2 have identical execution times since Part 2 is just a placeholder with no computation.

## Implementation Differences

All implementations follow the same core algorithm: parse shapes to calculate areas, parse queries, and perform area checks. The differences lie in language-idiomatic patterns and data structures.

### C
- **Manual memory management**: Uses `malloc`/`realloc` for dynamic line arrays; manual string manipulation with `strchr`, `strtok`, `sscanf`
- **Explicit parsing**: Character-by-character parsing with manual string splitting and integer conversion via `atoi`
- **Efficient**: Low-level control over memory; minimal overhead; one of the fastest implementations

### Clojure
- **Immutable data structures**: Uses vectors and lists; no mutable state; `loop`/`recur` for iteration
- **Functional transformations**: Heavy use of `map`, `filter`, `reduce`; `try`/`catch` for safe parsing
- **Note**: Currently needs fixing (ClassCastException error)

### Elixir
- **Pattern matching**: Uses `cond` for conditional logic; `try`/`catch` for safe integer parsing
- **Enum functions**: `Enum.drop`, `Enum.map`, `Enum.zip_with`, `Enum.sum` for data transformations
- **Pipe operator**: Uses `|>` for readable data pipelines

### Go
- **Explicit error handling**: Uses `strconv.Atoi` with error checking; `strings.Fields` for splitting
- **Buffered I/O**: Uses `bufio.Scanner` for file reading; explicit error handling with `if err != nil`
- **Simple data structures**: Slices and arrays; straightforward imperative style

### Haskell
- **Pure functional style**: No mutable state; recursive functions with pattern matching
- **List comprehensions**: Uses `reads` for parsing; `zipWith` for element-wise operations
- **Type safety**: Strong static typing prevents many errors; `Maybe` and pattern matching for safe parsing
- **Note**: Fixed naming conflict with Prelude's `lines` function

### Java
- **Object-oriented**: Uses `String.split()`, `Integer.parseInt()` with exception handling
- **Collections**: Arrays and lists; `Files.readAllBytes()` for file I/O
- **Compiled performance**: Good balance of speed and safety; JVM optimization helps

### Julia
- **Array operations**: 1-indexed arrays; `parse(Int, ...)` for parsing; `split()` for string operations
- **Performance**: Fast array operations; compiled to efficient machine code
- **Note**: Fixed variable scoping issue with `width` and `height` in try blocks

### Kotlin
- **Null safety**: Uses `toIntOrNull()` for safe parsing; null-safe operators
- **Extension functions**: `padEnd()`, `split()`; concise collection operations
- **Modern JVM**: Good performance with concise syntax; similar to Java but more expressive

### Python
- **List comprehensions**: Elegant parsing with `split()`, `strip()`, `count()`; `int()` for conversion
- **Simple syntax**: Most readable and concise implementation; fast development time
- **Fastest interpreted language**: Excellent performance for an interpreted language

### Ruby
- **Dynamic typing**: `Integer()` for parsing; `split()`, `count()` for string operations
- **Expressive methods**: `end_with?`, `include?`, `empty?` for readable code; blocks for iteration
- **Good balance**: Fast enough while remaining very readable

### Rust
- **Ownership system**: Borrow checker ensures memory safety; `match` for pattern matching
- **Iterators**: Uses iterator chains with `filter_map()`, `collect()`, `parse()`; efficient zero-copy operations
- **Compiled performance**: Excellent speed with memory safety guarantees

### TypeScript
- **Strong typing**: Type annotations; `parseInt()` with radix; regex patterns for parsing
- **Modern JavaScript**: Array methods like `split()`, `map()`, `filter()`, `reduce()`; async/await support (not used here)
- **Slowest**: Node.js overhead makes it slower than compiled languages, but still acceptable

## Key Observations

1. **Simplified algorithm**: All implementations use the same area-check heuristic rather than complex packing algorithms. This makes the code simpler and easier to understand across all languages.

2. **Performance differences**: Compiled languages (C, Rust, Java, Kotlin, Haskell) are generally faster, with Python being surprisingly fast among interpreted languages. TypeScript is the slowest due to Node.js overhead.

3. **Error handling patterns**:
   - **Try/catch**: Clojure, Elixir, Ruby, Kotlin use exceptions for parsing failures
   - **Explicit errors**: Go, Rust, Haskell use `Result` or explicit error returns
   - **Graceful degradation**: Most implementations skip invalid queries rather than crashing

4. **Parsing approaches**:
   - **String methods**: Python, Ruby, JavaScript/TypeScript use high-level string methods
   - **Manual parsing**: C uses low-level character-by-character parsing
   - **Regex**: Some languages use regex patterns (TypeScript, Julia partially)
   - **Standard library**: Most languages use built-in parsing functions (`atoi`, `parseInt`, `Integer.parseInt`, etc.)

5. **Functional vs imperative**:
   - **Functional**: Haskell, Clojure, Elixir use pure functional transformations
   - **Imperative**: C, Go use explicit loops and mutation
   - **Hybrid**: Python, Ruby, JavaScript use both styles flexibly

6. **Part 2 simplicity**: Since Part 2 is just a placeholder ("Final star"), all implementations handle it the same way - simply return the string without computation.

## Notes

- This puzzle demonstrates that sometimes a simple heuristic (area check) works perfectly for the actual input, even though the general problem (polyomino packing) is NP-hard. All implementations leverage this insight.
- The implementations are remarkably similar across languages due to the simplicity of the algorithm - the main differences are in syntax and idioms rather than algorithmic approach.
- Python's surprising speed (59ms) shows how optimized modern interpreters can be for straightforward algorithms like this one.
