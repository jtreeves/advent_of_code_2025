# Analysis for Day 11

## Overall Approach

The problem requires counting paths in a directed graph. For Part 1, we count all paths from `you` to `out` in a directed graph. For Part 2, we count all paths from `svr` to `out` that include both `fft` and `dac` somewhere along the path.

The core algorithm uses **Depth-First Search (DFS) with memoization** to efficiently count paths without enumerating all paths explicitly. Part 1 uses simple node-based memoization, while Part 2 tracks state (which required nodes have been visited) as part of the memoization key.

**Clues**

- The problem states "data flows only forward" and "never backwards" - indicating a directed graph where cycles are not part of valid paths
- Part 1 asks for "every path from `you` to `out`" - suggesting we need to count all possible paths, not just find one
- Part 2 requires paths that "include both `fft` and `dac` somewhere along the path" - indicating we need state tracking during path traversal
- The note that "every valid path hits `fft` before `dac`" suggests the graph structure, but we still need to check both flags

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(V + E) where V = number of devices (vertices), E = number of edges. With memoization, each node is processed once, and each edge is traversed once.
  - Part 2: O(V + E) - same as Part 1. Although the memoization key includes two boolean flags (4 possible states per node), the effective complexity remains O(V + E) since memoization ensures each (node, state) combination is computed only once.

- **Space Complexity**: 
  - Graph storage: O(V + E) for adjacency list representation
  - Memoization: O(V) for Part 1 (one value per node), O(V) for Part 2 (at most 4 states per node, but typically fewer)
  - Recursion stack: O(V) in worst case (longest path in graph)
  - Total: O(V + E)

## Solutions

### Part 1
| Language   | Initial Solution      |
|------------|-----------------------|
| C          | 497                   |
| Clojure    | 497                   |
| Elixir     | 497                   |
| Go         | 497                   |
| Haskell    | 497                   |
| Java       | 497                   |
| Julia      | 497                   |
| Kotlin     | 497                   |
| Python     | 497                   |
| Ruby       | 497                   |
| Rust       | 497                   |
| TypeScript | 497                   |

### Part 2
| Language   | Initial Solution      |
|------------|-----------------------|
| C          | 358564784931864       |
| Clojure    | 358564784931864       |
| Elixir     | 358564784931864       |
| Go         | 358564784931864       |
| Haskell    | 358564784931864       |
| Java       | 358564784931864       |
| Julia      | 358564784931864       |
| Kotlin     | 358564784931864       |
| Python     | 358564784931864       |
| Ruby       | 358564784931864       |
| Rust       | 358564784931864       |
| TypeScript | 358564784931864       |

## Performance

### Part 1
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 330                 |
| Clojure    | 889                 |
| Elixir     | 873                 |
| Go         | 587                 |
| Haskell    | 529                 |
| Java       | 121                 |
| Julia      | 705                 |
| Kotlin     | 160                 |
| Python     | 77                  |
| Ruby       | 150                 |
| Rust       | 565                 |
| TypeScript | 2087                |

### Part 2
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 330                 |
| Clojure    | 889                 |
| Elixir     | 873                 |
| Go         | 587                 |
| Haskell    | 529                 |
| Java       | 121                 |
| Julia      | 705                 |
| Kotlin     | 160                 |
| Python     | 77                  |
| Ruby       | 150                 |
| Rust       | 565                 |
| TypeScript | 2087                |

## Implementation Differences

### C
The C implementation uses manual memory management with custom hash table implementations for both the graph and memoization. The graph is represented as a hash table of linked lists (separate chaining), where each bucket contains nodes with their adjacency lists. Memoization for Part 1 uses a string-keyed hash table, while Part 2 uses a struct-based key `(node, visited_fft, visited_dac)` with custom hash and equality functions. Memory is manually allocated with `malloc`/`realloc` and must be carefully managed to avoid leaks. The parsing uses `strtok` carefully to avoid nested calls.

### Clojure
Clojure uses immutable data structures throughout. The graph is a persistent map from device names to lists of outputs. Memoization uses atoms (mutable references to immutable values) to cache results during recursion. The recursive functions use `letfn` for local recursive definitions. Part 1 memoizes by node string, while Part 2 uses a vector `[node visited-fft visited-dac]` as the memoization key. The functional style avoids mutation except for the memo atoms, which are thread-safe.

### Elixir
Elixir uses immutable maps for both the graph and memoization. The recursive functions return tuples `{result, updated_memo}` to thread memo state through recursion. Pattern matching is used extensively for base cases and conditional logic. The graph is built using `Enum.reduce`, and path counting uses recursion with pattern matching on the memo map using `Map.fetch`. Part 2 memoization uses tuples `{node, visited_fft, visited_dac}` as keys in the memo map.

### Go
Go uses `map[string][]string` for the graph representation. Memoization uses maps directly, with `map[string]int` for Part 1 and a custom `MemoKey` struct (with fields `node`, `visitedFFT`, `visitedDAC`) for Part 2. Recursive closures capture the graph and memo map. The `MemoKey` struct is used as a map key (Go allows structs as map keys if they're comparable). Error handling is minimal since the problem guarantees valid input.

### Haskell
Haskell uses `Data.Map.Strict` for both the graph and memoization. The recursive functions take the memo as a parameter and return `(result, updated_memo)` tuples, threading state through recursion. Part 1 uses `Map String Integer`, while Part 2 uses `Map (String, Bool, Bool) Integer` (tuple keys). The functions are pure and use pattern matching for base cases. The graph building uses `foldl'` for strict left-fold accumulation, and path counting uses `foldl'` to combine results from neighbors.

### Java
Java uses `Map<String, List<String>>` for the graph. Memoization uses `Map<String, Long>` for Part 1 and `Map<MemoKey, Long>` for Part 2, where `MemoKey` is a static inner class with `equals()` and `hashCode()` properly implemented for use as a map key. The recursive methods are private static methods. Long integers are used for potentially large path counts. The `MemoKey` class implements proper `equals()` and `hashCode()` for correct map key behavior.

### Julia
Julia uses `Dict{String, Vector{String}}` for the graph. Part 1 memoization uses `Dict{String, Int64}`, while Part 2 defines a `MemoKey` struct at module level (structs must be at top-level in Julia) with fields `node::String`, `visited_fft::Bool`, `visited_dac::Bool`. The recursive functions use `get()` with default values for safe dictionary access. Julia's dynamic typing allows concise code while maintaining type stability through type annotations.

### Kotlin
Kotlin uses `MutableMap<String, List<String>>` for the graph. Memoization uses `MutableMap<String, Long>` for Part 1 and `MutableMap<MemoKey, Long>` for Part 2, where `MemoKey` is a data class (automatically provides `equals()` and `hashCode()`). Recursive functions are defined with `fun` inside the `solve` function as local functions. Null safety is handled with `?:` operator and `getOrDefault`. Kotlin's data classes make the memoization key implementation concise.

### Python
Python uses `dict[str, list[str]]` with type hints. Memoization uses dictionaries directly: `dict[str, int]` for Part 1 and `dict[tuple[str, bool, bool], int]` for Part 2, using tuples as keys (Python tuples are hashable). Recursive functions are defined as nested functions inside `solve`, capturing the graph in their closure. Python's dynamic typing and dictionary flexibility make the memoization straightforward.

### Ruby
Ruby uses a Hash for the graph. Memoization uses a Hash with string keys for Part 1 and arrays `[node, visited_fft, visited_dac]` as keys for Part 2 (Ruby arrays are hashable). Recursive functions are defined as methods using `def` inside a module or as local functions using `def`. The code uses Ruby's expressive array/hash syntax with `||` for defaults and `||=` for memoization checks. The functional style uses `each` blocks.

### Rust
Rust uses `HashMap<String, Vec<String>>` for the graph. Memoization uses `HashMap<String, i64>` for Part 1 and `HashMap<MemoKey, i64>` for Part 2, where `MemoKey` is a struct with derived `Hash` and `Eq` traits. The recursive functions take `&HashMap` (borrowed references) for the graph and `&mut HashMap` for mutable memo. Ownership and borrowing are carefully managed - the graph is borrowed immutably while memo is borrowed mutably. Rust's type system ensures memory safety without garbage collection.

### TypeScript
TypeScript uses `Map<string, string[]>` for the graph with type annotations. Memoization uses `Map<string, number>` for Part 1 and `Map<string, number>` for Part 2, where the key is a stringified version of `{node, visitedFFT, visitedDAC}` using `memoKeyToString()` helper function (since Maps require string keys for custom objects). The recursive functions are defined as nested functions with proper type annotations. TypeScript provides type safety while maintaining JavaScript's flexibility.

## Key Observations

### Memoization Patterns
- **Imperative languages (C, Go, Java, Kotlin)**: Use mutable maps/dictionaries that are passed by reference and modified in-place during recursion. This is the most straightforward approach.
- **Functional languages (Haskell, Elixir)**: Thread memo state through recursion by returning `(result, updated_memo)` tuples. This maintains purity but requires careful state threading.
- **Hybrid languages (Python, Ruby, TypeScript, Julia)**: Use mutable dictionaries/maps but in a more functional style, taking advantage of both paradigms.

### Memoization Keys for Part 2
- **Struct/Class-based keys (Go, Java, Kotlin, Rust, Haskell, Julia)**: Define a dedicated type for the memo key with proper equality/hash methods. This is type-safe and clear.
- **Tuple-based keys (Python, Elixir, Haskell)**: Use tuples directly as map keys. Simple and expressive.
- **Array/Vector-based keys (Ruby, Clojure)**: Use arrays/vectors as keys. Works well in these languages where arrays are hashable.
- **String-based keys (TypeScript)**: Serialize the key to a string due to TypeScript's Map limitations with object keys. Less type-safe but pragmatic.

### Graph Representation
All implementations use an adjacency list (map/dictionary from node to list of neighbors). This is the natural choice for sparse directed graphs and matches the input format directly.

### Performance Insights
- **Python (77ms)**: Fastest execution, demonstrating that Python's dictionary operations are highly optimized for this use case.
- **Java (121ms)**: Excellent performance despite being a compiled language, likely due to JVM optimizations and efficient HashMap implementation.
- **Ruby (150ms)**: Competitive with compiled languages, showing Ruby's hash operations are well-optimized.
- **TypeScript (2087ms)**: Slowest, likely due to ts-node overhead and less optimized Map operations compared to native JavaScript engines.
- **Compiled languages (C, Rust, Go, Haskell)**: Range from 330ms to 587ms, with C being fastest among compiled languages. The overhead likely comes from compilation time being included in measurements.

### Language Paradigm Reflections
- **Functional purity (Haskell, Elixir)**: Require explicit state threading, making code more verbose but easier to reason about.
- **Imperative simplicity (C, Go, Java)**: Direct mutable state makes memoization straightforward and efficient.
- **Dynamic flexibility (Python, Ruby, TypeScript)**: Dictionary/tuple flexibility enables concise code without sacrificing readability.
- **Type safety (Rust, Haskell, TypeScript)**: Compile-time guarantees catch errors early, at the cost of more verbose type annotations.

## Notes

All 12 implementations converged to the same answers for both parts, demonstrating that the DFS with memoization approach is language-agnostic. The memoization strategy is critical - without it, the problem would be intractable for large graphs. Part 2's state tracking (visited flags) is elegantly handled through memoization keys, avoiding exponential explosion in path enumeration.
