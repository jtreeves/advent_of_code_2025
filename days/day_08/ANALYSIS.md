# Analysis for Day 08

## Overall Approach

The problem requires connecting 3D junction boxes into circuits using a greedy approach (closest pairs first) and tracking connected components using Union-Find (Disjoint Set Union) data structure. For Part 1, we connect the first 1000 closest pairs and compute the product of sizes of the three largest circuits. For Part 2, we continue connecting pairs until all boxes are in one circuit, then compute the product of X coordinates of the final connection pair.

**Clues**

- The problem mentions "closest pairs first" - indicating we need to compute all pairwise distances and sort them
- "Connect" and "circuit" terminology suggests using a Union-Find data structure for efficient component tracking
- Part 1 specifies "first 1000" connections - we need to process exactly 1000 pairs, even if they don't change circuit structure
- Part 2 mentions "final connection" - we need to track which pair completes the single circuit

## Complexity Analysis

- **Time Complexity**: O(n² log n) where n = number of boxes - dominated by computing all pairwise distances (O(n²)) and sorting them (O(n² log n²) = O(n² log n))
- **Space Complexity**: O(n²) for storing all pairs and distances, plus O(n) for Union-Find data structures

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 205   | 324                         |
| Clojure    | 115   | 6935                        |
| Elixir     | 158   | 2630                        |
| Go         | 166   | 616                         |
| Haskell    | 107   | 2732                        |
| Java       | 151   | 355                         |
| Julia      | 134   | 1019                        |
| Perl       | 133   | 1844                        |
| Python     | 114   | 696                         |
| Ruby       | 113   | 574                         |
| Rust       | 149   | 803                         |
| TypeScript | 132   | 2202                        |

### C
- **Manual memory management**: Uses `malloc`/`realloc`/`free` for dynamic arrays and Union-Find structures
- **Structs for data**: `Coord` and `Pair` structs for type safety without classes
- **Function pointers**: Uses `qsort` with `compare_pairs` and `compare_long_long` comparators
- **Explicit resource management**: `uf_free()` function to clean up Union-Find memory
- **Performance**: Fastest execution (324 ms) due to compiled code and direct memory access

### Clojure
- **Immutable data structures**: Uses `defrecord` for `Coord` and `Pair` types
- **Atoms for mutable state**: Union-Find uses atoms (`@parent`, `@size`, `@component-count`) to simulate mutable state within immutable paradigm
- **Functional transformations**: `sort-by :dist-sq` for sorting, `assoc` for map updates
- **Threading**: Natural functional pipeline without explicit threading macros for this algorithm
- **Performance**: Slowest execution (6935 ms) due to JVM startup and immutable overhead

### Elixir
- **Pattern matching**: Uses `%Coord{}` and `%Pair{}` structs with pattern matching
- **Enum pipelines**: Extensive use of `|>` pipe operator; `Enum.flat_map` for nested comprehensions
- **Immutable updates**: Union-Find returns new state instead of mutating (challenging for this algorithm)
- **Range handling**: Careful handling of descending ranges (`1..0`) to avoid warnings
- **Performance**: Higher overhead (2630 ms) from functional paradigm and BEAM VM

### Go
- **Structs and methods**: `UnionFind` struct with methods; `Coord` and `Pair` as struct types
- **Slice operations**: Uses slices for dynamic arrays, `sort.Slice` for sorting
- **Explicit error handling**: `strconv.Atoi` returns errors (handled with `_`)
- **Garbage collection**: Automatic memory management with GC overhead
- **Performance**: Good balance (616 ms) - compiled but with GC overhead

### Haskell
- **Pure functional**: Union-Find operations return new `UnionFind` instead of mutating
- **Pattern matching**: Extensive use for destructuring and case analysis
- **List comprehensions**: Used for generating pairs: `[(i, j, dist) | i <- [0..n-1], j <- [i+1..n-1]]`
- **Map operations**: `Map.fromList` for component sizes, `Map.elems` for values
- **Immutability challenge**: Union-Find mutation simulated through state threading
- **Performance**: Higher overhead (2732 ms) from pure functional paradigm and lazy evaluation

### Java
- **Object-oriented design**: `UnionFind` class with methods; inner `Coord` and `Pair` classes
- **Collections**: `ArrayList`, `HashMap`, `List.sort()` with `Long.compare`
- **Long arithmetic**: Uses `long` to avoid integer overflow for distances and products
- **JVM optimization**: HotSpot JIT compiler provides good performance after warmup
- **Performance**: Excellent (355 ms) - JVM JIT optimization despite object overhead

### Julia
- **Mutable structs**: `UnionFind` as mutable struct; `Coord` as immutable struct
- **1-indexed arrays**: Arrays start at 1, requiring `(i, j)` tuple indexing from `1:n` instead of `0:n-1`
- **Type annotations**: Optional but useful: `UnionFind(n::Int)`, `Coord(x::Int, y::Int, z::Int)`
- **Array operations**: Direct array indexing and mutation; `sort!` for in-place sorting
- **Performance**: Good (1019 ms) - JIT compiled but with runtime compilation overhead

### Perl
Perl implements Union-Find using package-based object-oriented programming. The `UnionFind` package uses a blessed hash reference `$self` to store state: `parent => [0..$n-1]`, `size => [(1) x $n]` using repetition operator, and `component_count`. Methods access hash keys with `$self->{parent}[$x]` and modify in-place. The `bless $self, $class` creates an object. Path compression is implemented recursively: `$self->{parent}[$x] = $self->find($self->{parent}[$x])`. Pairs are stored as array references `[$i, $j, $dist_sq]` and sorted with `sort { $a->[2] <=> $b->[2] }`. Component sizes are tracked in a hash `%component_sizes` using roots as keys, and values are extracted with `values %component_sizes` then sorted descending: `sort { $b <=> $a }`. Array access uses conditional: `@sizes >= 3 ? $sizes[0] * $sizes[1] * $sizes[2] : 0`. The implementation uses `undef` to track optional values and `defined()` to check. Early termination uses `last if` conditions.

### Python
- **Class-based Union-Find**: `UnionFind` class with methods using `self`
- **Type hints**: Function signatures like `def solve(input_data: str) -> tuple[str, str]`
- **List comprehensions**: Used for coordinate parsing and pair generation
- **Tuple unpacking**: `x, y, z = int(parts[0]), int(parts[1]), int(parts[2])`
- **Dynamic typing**: Flexible but with runtime overhead
- **Performance**: Moderate (696 ms) - interpreted with significant overhead

### Ruby
- **Struct types**: `Coord = Struct.new(:x, :y, :z)` for lightweight classes
- **Method chaining**: `pairs.sort_by! { |p| p.dist_sq }` for in-place sorting
- **Blocks and procs**: Blocks used extensively: `(0...n).each do |i|`
- **Mutable state**: Union-Find naturally uses instance variables (`@parent`, `@size`)
- **Readable syntax**: `component_sizes[root] = uf1.size[root]` very readable
- **Performance**: Good (574 ms) - interpreted but optimized Ruby VM

### Rust
- **Ownership and borrowing**: Union-Find methods take `&mut self` for mutation
- **Pattern matching**: `match`, `if let`, destructuring used throughout
- **Iterator chains**: `pairs.iter()`, `filter_map()`, `collect()` for functional style
- **Type safety**: `usize` for indices, `i64` for coordinates and distances
- **Zero-cost abstractions**: Compiled code is very efficient
- **Performance**: Good (803 ms) - compiled but includes compilation time in timing

### TypeScript
- **Class-based design**: `UnionFind` class with private/public members
- **Strong typing**: `type Coord = [number, number, number]` for tuple types
- **Array methods**: `sort((a, b) => a[2] - b[2])` for sorting, `filter()`, `map()`
- **Type assertions**: Occasional `as` casts for array access
- **Modern JS features**: Arrow functions, destructuring, spread operator
- **Performance**: Moderate (2202 ms) - Node.js V8 engine with interpretation overhead

## Key Observations

1. **Union-Find Implementation Variants**:
   - **Mutable languages** (C, Java, Perl, Ruby, Rust, Julia, Python, Go): Direct mutation of parent/size arrays - most natural and efficient
   - **Immutable languages** (Clojure, Elixir, Haskell): Use atoms, return new state, or state threading - more complex but maintains immutability

2. **Performance Patterns**:
   - **Fastest**: Compiled languages (C: 324ms, Java: 355ms) - direct machine code or JIT optimized
   - **Slowest**: Interpreted functional languages (Clojure: 6935ms, Elixir: 2630ms) - VM overhead and immutable data structure copying
   - **Mid-range**: Interpreted imperative (Python: 696ms, Ruby: 574ms) - better than functional but slower than compiled

3. **Language-Specific Challenges**:
   - **Julia**: 1-indexed arrays require converting to 0-indexed logic mentally
   - **Elixir**: Descending ranges (`1..0`) cause warnings, requiring conditional logic
   - **Haskell**: Union-Find mutation requires threading state through function calls
   - **TypeScript**: Const reassignment errors required using `let` instead of `const` in union method

4. **Algorithm Consistency**:
   - All implementations use the same core algorithm: Union-Find with path compression and union by size
   - All use squared distances to avoid floating-point precision issues
   - All sort pairs before processing
   - Differences are purely in language idioms and data structures

5. **Code Complexity**:
   - **Simplest**: Python and Ruby - readable, concise
   - **Most verbose**: C - manual memory management requires more boilerplate
   - **Most elegant**: Clojure and Haskell - functional style is clean but slower

6. **Memory Management**:
   - **Explicit**: C requires `malloc`/`free` calls
   - **Automatic**: All other languages use GC (except Rust which uses ownership)
   - **Trade-off**: C's manual management is fastest but error-prone; GC languages are safer but slower

## Notes

[To be filled by dev personally]
