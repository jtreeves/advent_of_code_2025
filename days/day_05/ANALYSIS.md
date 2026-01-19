# Analysis for Day 05

## Overall Approach

The problem requires processing ingredient ID ranges and specific IDs to check freshness. For Part 1, we check each specific ID against all ranges to see if it falls within any range (inclusive on both ends). For Part 2, we merge overlapping and adjacent ranges, then count the total number of unique IDs covered by all merged ranges.

The core algorithm involves: (1) parsing input split by a blank line into ranges and IDs, (2) for Part 1, checking each ID against all ranges with a nested loop, (3) for Part 2, sorting ranges by start value, then merging overlapping/adjacent ranges in a single pass, and (4) summing the lengths of merged ranges to get total coverage.

**Clues**

- The problem states ranges are "inclusive on both ends" (e.g., `3-5` means 3, 4, 5 are all fresh), indicating we need inclusive range checks: `start <= ID <= end`
- Part 2's requirement to find "total number of unique IDs covered" combined with ranges that "may overlap heavily" indicates we need to merge overlapping ranges rather than naively summing lengths
- The phrase "overlapping ranges must be merged" confirms the need for interval merging algorithm
- The note about ranges being "much larger" than can be handled by expanding into memory suggests we need interval-based logic, not set-based expansion

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n × m) where n = number of IDs to check, m = number of ranges. For each ID, we check all ranges until finding a match (early break on first match).
  - Part 2: O(m log m) - dominated by sorting ranges. The merge pass is O(m) after sorting. Total is O(m log m).
- **Space Complexity**: O(m + n) - storage for ranges (tuples/pairs) and IDs (integers). The merged ranges list is at most O(m) in worst case (no overlaps).

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 184   | 202                         |
| Clojure    | 46    | 516                         |
| Elixir     | 65    | 364                         |
| Go         | 106   | 396                         |
| Haskell    | 56    | 352                         |
| Java       | 86    | 73                          |
| Julia      | 71    | 511                         |
| Perl       | 86    | 83                          |
| Python     | 62    | 55                          |
| Ruby       | 68    | 105                         |
| Rust       | 76    | 267                         |
| TypeScript | 79    | 1412                        |

### C
The C implementation uses manual memory management with dynamic allocation (`malloc`, `realloc`) for ranges and IDs arrays. String parsing manually splits lines by iterating through content and replacing newlines with null terminators to preserve blank lines (since `strtok` skips them). Uses `qsort` with a custom comparator for range sorting. Range merging modifies ranges in-place using pointers (`&merged[merged_count - 1]`). Uses `strtoll` for parsing large integers to `long long` (64-bit). Manual cleanup with `free()` for all allocated memory.

### Clojure
Clojure uses immutable data structures and a pure functional approach. The merge logic uses `loop/recur` for tail-recursive range merging, building immutable vectors. Range parsing uses threading macros (`->>`) with `map` and `filter`. Part 1 uses `count` with `filter` and `some` to check ID membership. The `merge-ranges` function is extracted as a separate pure function. All transformations are lazy/eager depending on operations, with no mutable state.

### Elixir
Elixir uses Enum pipelines extensively for processing ranges and IDs. Pattern matching extracts ranges with tuple destructuring `{start, e}`. The merge operation uses `Enum.reduce` with an accumulator `{acc, last}` pattern, which is somewhat complex due to needing to track both the accumulator list and the last range. Uses `String.split/2` for parsing and `String.to_integer/1` for conversion. The `Code.load_file` warning indicates it should use `Code.require_file` instead (deprecated API).

### Go
Go uses simple imperative style with `Range` struct and slices. Uses `sort.Slice` with a closure comparator for sorting. Range merging modifies slices in-place using pointer references (`&merged[len(merged)-1]`). Parsing uses `strconv.ParseInt` with 64-bit integers. The implementation is straightforward with explicit type declarations and error handling via blank identifier `_` to ignore parse errors. Uses `utils.ReadInputRaw` for file I/O.

### Haskell
Haskell uses pure functional recursion with `mergeRanges` implemented as a recursive function pattern matching on list structure. Range parsing uses `words` after mapping `-` to spaces, then `read` for integer conversion. Part 1 uses `length` with `filter` and `any` to count matching IDs. The merge function recursively processes ranges, building a new list. Uses `Integer` type for arbitrary-precision integers. Pattern matching on function heads handles base cases elegantly.

### Java
Java uses `List<long[]>` for ranges (arrays of two longs) and `List<Long>` for IDs. Sorting uses `Collections.sort` with a lambda comparator `(a, b) -> Long.compare(a[0], b[0])`. Range merging modifies arrays in-place by directly updating `last[1] = Math.max(...)`. Uses `Long.parseLong` for 64-bit integer parsing. The implementation is straightforward OOP style with ArrayList collections and standard library methods.

### Julia
Julia uses 1-based indexing for arrays (`1:blank_idx`). Range tuples are stored in arrays. The merge logic modifies arrays in-place (`last[2] = max(...)`). Uses `parse(Int64, ...)` for integer conversion. The implementation uses Julia's multiple dispatch but in a simple imperative style here. `sum` with generator expression calculates total coverage. Note the unusual conversion `([x[1], x[2]] for x in merged)` to handle array mutation in the sum calculation.

### Perl
Perl uses anonymous array references `[$start, $end]` for range storage, building `@ranges` as an array of references. Sorting uses a custom comparator with the `<=>` operator: `sort { $a->[0] <=> $b->[0] } @ranges`. The merge logic copies the first range: `push @merged, [@{$ranges[0]}]` using array slice `[@{...}]` to create a new array reference. Range merging dereferences the last merged range: `my ($last_start, $last_end) = @{$merged[-1]}` and modifies in-place using negative indexing: `$merged[-1]->[1] = $last_end > $end ? $last_end : $end`. The ternary operator handles the maximum calculation. Array references are dereferenced with `my ($start, $end) = @$range_ref`. The implementation uses numeric comparison `<=>` for sorting and `>=`/`<=` for range membership checks. For Part 1, nested loops check each ID against all ranges with early termination using `last`. Total coverage is calculated by summing `$end - $start + 1` for each merged range.

### Python
Python uses list of tuples `(start, end)` for ranges. Sorting uses `sorted(ranges, key=lambda x: x[0])`. Merge logic uses a list of lists initially (`merged.append(list(ranges_sorted[0]))`) then modifies in-place (`merged[-1][1] = max(...)`). Uses `map(int, ...)` for parsing and list comprehensions for summing lengths. The implementation is clean and readable with Python's standard idioms.

### Ruby
Ruby uses arrays of arrays `[start, end]` for ranges. Sorting uses `sort_by { |r| r[0] }`. Merge modifies arrays in-place (`last[1] = [last[1], curr_end].max`). Uses `sum` with a block `{ |start, e| e - start + 1 }` for total calculation. The implementation is concise with Ruby's expressive collection methods and array manipulation.

### Rust
Rust uses `Vec<(i64, i64)>` (vector of tuples) for ranges. Sorting uses `sort_by_key(|r| r.0)`. Merge uses `last_mut().unwrap()` to get a mutable reference to the last element, then modifies the tuple field directly (`last.1 = last.1.max(*end)`). Uses `parse::<i64>()` for integer parsing. The implementation leverages Rust's ownership system with references (`&ids`, `&ranges`) and explicit mutability (`mut part1_count`).

### TypeScript
TypeScript uses `Array<[number, number]>` with explicit type annotations. Sorting uses `sort((a, b) => a[0] - b[0])` with numeric comparison. Merge modifies tuples in-place (`last[1] = Math.max(...)`). Uses `parseInt(..., 10)` for base-10 integer parsing. The implementation is straightforward JavaScript/TypeScript style with standard array methods and explicit typing for clarity.

## Key Observations

The most significant difference across implementations is how languages handle mutability in range merging:

- **Mutable merge (C, Go, Java, TypeScript, Ruby, Julia)**: These languages modify the last range in-place during merging, which is efficient and straightforward. C uses pointers, Go uses slice references, Java/TypeScript modify array elements directly.

- **Immutable merge (Clojure, Haskell)**: These languages build new data structures during merging. Clojure uses `loop/recur` to construct new vectors. Haskell uses pure recursion. This is more functional but potentially less memory-efficient.

- **Hybrid approach (Python, Elixir)**: Python starts with a list of lists (mutable) then modifies. Elixir's approach is more complex due to accumulator pattern.

**Performance observations:**
- Python (55ms) and Java (73ms) are fastest among measured languages, likely due to JIT compilation (Java) and optimized Python runtime
- TypeScript (1412ms) is significantly slower, likely due to Node.js interpretation overhead
- Compiled languages (C, Rust, Haskell) have similar performance (202-352ms) when including compilation time
- Functional languages (Clojure 516ms, Haskell 352ms) perform reasonably well despite immutability

**Range merging pattern:**
All implementations use the same algorithm: sort by start, then single-pass merge checking `current.start <= last.end + 1`. The uniformity across languages demonstrates that this is the optimal approach for interval merging.

**Off-by-one handling:**
All implementations correctly handle inclusive ranges with `end - start + 1` for length calculation. The adjacency check `<= last.end + 1` correctly handles merging adjacent ranges (e.g., `5-10` and `11-15` merge to `5-15`).

## Notes

This was the first day where I properly used the slash command. It isn't just an issue of typing in `/day NN`; when you type in `/day`, you need to select the command option that pops up, otherwise it won't actually use the proper command. It worked much better this time.
