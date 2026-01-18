# Technical Plan: Day 05

## Algorithms and Data Structures

### Algorithm Overview

The problem requires processing ingredient ID ranges and specific IDs to check freshness.

**Part 1**: For each specific ID, check if it falls within any of the given ranges.

**Part 2**: Merge overlapping and adjacent ranges, then count the total number of unique IDs covered by all ranges.

### Key Algorithms

1. **Input Parsing**: 
   - Read input file, split into two sections (ranges and IDs) by blank line
   - Parse ranges: split each line on `-`, convert start and end to integers
   - Parse IDs: convert each line after blank line to integer

2. **Part 1 Implementation**:
   - Store ranges as list of tuples/pairs `(start, end)`
   - For each specific ID to check:
     - Iterate through all ranges
     - Check if ID falls within any range: `start <= ID <= end`
     - If found in any range, increment counter
   - Return count

3. **Part 2 Implementation** (Range Merging):
   - Sort ranges by start value
   - Initialize merged ranges list with first range
   - For each subsequent range:
     - Compare with last merged range
     - If current range overlaps with last merged range (current.start <= last.end + 1), merge them
     - Merge by updating last.end = max(last.end, current.end)
     - If no overlap, add current range as new merged range
   - Sum lengths of all merged ranges: `sum((end - start + 1) for each merged range)`
   - Return total count

### Data Structures

- **Ranges**: 
  - List/array of tuples/pairs `(start, end)` or `{start, end}` structures
  - Or use range objects/structs if language supports them
  
- **IDs to check**: 
  - Simple list/array of integers
  
- **Merged ranges**: 
  - List/array to store merged range tuples

### Why This Approach is Optimal

- **Time Complexity**: 
  - Part 1: O(n * m) where n = number of IDs, m = number of ranges
    - For each ID, check all ranges (can optimize with interval tree, but not necessary for typical inputs)
  - Part 2: O(m log m) for sorting ranges, O(m) for merging = O(m log m) overall
    - Must sort to merge efficiently, then linear pass to merge
  
- **Space Complexity**: 
  - O(m) for storing ranges
  - O(n) for storing IDs to check
  - O(m) for merged ranges (worst case, no overlaps)

- The sorting + linear merge approach is optimal for Part 2. Part 1 could be optimized with interval tree, but O(n*m) is acceptable for typical inputs.

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n * m) where n = number of IDs to check, m = number of ranges
  - Part 2: O(m log m) - dominated by sorting ranges
- **Space Complexity**: O(m + n) - storage for ranges and IDs

## Approach

1. **Parse Input**: 
   - Read file, split on blank line
   - Parse first section: ranges (format `start-end`)
   - Parse second section: IDs (one per line)

2. **Part 1**:
   - For each ID to check:
     - For each range:
       - If `range.start <= ID <= range.end`, mark as fresh and break
     - Increment counter if fresh
   - Return count

3. **Part 2**:
   - Sort ranges by start value
   - Initialize merged list with first range
   - For each remaining range:
     - Compare with last merged range
     - If overlaps (current.start <= last.end + 1):
       - Merge: `last.end = max(last.end, current.end)`
     - Else:
       - Add current range as new merged range
   - Calculate total: `sum(end - start + 1)` for all merged ranges
   - Return total

### Range Overlap Detection

Two ranges `(a1, a2)` and `(b1, b2)` overlap or are adjacent if:
- `b1 <= a2 + 1` (b starts before or at a's end + 1)

When merging:
- New start: `min(a1, b1)` = `a1` (since ranges are sorted)
- New end: `max(a2, b2)`

### Edge Cases

- Single-element ranges: `5-5` has length 1 (end - start + 1 = 1)
- Adjacent ranges: `5-10` and `11-15` should merge to `5-15`
- Contained ranges: `5-20` and `10-15` should merge to `5-20` (10-15 is contained)

## Implementations

### C
- Use arrays or dynamic allocation for ranges and IDs
- Parse strings with `strtok` or manual parsing
- Sort ranges with `qsort` and custom comparator
- Step-by-step:
  1. Read file, split on blank line (find first empty line)
  2. Parse ranges: use `strtok` to split on `-`, convert to `long long`
  3. Parse IDs: convert lines to `long long`
  4. Part 1: nested loops to check each ID against all ranges
  5. Part 2: sort ranges, merge in one pass, sum lengths
  6. Return counts as `long long`

### Clojure
- Use vectors for ranges and IDs
- Use `sort-by` for range sorting
- Immutable merge logic with `reduce`
- Step-by-step:
  1. Read file with `slurp`, split by lines, find blank line index
  2. Parse ranges: `map` over lines, split on `-`, convert with `Long/parseLong`
  3. Parse IDs: `map` over lines after blank, convert to long
  4. Part 1: use `count` with `some` to check ID membership
  5. Part 2: `sort-by first`, `reduce` to merge, `reduce +` for lengths
  6. Return counts

### Elixir
- Use lists or tuples for ranges
- Pattern matching for parsing
- `Enum.sort_by` for sorting
- Step-by-step:
  1. Read file, split by lines, `Enum.split_while` to find blank line
  2. Parse ranges: `Enum.map` with `String.split("-")`, convert with `String.to_integer`
  3. Parse IDs: `Enum.map` with `String.to_integer`
  4. Part 1: `Enum.count` with `Enum.any?` to check membership
  5. Part 2: `Enum.sort_by`, `Enum.reduce` to merge, `Enum.sum` for lengths
  6. Return counts

### Go
- Use slices `[]struct{start, end int64}` or `[]struct{start, end int64}`
- `sort.Slice` for sorting
- Step-by-step:
  1. Read file with `bufio.Scanner`, track blank line
  2. Parse ranges: `strings.Split` on `-`, `strconv.ParseInt`
  3. Parse IDs: `strconv.ParseInt`
  4. Part 1: nested loops
  5. Part 2: `sort.Slice`, merge loop, sum lengths
  6. Return `int64`

### Haskell
- Use lists of tuples `[(Int, Int)]`
- `sortBy` or `sortOn` for sorting
- Immutable merge with `foldl` or recursion
- Step-by-step:
  1. Read file, `lines`, `break null` to split sections
  2. Parse ranges: `map (parseRange . splitOn "-")`, convert with `read`
  3. Parse IDs: `map read`
  4. Part 1: `length . filter (isInRange ranges)` with `any`
  5. Part 2: `sortOn fst`, `foldl merge`, `sum . map rangeLength`
  6. Return `Int` or `Integer`

### Java
- Use `List<Range>` where `Range` is a record/class `{long start, long end}`
- `Collections.sort` or `list.sort(Comparator.comparing(Range::start))`
- Step-by-step:
  1. Read file with `Files.readAllLines`, find blank line index
  2. Parse ranges: `map` with `split("-")`, `Long.parseLong`
  3. Parse IDs: `map` with `Long.parseLong`
  4. Part 1: nested loops or streams
  5. Part 2: sort, merge loop, sum with streams
  6. Return `long`

### Julia
- Use vectors of tuples or `NamedTuple`
- `sort` with `by` keyword
- Step-by-step:
  1. Read file, `split(..., '\n')`, find blank line
  2. Parse ranges: `map` with `split("-")`, `parse(Int64, ...)`
  3. Parse IDs: `parse.(Int64, ...)`
  4. Part 1: `count` with `any`
  5. Part 2: `sort`, merge loop, `sum` lengths
  6. Return `Int64`

### Kotlin
- Use `List<Pair<Long, Long>>` or data class
- `sortedBy` for sorting
- Step-by-step:
  1. Read file with `File.readLines`, find blank line index
  2. Parse ranges: `map` with `split("-")`, `toLong()`
  3. Parse IDs: `map` with `toLong()`
  4. Part 1: `count` with `any`
  5. Part 2: `sortedBy`, `fold` to merge, `sumOf` lengths
  6. Return `Long`

### Python
- Use list of tuples `[(start, end), ...]`
- `sorted` with `key=lambda x: x[0]`
- Step-by-step:
  1. Read file, `split('\n')`, find blank line index
  2. Parse ranges: `map` with `split('-')`, `int`
  3. Parse IDs: `map` with `int`
  4. Part 1: `sum(1 for id in ids if any(start <= id <= end for start, end in ranges))`
  5. Part 2: `sorted(ranges, key=lambda x: x[0])`, merge loop, `sum(end - start + 1)`
  6. Return `int`

### Ruby
- Use arrays of arrays `[[start, end], ...]`
- `sort_by { |r| r[0] }`
- Step-by-step:
  1. Read file, `split("\n")`, find blank line index
  2. Parse ranges: `map` with `split('-')`, `to_i`
  3. Parse IDs: `map` with `to_i`
  4. Part 1: `count { |id| ranges.any? { |start, e| start <= id && id <= e } }`
  5. Part 2: `sort_by(&:first)`, merge loop, `sum { |s, e| e - s + 1 }`
  6. Return `Integer`

### Rust
- Use `Vec<(i64, i64)>` or `Vec<Range>` struct
- `sort_by_key` for sorting
- Step-by-step:
  1. Read file, `lines()`, find blank line
  2. Parse ranges: `map` with `split('-')`, `parse::<i64>()`
  3. Parse IDs: `map` with `parse::<i64>()`
  4. Part 1: `iter().filter(|id| ranges.iter().any(|(s, e)| s <= id && id <= e)).count()`
  5. Part 2: `sort_by_key(|r| r.0)`, merge loop, `sum` lengths
  6. Return `i64` or `usize`

### TypeScript
- Use `Array<[number, number]>` or `Array<{start: number, end: number}>`
- `sort((a, b) => a[0] - b[0])`
- Step-by-step:
  1. Read file, `split('\n')`, find blank line index
  2. Parse ranges: `map` with `split('-')`, `Number.parseInt`
  3. Parse IDs: `map` with `Number.parseInt`
  4. Part 1: `filter(id => ranges.some(([s, e]) => s <= id && id <= e)).length`
  5. Part 2: `sort(...)`, merge loop, `reduce((sum, [s, e]) => sum + e - s + 1, 0)`
  6. Return `number`

## Edge Cases and Considerations

- Large numbers: Use 64-bit integers (`long long`, `i64`, `Int64`) to handle large ranges
- Empty input: Handle gracefully (0 ranges, 0 IDs)
- Single range: Merge algorithm should handle
- All ranges overlapping: Should merge to single range
- Off-by-one: Remember that range length = `end - start + 1` (inclusive)
- Adjacent ranges: `[a, b]` and `[b+1, c]` should merge to `[a, c]`
