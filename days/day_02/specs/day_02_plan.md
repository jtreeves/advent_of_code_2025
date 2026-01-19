# Technical Plan: Day 02

## Algorithms and Data Structures

### Algorithm Overview

The problem requires finding invalid product IDs within given ranges. An ID is invalid based on digit pattern rules:

**Part 1**: ID is invalid if it consists of exactly two identical sequences (e.g., `55`, `6464`, `123123`).

**Part 2**: ID is invalid if it consists of a sequence repeated 2+ times, covering the whole ID (e.g., `111`, `6464`, `123123123`).

### Key Algorithms

1. **Range Parsing**: Parse input line to extract ranges:
   - Split by commas
   - For each range, split by `-` to get start and end
   - Parse integers from strings

2. **Part 1 Validation**: Check if ID is exactly two identical sequences:
   - Convert number to string
   - Check if length is even
   - If even, split string in half
   - Check if first half equals second half
   - If yes, ID is invalid

3. **Part 2 Validation**: Check if ID is a sequence repeated 2+ times:
   - Convert number to string
   - For each possible divisor `k` from 2 to string length:
     - If `length % k == 0`:
       - Extract pattern of length `length / k`
       - Check if string equals pattern repeated `k` times
       - If yes, ID is invalid

### Data Structures

- **No complex data structures needed**: Simple string/integer operations
- **Input parsing**: Split string by commas and dashes to extract ranges
- **String operations**: Convert integers to strings, substring extraction, string comparison

### Why This Approach is Optimal

- **Time Complexity**: O(n × m × k) where n is number of ranges, m is average range size, and k is average number length. For Part 2, checking all divisors adds O(k) per number. This is optimal since we must examine each number in each range.
- **Space Complexity**: O(1) per number - only need string representation and temporary variables. However, total space depends on range sizes when iterating.
- The problem inherently requires checking each number, so the algorithm cannot be more efficient

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n × m × k) where n = number of ranges, m = average range size (end - start + 1), k = average string length of numbers
  - Part 2: O(n × m × k²) where k² comes from checking all divisors (2 to k) for each number
- **Space Complexity**: O(k) where k is the length of the longest number string - needed for string conversion and pattern matching

## Approach

1. **Parse Input**: Read single line, split by commas, parse each range `start-end`
2. **Iterate Ranges**: For each range, iterate from start to end (inclusive)
3. **Check Each Number**:
   - Convert number to string
   - **Part 1**: Check if length is even and first half equals second half
   - **Part 2**: Check all divisors k ≥ 2 to see if string equals pattern repeated k times
4. **Sum Invalid IDs**: Add invalid IDs to running sum
5. **Return Sum**: Return total sum for each part

### Optimization Considerations

- For Part 2, we can early-exit once we find a valid divisor match
- String comparison operations are typically O(k) where k is string length
- For very large ranges, we might consider mathematical optimizations, but the straightforward approach is clear and correct

## Implementations

### C
- Use `strtok` to split by commas and dashes
- Parse integers with `atoll` or `strtol` for large numbers
- Convert number to string with `snprintf`
- String comparison with `strncmp` for substring checks
- Manual string manipulation for pattern repetition checks

### Clojure
- Use `clojure.string/split` to parse ranges
- Convert numbers with `str`
- String comparison with `=` and `subs` for substrings
- Use `repeat` and `apply str` to build repeated patterns
- Functional approach with `reduce` and `filter`

### Elixir
- Use `String.split/3` to parse ranges
- Convert numbers with `Integer.to_string/1`
- Pattern matching and `String.slice/3` for substrings
- Use `String.duplicate/2` for pattern repetition
- Enum pipelines with `Enum.reduce`

### Go
- Use `strings.Split` to parse ranges
- Convert numbers with `strconv.Itoa` or `fmt.Sprintf` for large numbers
- String slicing with `str[i:j]` syntax
- Use `strings.Builder` or `strings.Repeat` for pattern repetition

### Haskell
- Use `splitOn` from Data.List.Split to parse ranges
- Convert numbers with `show`
- String manipulation with `take` and `drop`
- Pattern repetition with `replicate` and `concat`
- Functional approach with list comprehensions or `foldl'`

### Java
- Use `String.split()` to parse ranges
- Convert numbers with `String.valueOf()` or `Integer.toString()`
- String manipulation with `substring()`
- Use `String.repeat(k)` for pattern repetition (Java 11+)
- Imperative approach with loops

### Julia
- Use `split()` to parse ranges
- Convert numbers with `string()`
- String slicing with `str[1:n]` syntax
- Use `repeat()` for pattern repetition
- Array indexing and ranges

### Perl
- Use `split /,/, $line` to parse comma-separated ranges
- Regex pattern matching: `$range_str =~ /(\d+)-(\d+)/` with capture groups `$1`, `$2`
- String slicing with `substr($str, $start, $length)`
- String repetition operator `x`: `$pattern x $k` to repeat a pattern k times
- String comparison with `eq` and length checking with `length($str)`
- Step-by-step implementation:
  1. Parse ranges using `split /,/, $line` then regex matching for start-end pairs
  2. For each ID, convert to string with implicit conversion or `"$id"`
  3. Part 1: Check if length is even, split in half with `substr()`, compare halves with `eq`
  4. Part 2: Loop through divisors `k` from 2 to length, extract pattern, check if string equals `$pattern x $k`
  5. Return tuple of sums
- Use `repeat()` for pattern repetition
- Can use either functional or imperative style

### Python
- Use `split()` to parse ranges
- Convert numbers with `str()`
- String slicing with `str[i:j]` syntax
- Use `str * k` for pattern repetition
- Simple imperative loops or list comprehensions

### Ruby
- Use `split()` to parse ranges
- Convert numbers with `to_s`
- String slicing with `str[i, len]` or range notation
- Use `str * k` for pattern repetition
- Block-based iteration or functional methods

### Rust
- Use `split()` and `split_once()` to parse ranges
- Convert numbers with `to_string()`
- String slicing with `&str[start..end]` syntax
- Use `str.repeat(k)` for pattern repetition
- Ownership-aware string handling

### TypeScript
- Use `split()` to parse ranges
- Convert numbers with `toString()`
- String slicing with `slice()` or `substring()`
- Use `str.repeat(k)` for pattern repetition
- Can use functional or imperative style
