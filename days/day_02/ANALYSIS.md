# Analysis for Day 02

## Overall Approach

The problem requires finding invalid product IDs within given ranges based on digit pattern rules. For Part 1, an ID is invalid if it consists of exactly two identical sequences (e.g., `55` → "5" twice, `6464` → "64" twice, `123123` → "123" twice). For Part 2, an ID is invalid if it consists of a sequence repeated 2+ times, covering the whole ID (generalizing Part 1 to include 3+, 4+, etc. repetitions).

The core algorithm involves: (1) parsing input ranges in the format `start-end, start-end, ...`, (2) iterating through all numbers in each range, (3) converting each number to a string, and (4) checking if the string matches the invalid pattern rules. For Part 1, we check if the string length is even and the first half equals the second half. For Part 2, we check all possible divisors `k` from 2 to the string length to see if the string consists of the first `length/k` characters repeated `k` times.

**Clues**

- The problem explicitly states "exactly two identical digit‐sequences repeat" for Part 1, indicating we need to check if a string can be split into two identical halves
- The phrase "patterns like `111` ('1' thrice), or `1212` are *not* invalid under Part 1" clarifies that `111` doesn't count (because it's not exactly two sequences - it would be three), but `1212` is actually valid for Part 1 (it IS two sequences of "12")
- Part 2's description "some sequence repeated two or more times" generalizes Part 1, indicating we need to check all possible divisors and repetitions
- The note "IDs have no leading zeros" ensures we can safely convert integers to strings without special cases
- The constraint "potentially exceeding 32-bit integer limits" suggests we should use 64-bit integers or arbitrary-precision types

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n × m × k) where n = number of ranges, m = average range size (end - start + 1), k = average string length of numbers. Each number requires a string conversion O(k) and a string comparison O(k).
  - Part 2: O(n × m × k²) where k² comes from checking all divisors (2 to k) for each number, each requiring O(k) string comparison and O(k) string repetition construction.
- **Space Complexity**: O(k) where k is the length of the longest number string - needed for string conversion, pattern extraction, and pattern repetition construction.

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 168   | [Needs fix]                 |
| Clojure    | 64    | 1687                        |
| Elixir     | 82    | [Needs fix]                 |
| Go         | 100   | 863                         |
| Haskell    | 56    | 12048                       |
| Java       | 92    | 350                         |
| Julia      | 86    | 1117                        |
| Perl       | 86    | 4563                        |
| Python     | 73    | 1310                        |
| Ruby       | 61    | 2498                        |
| Rust       | 81    | 783                         |
| TypeScript | 84    | 2069                        |

### C
The C implementation uses manual memory management through the `InputLines` utility structure and pointer-based parameter passing for results. String parsing uses `strtok` to split by commas and dashes, and `atoll` for parsing large integers. String comparison uses `strncmp` for substring checks, and pattern repetition is checked by manually comparing each segment of the repeated pattern.

### Clojure
Clojure uses immutable data structures and functional programming patterns. Range parsing uses `clojure.string/split` and `Long/parseLong`. The solution uses `loop/recur` for iteration or `reduce` for accumulation. String repetition checking uses `repeat` and `apply str` to build repeated patterns. The functional style means no mutable variables—state is passed as parameters.

### Elixir
Elixir uses pattern matching extensively for parsing ranges. Enum pipelines with `Enum.reduce` process ranges functionally. String manipulation uses `String.slice/3` for substrings and `String.duplicate/2` for pattern repetition. The solution leverages Elixir's binary pattern matching and pipe operators for clean, readable code.

### Go
Go's implementation is straightforward and imperative, using `strings.Split` for parsing and `strconv.Atoi` or `strconv.FormatInt` for number conversion. String manipulation uses slice notation `str[i:j]` and `strings.Repeat` or `strings.Builder` for pattern repetition. The implementation uses `int64` for large numbers to handle potential 32-bit overflow.

### Haskell
Haskell uses pure functional recursion with explicit base cases. String manipulation uses `take` and `drop` for substrings, and `replicate` with `concat` for pattern repetition. Range parsing uses `splitOn` from Data.List.Split and `read` for parsing. The solution can use list comprehensions or `foldl'` for accumulation, maintaining referential transparency.

### Java
Java uses object-oriented collections and standard library methods like `String.split()`, `String.valueOf()`, and `Long.parseLong()` for large numbers. String manipulation uses `substring()` and `String.repeat(k)` (Java 11+) for pattern repetition. The implementation uses `long` types to handle large numbers.

### Julia
Julia uses simple `split()` for parsing and `string()` for conversion. String slicing uses `str[1:n]` syntax, and `repeat()` is used for pattern repetition. Array indexing and ranges follow Julia conventions. The implementation leverages Julia's performance characteristics with straightforward code.

### Perl
Perl uses `split /,/, $line` to parse comma-separated ranges, then regex pattern matching `$range_str =~ /(\d+)-(\d+)/` with capture groups `$1` and `$2` to extract start and end values into anonymous array references `[$1, $2]`. The implementation iterates through ranges using Perl's idiomatic range operator: `for my $num ($start..$end)` which iterates through all integers in the inclusive range. String validation uses `substr()` for slicing: `substr($id_str, 0, $half)` for the first half and `substr($id_str, $half)` for the second half. The function returns 0 or 1 using a ternary operator: `return $first_half eq $second_half ? 1 : 0`. For Part 2, Perl leverages the string repetition operator `x` to check patterns: `$id_str eq $pattern x $k` elegantly checks if a string consists of a pattern repeated k times. This is more concise than manually constructing repeated strings in most languages. The implementation uses `grep` with `/\S/` to filter non-empty lines and string interpolation `"$num"` for implicit string conversion.

### Python
Python uses simple `split()` for parsing and `str()` for conversion. String slicing uses `str[i:j]` syntax, and `str * k` provides elegant pattern repetition. The implementation uses straightforward loops and can leverage list comprehensions. The code is clear and readable, typical of Python's philosophy.

### Ruby
Ruby uses `split()` for parsing and `to_s` for conversion. String slicing uses `str[i, len]` or range notation `str[i..j]`, and `str * k` provides pattern repetition. The implementation can use `each` or `reduce` blocks, with concise and readable syntax.

### Rust
Rust requires explicit ownership management and borrowing (`&str`). Pattern matching and string methods like `split()` and `split_once()` are used for parsing. String slicing uses `&str[start..end]` syntax, and `str.repeat(k)` provides pattern repetition. Mutability is explicitly marked with `mut`. The ownership system ensures memory safety.

### TypeScript
TypeScript provides type annotations for clarity. String parsing uses `split()`, `toString()`, and `parseInt()`. String slicing uses `slice()` or `substring()`, and `str.repeat(k)` provides pattern repetition. The implementation can use functional (`reduce`, `map`) or imperative (`for...of`) styles.

## Key Observations

The most significant algorithmic difference between Part 1 and Part 2 is the complexity: Part 1 requires a single check (is length even? does first half equal second half?), while Part 2 requires checking all divisors from 2 to the string length. This makes Part 2 quadratically more complex in terms of string length, though in practice the number of divisors is typically small.

String manipulation approaches vary interestingly across languages: pattern matching (Elixir's binary patterns), slicing (Python's `str[i:j]`, Ruby's `str[i..j]`), substring methods (Java's `substring()`, Rust's `&str[i..j]`), and functional approaches (Haskell's `take`/`drop`). These differences reflect language idioms more than algorithmic necessity.

Pattern repetition is elegantly handled in most languages: Python and Ruby use `str * k`, Java and Rust use `str.repeat(k)`, while C requires manual loops. Functional languages use `replicate`/`repeat` combined with concatenation.

The solutions are algorithmically identical across languages—the core challenge is correctly identifying when a string consists of a repeated pattern. Language differences primarily manifest in parsing styles, string manipulation syntax, and state management paradigms rather than algorithmic choices.

Memory considerations are minimal since we process one number at a time, though Part 2 requires constructing temporary repeated strings for comparison. C requires explicit memory management, Rust enforces it through ownership, while garbage-collected languages handle it automatically.

## Notes

Today was the first day where I attempted to do the full /day command. The result involved a lot of hiccups, which necessitated certain refinements.
