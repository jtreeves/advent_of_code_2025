# Analysis for Day 01

## Overall Approach

The problem simulates a circular dial with 100 positions (0-99) starting at position 50. For Part 1, we track the dial position after each rotation command and count how many times it ends at position 0. For Part 2, we also count every intermediate position during each rotation where the dial points at 0, not just the final position.

The core algorithm uses modular arithmetic to handle the circular nature of the dial. For left rotations (L), we subtract the distance and use modulo with proper handling of negative values: `((position - distance) % 100 + 100) % 100`. For right rotations (R), we add the distance and use standard modulo: `(position + distance) % 100`.

Part 1 is straightforward: apply each rotation and check if the final position equals 0. Part 2 requires simulating each "click" (intermediate position) during the rotation to count how many times the dial points at 0, which is necessary because rotations can wrap around multiple times (e.g., R1000 passes through 0 ten times).

**Clues**

- The problem explicitly states "the dial is a circle" and "turning the dial left from 0 one click makes it point at 99" - indicating circular/wrapping behavior that requires modular arithmetic
- The requirement "count the number of times the dial is left pointing at 0" suggests we need to track state transitions, not just final positions
- Part 2's phrase "method 0x434C49434B" and "any click causes the dial to point at 0" explicitly indicates we must check intermediate positions during rotations, not just endpoints
- The example "R1000 would cause the dial to point at 0 ten times" confirms that large rotations wrap around multiple times, necessitating step-by-step simulation in Part 2

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n) where n is the number of rotation lines. Each rotation requires a constant-time position update and comparison.
  - Part 2: O(n × d) where n is the number of rotation lines and d is the average distance per rotation. This is because we must simulate each intermediate position during each rotation to count zeros. In the worst case, large rotations like R1000 require checking 1000 positions.
- **Space Complexity**: O(1) - constant space regardless of input size. We only need a few integer variables to track the current position and counters for both parts. No additional data structures scale with input size.

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 131   | 185                         |
| Clojure    | 57    | 464                         |
| Elixir     | 71    | 393                         |
| Go         | 96    | 297                         |
| Haskell    | 61    | 345                         |
| Java       | 76    | 76                          |
| Julia      | 64    | 4323                        |
| Perl       | 70    | 160                         |
| Python     | 58    | 71                          |
| Ruby       | 66    | 207                         |
| Rust       | 80    | 202                         |
| TypeScript | 73    | 3875                        |

### C
The C implementation uses manual memory management through the `InputLines` utility structure and pointer-based parameter passing (`*part1_result`, `*part2_result`). String parsing uses `atoi()` to convert character arrays to integers. Modulo arithmetic requires explicit handling: `((position - distance) % 100 + 100) % 100` to ensure non-negative results for left rotations.

### Clojure
Clojure uses immutable data structures and recursion via `loop/recur` for tail-call optimization. The `reduce` function is used to count zeros during rotations, applying a function across a range `(range 1 (inc distance))`. Pattern matching is implicit through `first` and `subs` for parsing direction and distance. The functional style means no mutable variables—state is passed as parameters to recursive functions.

### Elixir
Elixir uses pattern matching extensively, including binary pattern matching for parsing: `<<direction::binary-size(1)>> <> dist_str = line`. Enum pipelines with `Enum.reduce` process rotations functionally. Tail recursion via `defp` functions ensures efficient iteration. The solution uses pattern matching on function heads (e.g., `solve_part1([], _position, count)`) for base cases.

### Go
Go's implementation is straightforward and imperative, using simple `for` loops and multiple return values `(string, string)`. String parsing uses `strconv.Atoi` and byte indexing `line[0]`. The implementation is verbose but clear, with explicit variable declarations and no special abstractions. Error handling is minimal (using `_` to ignore parse errors).

### Haskell
Haskell uses pure functional recursion with explicit base cases (`solvePart1 [] _ count = count`). List comprehensions elegantly count zeros: `sum [1 | click <- [1..distance], clickPos == 0]`. The `mod` function handles modulo arithmetic, with explicit handling for negative results similar to C. Pattern matching extracts direction with `head` and distance with `read (tail trimmed)`. Functions are separated for Part 1 and Part 2, maintaining referential transparency.

### Java
Java uses object-oriented collections (`List<String>` from input utilities) and standard library methods like `String.charAt(0)` and `Integer.parseInt()`. The implementation is imperative with mutable integer variables. String manipulation uses `substring()` methods. The code structure is straightforward, leveraging Java's mature standard library.

### Julia
Julia uses simple `readlines` for input and `parse(Int, ...)` for string-to-integer conversion. The implementation is similar to Python in style but leverages Julia's performance characteristics. Array indexing `line[1]` for direction and `line[2:end]` for distance follows Julia conventions.

### Perl
Perl uses `use strict; use warnings;` for safety. File I/O uses lexical filehandles (`open my $fh, '<', "file"`) and slurps the entire file with `do { local $/; <$fh> }` (the `local $/` idiom makes `<>` read the whole file at once). Input filtering uses `grep { $_ =~ /\S/ }` to skip empty lines. String parsing uses `substr($line, 0, 1)` for character extraction and `int(substr($line, 1))` for numeric conversion. The implementation uses `for my $line (@lines)` for iteration and `for my $click (1..$distance)` for Part 2's position simulation, leveraging Perl's range operator. Modulo arithmetic requires the same negative handling as C: `(($position - $distance) % 100 + 100) % 100` because Perl's `%` operator can return negative values. Part 2 tracks the starting position `my $start_pos = $position` before simulating clicks, then updates the position after checking all intermediate positions. The solution returns a tuple `($part1, $part2)` which is destructured with `my ($part1, $part2) = solve($data)`.

### Python
Python uses simple string slicing `line[0]` and `line[1:]` with `int()` conversion. The implementation uses straightforward `for` loops and `range(1, distance + 1)` for Part 2's intermediate position checking. List comprehensions could be used but aren't necessary here. The code is clear and readable, typical of Python's philosophy.

### Ruby
Ruby uses `first` and array slicing `line[1..-1].to_i` for parsing. The implementation can use `each` or `reduce` blocks, though simple loops are also idiomatic. Ruby's dynamic typing means no type annotations needed. The code style is concise and readable.

### Rust
Rust requires explicit ownership management and borrowing (`&lines`, `&str`). Pattern matching is used for parsing with `chars().next()` and `parse()`. Mutability is explicitly marked with `mut` for `position` and counters. String handling uses `trim()` and indexing methods. The ownership system ensures memory safety without garbage collection.

### TypeScript
TypeScript provides type annotations for clarity (though minimal in this solution). String parsing uses `charAt(0)` and `parseInt(line.slice(1))`. The implementation can use `Array.from()` with ranges or simple `for` loops. TypeScript compiles to JavaScript, so runtime behavior matches JavaScript with compile-time type checking.

## Key Observations

The most significant difference across implementations is how languages handle the modular arithmetic for negative numbers. Languages with C-style modulo (C, Go, Rust) need explicit handling: `((position - distance) % 100 + 100) % 100` because `%` can return negative values. Languages with mathematical modulo (Haskell, Clojure) can use simpler expressions, though some still use the double-modulo pattern for clarity.

Functional languages (Haskell, Clojure, Elixir) naturally express the recursive structure of processing rotations, while imperative languages (C, Go, Java) use straightforward loops. Haskell's list comprehensions for Part 2 (`sum [1 | click <- [1..distance], clickPos == 0]`) are particularly elegant, contrasting with the explicit loops in imperative languages.

Part 2's requirement to check intermediate positions highlights a key algorithmic difference: while Part 1 can simply track final positions, Part 2 necessitates simulating each step during rotation. All implementations use an inner loop/iteration for this, though functional languages use `reduce`, `map`, or list comprehensions rather than explicit loops.

String parsing approaches vary interestingly: pattern matching (Elixir's binary patterns, Haskell's `head`/`tail`), slicing (Python's `line[1:]`, Ruby's `line[1..-1]`), and parsing utilities (Go's `strconv.Atoi`, Rust's `parse()`). These differences reflect language idioms more than algorithmic necessity.

Memory management is notable: C requires explicit management through utility structures, Rust enforces it through ownership, while garbage-collected languages (Java, Python, Haskell) handle it automatically. This doesn't significantly impact the solution since we only use O(1) space, but it affects code structure.

The solutions are algorithmically identical across languages—the core challenge is handling circular arithmetic correctly and simulating intermediate positions for Part 2. Language differences primarily manifest in syntax, parsing styles, and state management paradigms rather than algorithmic choices.

## Notes

I did this one manually first in a separate repo, so this was somewhat kludged together.
