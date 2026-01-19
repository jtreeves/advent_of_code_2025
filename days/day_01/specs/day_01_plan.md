# Technical Plan: Day 01

## Algorithms and Data Structures

### Algorithm Overview

The problem requires simulating a circular dial with 100 positions (0-99) that starts at position 50. For each rotation command (L or R followed by a distance), we need to:

**Part 1**: Track the dial position after each rotation and count how many times it ends at 0.

**Part 2**: Track every position the dial passes through during each rotation (not just the end position) and count how many times it equals 0.

### Key Algorithms

1. **Circular Arithmetic**: Use modulo arithmetic to handle wrapping:
   - For left rotation: `new_pos = (current_pos - distance) % 100`
   - For right rotation: `new_pos = (current_pos + distance) % 100`
   - Note: Need to handle negative modulo correctly (add 100 if negative before modulo)

2. **Part 1 Implementation**:
   - Start with position = 50
   - For each rotation, parse direction and distance
   - Apply rotation using modulo arithmetic
   - If new position is 0, increment counter
   - Return counter

3. **Part 2 Implementation**:
   - Start with position = 50, counter = 0
   - For each rotation:
     - Determine start and end positions
     - Simulate each click/position during the rotation
     - Count how many times position equals 0
   - Return total counter

### Data Structures

- **No complex data structures needed**: Simple integer variables for position and counters
- **Input parsing**: Read lines and parse string to extract direction (L/R) and distance (integer)
- **Circular arithmetic**: Use modulo operations for position wrapping

### Why This Approach is Optimal

- **Time Complexity**: O(n * d) where n is number of rotations and d is average distance per rotation. For Part 2, we must simulate each position during rotation. This is optimal since we must examine each position to count zeros.
- **Space Complexity**: O(1) - only need a few integer variables
- The problem inherently requires checking every position during large rotations for Part 2, so the algorithm cannot be more efficient

## Complexity Analysis

- **Time Complexity**: O(n * d) where n = number of rotation lines, d = average distance per rotation
  - Part 1: O(n) - only check end positions
  - Part 2: O(n * d) - must check every position during each rotation
- **Space Complexity**: O(1) - constant space, only tracking current position and counters

## Approach

1. **Parse Input**: Read input file line by line, extract direction (L/R) and distance for each rotation
2. **Initialize**: Start position = 50, count = 0
3. **Process Rotations**:
   - For each rotation:
     - **Part 1**: Calculate end position using modulo arithmetic, check if 0, increment count
     - **Part 2**: Simulate the rotation step-by-step (or mathematically calculate positions that cross 0), count zeros
4. **Return Count**: Return the total count

### Part 2 Optimization

Instead of simulating every single click, we can mathematically calculate how many times we cross 0:
- For a rotation from `start` to `end`:
  - If rotating right: count zeros when passing from 99 → 0
  - If rotating left: count zeros when passing from 0 → 99 (wrapping backward)
- Calculate total positions traversed, determine wrap-around points, count zeros

However, for clarity and correctness, simulating each position during the rotation is safer and more straightforward.

## Implementations

### C
- Manual string parsing using character array iteration
- Use `%` operator for modulo (handle negative results: `(pos % 100 + 100) % 100`)
- Step-by-step implementation:
  1. Read file line by line using standard I/O
  2. Parse first character as direction, rest as integer
  3. For each rotation, update position using modulo arithmetic
  4. For Part 2, simulate each step in a loop
  5. Return count as integer

### Clojure
- Use `split-lines` and `map` to parse rotations
- Immutable state: use `reduce` or recursive function to accumulate position and count
- Step-by-step implementation:
  1. Read file with `slurp`, split by newlines
  2. Parse each line: first char as direction, rest as integer with `Integer/parseInt`
  3. Use `reduce` with accumulator `{:pos 50 :count 0}`
  4. For Part 2, use `range` or `take-while` to generate positions during rotation
  5. Return final count

### Elixir
- Pattern matching for parsing rotations
- Enum pipelines for processing
- Step-by-step implementation:
  1. Read file with `File.read!`, split by newlines
  2. Parse with pattern matching: `<<dir::binary-size(1)>> <> dist_str = line`
  3. Use `Enum.reduce` with `{position, count}` tuple
  4. For Part 2, use `Enum.flat_map` or recursion to generate all positions
  5. Return count

### Go
- Simple file reading with `bufio.Scanner`
- Parse strings with `strconv.Atoi`
- Step-by-step implementation:
  1. Open file, use `bufio.NewScanner` to read lines
  2. Parse: first byte as direction, rest as integer
  3. Loop through rotations, update position and count
  4. For Part 2, inner loop to simulate each position
  5. Return count

### Haskell
- Functional parsing with pattern matching
- Use `mod` function for modulo arithmetic
- Step-by-step implementation:
  1. Read file with `readFile`, split by lines
  2. Parse with pattern matching and `read`
  3. Use `foldl'` to accumulate `(position, count)`
  4. For Part 2, use `[start..end]` or generate list of positions
  5. Return count

### Java
- Use `Files.readAllLines` or `BufferedReader`
- Parse with `String.substring` and `Integer.parseInt`
- Step-by-step implementation:
  1. Read file as list of strings
  2. Parse each line: `charAt(0)` for direction, `substring(1)` for distance
  3. Loop through rotations with position and count variables
  4. For Part 2, nested loop to iterate through each position
  5. Return count as int

### Julia
- Simple file reading with `readlines`
- Parse with `parse(Int, ...)`
- Step-by-step implementation:
  1. Read file with `readlines`
  2. Parse: `line[1]` for direction, `parse(Int, line[2:end])` for distance
  3. Use `foldl` or loop to process rotations
  4. For Part 2, use range or collect to generate positions
  5. Return count

### Perl
- Use `use strict; use warnings;` for safety
- File I/O uses `open my $fh, '<', "file"` and slurps with `do { local $/; <$fh> }`
- String parsing uses `substr($line, 0, 1)` for single character extraction and `int(substr($line, 1))` for numeric conversion
- Step-by-step implementation:
  1. Slurp entire file into string, split by newlines with `split /\n/`
  2. Use `for my $line (@lines)` to iterate
  3. Parse direction with `substr($line, 0, 1)` and distance with `int(substr($line, 1))`
  4. Use modulo arithmetic with proper negative handling: `(($position - $distance) % 100 + 100) % 100`
  5. For Part 2, use `for my $click (1..$distance)` to simulate each position during rotation
  6. Return tuple `($part1, $part2)` from solve function

### Python
- Simple file reading with `open().readlines()`
- Parse with string slicing: `line[0]` for direction, `int(line[1:])` for distance
- Step-by-step implementation:
  1. Read file, split by newlines
  2. Parse each line with slicing and `int()`
  3. Loop through rotations, update position and count
  4. For Part 2, use `range()` to generate positions during rotation
  5. Return count

### Ruby
- Use `File.readlines` or `File.read.split("\n")`
- Parse with string indexing: `line[0]` and `line[1..-1].to_i`
- Step-by-step implementation:
  1. Read file, split by newlines
  2. Parse: `[0]` for direction, `[1..-1].to_i` for distance
  3. Use `each` or `reduce` to process rotations
  4. For Part 2, use `(start..end)` or array generation for positions
  5. Return count

### Rust
- Use `fs::read_to_string` and `lines()`
- Parse with pattern matching or string methods
- Step-by-step implementation:
  1. Read file to string, split by lines
  2. Parse: `line.chars().next()` for direction, rest as `i32`
  3. Use `fold` or loop with mutable `position` and `count`
  4. For Part 2, use `Range` or explicit loop to simulate positions
  5. Return count

### TypeScript
- Use `fs.readFileSync` and `split('\n')`
- Parse with string methods: `line[0]` and `parseInt(line.slice(1))`
- Step-by-step implementation:
  1. Read file, split by newlines
  2. Parse: `charAt(0)` for direction, `slice(1)` for distance
  3. Use `reduce` or `for...of` to process rotations
  4. For Part 2, use `Array.from` or loop to generate positions
  5. Return count as number