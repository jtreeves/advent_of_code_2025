# Technical Plan: Day 03

## Algorithms and Data Structures

### Algorithm Overview

The problem requires finding the largest N-digit number (N = 2 for Part 1, N = 12 for Part 2) that can be formed by selecting N digits in order from each bank of batteries. This is a classic **greedy algorithm** problem for finding the lexicographically largest subsequence of a fixed length.

**Part 1**: For each bank, find the largest 2-digit number by selecting exactly 2 digits in order.

**Part 2**: For each bank, find the largest 12-digit number by selecting exactly 12 digits in order.

### Key Algorithms

1. **Greedy Selection Algorithm**: For each bank (string of digits), find the largest N-digit subsequence:
   - At each position `i` (0-indexed) in the output, we need to select a digit from the remaining portion of the string
   - We need `(N - i - 1)` digits remaining after the current selection
   - At position `i`, pick the **largest digit** available in the remaining substring such that there are at least `(N - i - 1)` digits left after it
   - For each position, iterate through the remaining digits and find the maximum digit that still allows completing the sequence

2. **Part 1 Implementation**:
   - For each bank string `s`:
     - Initialize result as empty string
     - For position 0: find max digit in `s[0..len-1]` that has at least 1 digit after it
     - For position 1: find max digit in remaining substring that has at least 0 digits after it
     - Return the 2-digit number formed

3. **Part 2 Implementation**:
   - Same greedy approach but with N = 12
   - For each position 0 to 11, select the maximum digit from remaining substring that allows completion

### Data Structures

- **Strings/Character arrays**: Each bank is represented as a string of digits
- **No complex data structures needed**: Simple string indexing and iteration
- **Input parsing**: Read lines directly as strings (each line is already a bank)

### Why This Approach is Optimal

- **Time Complexity**: O(m × N) where m is the average bank length and N is the number of digits to select (2 or 12). For each position, we scan at most m digits. This is optimal since we must examine digits to make greedy choices.
- **Space Complexity**: O(1) per bank - only need temporary variables for tracking positions and building result. Total space O(m) for storing the bank string.
- The greedy approach is optimal because: at each position, choosing a larger digit always yields a larger final number, and we ensure enough digits remain to complete the sequence.

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(m × 2) = O(m) where m = average bank length
  - Part 2: O(m × 12) = O(m) where m = average bank length
  - For all banks: O(n × m) where n = number of banks
- **Space Complexity**: O(m) where m is the maximum bank length - needed to store the bank string. Result building is O(N) = O(1) for practical purposes.

## Approach

1. **Parse Input**: Read input file line by line, each line is a bank (string of digits)
2. **For Each Bank**:
   - Apply greedy algorithm to select N digits (N = 2 for Part 1, N = 12 for Part 2)
   - Build the result number by selecting digits in order
3. **Sum Results**: Add the maximum number from each bank
4. **Return Sum**: Return the total sum for each part

### Greedy Algorithm Details

For finding the largest N-digit subsequence from string `s` of length `L`:
- Initialize `result = ""`, `start = 0`
- For `i` from 0 to N-1:
  - `remaining_positions_needed = N - i - 1`
  - `end = L - remaining_positions_needed` (last position we can choose from)
  - Find `max_pos` in range `[start, end]` where `s[max_pos]` is maximum
  - Append `s[max_pos]` to result
  - Update `start = max_pos + 1`
- Return `result` converted to integer

## Implementations

### C
- Use `fgets` or character-by-character reading for input lines
- String manipulation with `char*` arrays and indexing
- Manual digit extraction and comparison using `char` comparison (digits '1'-'9')
- Convert final string to `long long` with `strtoll` for large numbers
- Step-by-step:
  1. Read file line by line
  2. For each line, apply greedy algorithm with nested loops
  3. Build result string character by character
  4. Convert to integer and add to sum
  5. Return sum

### Clojure
- Use `slurp` and `split-lines` to read input
- String manipulation with `subs` and `char` comparison
- Use `reduce` or recursion to implement greedy selection
- Convert final string to integer with `Long/parseLong`
- Functional approach with immutable strings

### Elixir
- Use `File.read!` and `String.split` to read input
- String slicing with `String.slice/3` and `String.graphemes/1`
- Pattern matching for string operations
- Use `Integer.parse/1` or `String.to_integer/1` for conversion
- Enum pipelines with `Enum.reduce` for greedy selection

### Go
- Use `bufio.Scanner` to read lines
- String slicing with `str[i:j]` syntax
- Character access with `str[i]` (byte type, compare as runes or bytes)
- Convert with `strconv.Atoi` for small numbers, `strconv.ParseInt` for large
- Loop-based greedy implementation

### Haskell
- Use `readFile` and `lines` to read input
- String manipulation with list operations: `take`, `drop`, `maximum`
- Pattern matching and recursion for greedy algorithm
- Convert with `read` or `readInteger` for large numbers
- Functional approach with `foldl` or recursive functions

### Java
- Use `Files.readAllLines` or `BufferedReader` to read input
- String methods: `charAt`, `substring`, `compareTo` for digit comparison
- Convert with `Long.parseLong` for large numbers (12-digit numbers fit in long)
- Loop-based greedy implementation with `StringBuilder` or string concatenation

### Julia
- Use `readlines` to read input
- String indexing with `s[i]` or `s[i:j]` for slicing
- Character comparison directly
- Convert with `parse(Int, str)` or `parse(BigInt, str)` if needed
- Loop-based greedy implementation

### Perl
- Use `split /\n/, $input_data` to split into lines
- String character access with `substr($bank, $j, 1)` 
- Character comparison with string operators: `$digit gt $max_digit` (greater than)
- Array building with `push @result, $max_digit`
- String building by joining array: `join('', @result)`
- Step-by-step implementation:
  1. Read file and split into lines
  2. For each bank string, use greedy algorithm with nested loops
  3. Find maximum digit in valid range using `substr($bank, $j, 1)` and `gt` comparison
  4. Track position and build result array with `push`
  5. Convert result array to integer with `int(join('', @result))`
- Convert with `toLong()` or `toBigInteger()` for large numbers
- Loop-based greedy with string building

### Python
- Use `open().readlines()` to read input
- String indexing `s[i]` and slicing `s[i:j]`
- Character comparison with `>` operator
- Convert with `int(str)` - Python ints are arbitrary precision
- Loop-based greedy implementation, can build result as string or list

### Ruby
- Use `File.readlines` to read input
- String indexing `str[i]` and slicing `str[i..j]`
- Character comparison with `>` operator
- Convert with `to_i` - Ruby integers are arbitrary precision
- Loop-based or `each_with_index` for greedy selection

### Rust
- Use `fs::read_to_string` and `lines()` iterator
- String slicing with `&str[i..j]` syntax
- Character access with `chars().nth(i)` or `as_bytes()[i]`
- Convert with `str.parse::<u64>()` or `parse::<u128>()` for large numbers
- Loop-based greedy with `String` building or iterator chains

### TypeScript
- Use `fs.readFileSync` and `split('\n')` to read input
- String indexing `str[i]` and slicing `str.slice(i, j)`
- Character comparison with `>` operator
- Convert with `parseInt(str)` or `BigInt(str)` for large numbers (12-digit fits in Number)
- Loop-based greedy with string building or array methods
