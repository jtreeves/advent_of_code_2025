# Analysis for Day 03

## Overall Approach

The problem requires finding the largest N-digit number (N = 2 for Part 1, N = 12 for Part 2) that can be formed by selecting N digits in order from each bank of batteries. This is a classic **greedy algorithm** problem for finding the lexicographically largest subsequence of a fixed length.

The greedy approach is optimal: at each position i (0 to N-1), we select the largest digit available in the remaining substring such that there are still enough digits remaining to complete the sequence. Specifically, for position i, we need at least (N - i - 1) digits remaining after the selected digit. We scan the valid range [start, end] where `end = bank_len - (N - i - 1)`, find the maximum digit, add it to our result, and move the start position to just after the selected digit.

**Clues**

- The problem states "you cannot rearrange batteries" and "the joltage that the bank produces is equal to the number formed by the digits on the batteries you've turned on" - indicating we must select digits in their original order to form a subsequence
- The requirement to find the "largest possible joltage" indicates we want the maximum numeric value, which for fixed-length numbers means lexicographically largest
- The constraint "exactly two batteries" (Part 1) and "exactly twelve batteries" (Part 2) means we're finding subsequences of fixed length, not any subsequence
- The examples show greedy selection: choosing the largest first digit while ensuring enough digits remain (e.g., `818181911112111` → `92` by selecting 9 and 2, not 8 and 9)

## Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n × m × 2) = O(n × m) where n is the number of banks and m is the average bank length. For each bank, we perform O(2) = O(1) iterations, and each iteration scans at most m digits.
  - Part 2: O(n × m × 12) = O(n × m) where n is the number of banks and m is the average bank length. For each bank, we perform O(12) = O(1) iterations, and each iteration scans at most m digits.
  - The greedy algorithm is optimal for this problem since we must examine digits to make choices, and each digit is examined at most once per iteration.

- **Space Complexity**: O(1) per bank - we only need temporary variables for tracking positions and building the result. The result string/array is at most length N (2 or 12), which is constant. Total space complexity is O(n) if we store all banks, but typically we process them one at a time, requiring only O(1) additional space.

## Solutions

### Part 1
| Language   | Initial Solution |
|------------|------------------|
| C          | 17535            |
| Clojure    | 17535            |
| Elixir     | 17535            |
| Go         | 17535            |
| Haskell    | 17535            |
| Java       | 17535            |
| Julia      | 17535            |
| Kotlin     | 17535            |
| Python     | 17535            |
| Ruby       | 17535            |
| Rust       | 17535            |
| TypeScript | 17535            |

### Part 2
| Language   | Initial Solution |
|------------|------------------|
| C          | 173577199527257  |
| Clojure    | 173577199527257  |
| Elixir     | 173577199527257  |
| Go         | 173577199527257  |
| Haskell    | 173577199527257  |
| Java       | 173577199527257  |
| Julia      | 173577199527257  |
| Kotlin     | 173577199527257  |
| Python     | 173577199527257  |
| Ruby       | 173577199527257  |
| Rust       | 173577199527257  |
| TypeScript | 173577199527257  |

## Performance

### Part 1
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 308                 |
| Clojure    | 493                 |
| Elixir     | 387                 |
| Go         | 223                 |
| Haskell    | 346                 |
| Java       | 63                  |
| Julia      | 679                 |
| Kotlin     | 126                 |
| Python     | 40                  |
| Ruby       | 100                 |
| Rust       | 360                 |
| TypeScript | 1852                |

### Part 2
| Language   | Execution Time (ms) |
|------------|---------------------|
| C          | 308                 |
| Clojure    | 493                 |
| Elixir     | 387                 |
| Go         | 223                 |
| Haskell    | 346                 |
| Java       | 63                  |
| Julia      | 679                 |
| Kotlin     | 126                 |
| Python     | 40                  |
| Ruby       | 100                 |
| Rust       | 360                 |
| TypeScript | 1852                |

## Implementation Differences

### C
The C implementation uses manual string manipulation with character arrays. The `find_largest_subsequence` function uses `char` array indexing and `strlen()` for length checks. String building is done character-by-character into a `char result[16]` array, then converted to integer using `atoll()`. File I/O uses standard `fgets()` with `strcspn()` to remove newlines. The implementation is straightforward and imperative with no dynamic memory allocation for the greedy algorithm itself.

### Clojure
Clojure uses immutable data structures and `loop/recur` for tail-call optimization in the greedy algorithm. String manipulation uses `subs` for slicing and `seq` to convert strings to character sequences. The `reduce` function finds the maximum character by comparing integer values: `(reduce #(if (> (int %2) (int %1)) %2 %1) candidates-chars)`. Position tracking uses `take-while` to find the index of the maximum character. Final string building uses `apply str` and `Long/parseLong` for conversion. The functional style means no mutable variables—all state is passed through loop parameters.

### Elixir
Elixir uses pattern matching and Enum pipelines extensively. The greedy algorithm uses `Enum.reduce` with a tuple accumulator `{"", 0}` for result string and start position. String slicing uses `String.slice(bank, start, end_pos - start)` and `String.graphemes` for character extraction. Maximum character finding uses `Enum.max()` on grapheme lists, and position finding uses `Enum.find_index` on the candidates list. The functional pipeline style `|>` chains operations cleanly. String building uses `<>` for concatenation and `String.to_integer()` for conversion.

### Go
Go's implementation is imperative and straightforward, using `strings.Builder` for efficient string building in the greedy algorithm. Character access uses byte indexing `bank[j]` and direct byte comparison. The `findLargestSubsequence` function uses simple `for` loops with explicit position tracking. String building is done byte-by-byte with `result.WriteByte(maxDigit)`, then converted using `strconv.ParseInt()`. The implementation is verbose but clear, with no special abstractions beyond standard library utilities.

### Haskell
Haskell uses a recursive helper function `helper` that accumulates the result string and start position. String manipulation uses `take` and `drop` for slicing: `take (end - start) $ drop start bank`. Maximum character finding uses `maximum candidates` directly on character lists. Position calculation uses `length (take-while (/= maxDigit) candidates)` to find the index. String building uses list concatenation `result ++ [maxDigit]` and `read` for conversion. The functional recursion style maintains referential transparency throughout.

### Java
Java uses `StringBuilder` for efficient string building in the greedy algorithm. Character access uses `bank.charAt(j)` and direct character comparison. The `findLargestSubsequence` method uses simple `for` loops with explicit position tracking. String building is done character-by-character with `result.append(maxDigit)`, then converted using `Long.parseLong()`. The implementation is object-oriented but straightforward, leveraging Java's standard library string methods.

### Julia
Julia uses 1-indexed arrays, so the greedy algorithm starts at position 1 instead of 0. String indexing uses `bank[j]` for character access and comparison. The `find_largest_subsequence` function uses `Char[]` for result building and `push!` to add characters. String conversion uses `String(result)` and `parse(Int, ...)`. The implementation handles `SubString` types from `split()` by converting them to `String` using `String(x)`. The code style is similar to Python but with Julia's performance characteristics.

### Kotlin
Kotlin uses `StringBuilder` for string building, similar to Java but with more concise syntax. Character access uses `bank[j]` with direct character comparison. The `findLargestSubsequence` function uses simple loops with `var` for mutable position tracking. String building uses `result.append(maxDigit)` and conversion uses `result.toString().toLong()`. Kotlin's type inference and concise syntax make the code cleaner than Java while maintaining similar structure.

### Python
Python uses simple string indexing `bank[j]` and character comparison with `>` operator. The `find_largest_subsequence` function builds the result as a list `result = []` and appends characters, then joins with `''.join(result)` and converts using `int()`. String slicing is idiomatic: `bank[start:end]`. The implementation is straightforward and readable, typical of Python's philosophy. No special data structures are needed—strings and lists suffice.

### Ruby
Ruby uses string indexing `bank[j]` and direct character comparison. The `find_largest_subsequence` function builds the result as an array `result = []` using `<<` for appending, then joins with `result.join.to_i` for conversion. String slicing uses `bank[start...end_pos]` with three-dot range syntax. The implementation is concise and idiomatic Ruby, with block syntax for iteration where appropriate.

### Rust
Rust requires explicit ownership management, using `&str` for string slices. Character access uses `bank.chars().nth(j)` which returns an `Option<char>`, requiring unwrapping. The `find_largest_subsequence` function builds the result as a `Vec<char>` using `result.push(maxDigit)`, then converts using `result.iter().collect::<String>().parse::<u64>()`. String manipulation uses standard Rust string methods. The ownership system ensures memory safety without garbage collection.

### TypeScript
TypeScript provides type annotations for clarity (`string`, `number`). String indexing uses `bank[j]` with direct character comparison. The `findLargestSubsequence` function builds the result as a string array `result: string[]` using `result.push(maxDigit)`, then joins with `result.join('')` and converts using `parseInt(result.join(''), 10)`. The implementation is similar to JavaScript but with type safety. String slicing uses `str.slice(i, j)` syntax.

## Key Observations

The most significant observation is that all implementations use the same greedy algorithm, demonstrating that this problem has a clear optimal approach regardless of language paradigm. The greedy strategy is optimal because: (1) at each position, choosing a larger digit always yields a larger final number, and (2) we ensure enough digits remain to complete the sequence, so the choice doesn't constrain future options.

Functional languages (Haskell, Clojure, Elixir) express the greedy selection through recursion and immutable data structures. Haskell's recursive helper function and Clojure's `loop/recur` pattern both accumulate state through parameters rather than mutation. In contrast, imperative languages (C, Go, Java) use straightforward loops with mutable variables, which is more direct but less mathematically elegant.

String building approaches vary interestingly: some languages build incrementally (Python's list, Ruby's array, Rust's Vec), while others use builder patterns (Java's `StringBuilder`, Kotlin's `StringBuilder`, Go's `strings.Builder`). The builder pattern is more efficient for string concatenation in loops, while array/vector building is more idiomatic in functional contexts.

Character comparison is uniform across languages—direct comparison of characters or their integer values works because digits '0'-'9' have consecutive ASCII/Unicode values. However, some languages (Clojure, Rust) require explicit conversion to integers for comparison, while others (Python, Ruby, Go) compare characters directly.

The greedy algorithm's structure—nested loops with a maximum-finding inner loop—is naturally expressed in imperative languages. Functional languages transform this into recursive patterns or higher-order functions (`reduce`, `maximum`), which can be less readable for those unfamiliar with functional paradigms but more mathematically elegant.

Julia's 1-indexing requires careful adjustment in the algorithm, particularly when calculating positions. This is a language-specific quirk that doesn't affect the algorithm's correctness but requires attention during implementation.

The problem's constraint of fixed-length subsequences (2 for Part 1, 12 for Part 2) means the algorithm complexity is effectively O(n × m) for both parts, with the constant factor being the subsequence length. This makes Part 2 only slightly slower than Part 1 in practice, despite selecting 6× more digits.

Memory management differences are minimal here since we process banks one at a time and only use O(1) additional space per bank. However, C's manual management, Rust's ownership system, and garbage-collected languages' automatic management all converge to similar approaches due to the simple memory pattern.

## Notes

Very kludgy.
