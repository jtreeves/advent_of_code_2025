# Analysis for Day 06

## Overall Approach

The problem requires parsing a math worksheet with cephalopod math notation. For Part 1, numbers are read horizontally in rows, with problems separated by space columns. For Part 2, numbers are read vertically in columns (right-to-left), with digits stacked (most significant at top). Each problem has an operator (`+` or `*`) applied to all its numbers, and we sum all problem results.

**Clues**

- The problem states problems are "separated with a column consisting only of spaces" - indicating we need column-based parsing
- Part 2 specifies "right-to-left in columns" and "most significant digit at the top" - indicating a different parsing direction and digit ordering
- The example shows problems with varying numbers of numbers per problem - requiring flexible parsing

## Complexity Analysis

- **Time Complexity**: O(n × m) where n = number of rows, m = number of columns - need to scan all characters to identify problems and parse numbers
- **Space Complexity**: O(n × m) for storing the grid/input, plus O(k) for storing problems and results where k = number of problems

## Part 2 Implementation Challenges

**Part 2 proved significantly more difficult than Part 1**, requiring a fundamentally different parsing strategy. The challenge involved:

1. Understanding that numbers must be read **vertically** (columns) instead of horizontally (rows)
2. Processing columns **right-to-left** within each problem
3. Handling digits stacked **top-to-bottom** (most significant at top)
4. Identifying the correct algorithm after the initial complex column-grouping approach failed

The original Python implementation attempted a sophisticated column grouping strategy that produced an incorrect result (`992008`). All languages now use a simpler, correct approach: **transpose columns to rows, remove spaces, parse as integers, reverse for right-to-left reading, then apply operators**. This simpler approach successfully converged to the correct answer (`8843673199391`) across all 12 languages.

### Common Strategy

All Part 2 implementations follow the same core algorithm but use language-idiomatic patterns:

1. **Transpose columns to rows**: Extract digit strings from each column (top-to-bottom)
2. **Remove spaces**: Filter out spaces from column strings
3. **Parse numbers**: Convert column strings to integers
4. **Reverse order**: Process columns right-to-left (reverse the list)
5. **Apply operators**: Sum or multiply the parsed numbers

## Implementation Differences

| Language   | Lines | Execution Time (ms)         |
|------------|-------|-----------------------------|
| C          | 313   | 197                         |
| Clojure    | 91    | 618                         |
| Elixir     | 129   | 1006                        |
| Go         | 185   | 376                         |
| Haskell    | 114   | 552                         |
| Java       | 156   | 103                         |
| Julia      | 132   | 559                         |
| Perl       | 151   | 63                          |
| Python     | 119   | 55                          |
| Ruby       | 114   | 98                          |
| Rust       | 151   | 470                         |
| TypeScript | 144   | 1458                        |

### C
- **Part 1**: Manual memory management with pointers; uses `malloc`/`realloc` for dynamic arrays
- **Part 2**: Manual column extraction with explicit memory management; uses `strtoll` for parsing; builds column strings character-by-character

### Clojure
- **Part 1**: Immutable data structures with functional transformations
- **Part 2**: Uses `->>` threading macro for elegant pipeline processing; `range`, `remove`, `map`, `filter`, `reverse` in a pure functional style

### Elixir
- **Part 1**: Pattern matching and Enum functions
- **Part 2**: Leverages `|>` pipe operator extensively; uses `Enum.reject`, `Enum.map`, `Enum.reverse`, `Enum.sum`/`Enum.product` for concise code

### Go
- **Part 1**: Explicit string operations and manual iteration
- **Part 2**: Builds transposed representation explicitly; uses `strings.TrimSpace()` for cleaning; manual iteration with range loops

### Haskell
- **Part 1**: Pure functional style with list comprehensions
- **Part 2**: Separate `solveProblemPart2` function using list comprehensions, `filter`, `map`, `mapMaybe`, `parseInteger`; maintains pure functional approach

### Java
- **Part 1**: Object-oriented approach with Collections
- **Part 2**: Uses Java streams extensively; `stream()`, `collect()`, `Collections.reverse()`, `replaceAll()`, `mapToLong()` for sums

### Julia
- **Part 1**: Array operations and broadcasting
- **Part 2**: Array indexing `[row[col] for row in num_rows]` for transpose; `filter(isdigit)`, `reverse()`, `sum`/`prod` for operations

### Perl
- **Part 1**: Perl uses `split /\n/` to split input into lines, then finds maximum line length with `length($line)` and pads lines with string repetition: `$_ . (' ' x ($max_len - length($_)))`. Array slicing extracts subsets: `my @num_rows = @padded_lines[0..$op_row_idx-1]` and uses `$#padded_lines` for the last index. Column grouping identifies space columns by iterating through each column and checking all lines with `substr($line, $col, 1) ne ' '`, storing results in a boolean array `@is_space_col`. Numbers are parsed horizontally by extracting substrings with `substr()` and converting with `int()`. Perl uses idiomatic accumulation: `$result += $_ for @numbers` which is more concise than explicit loops.
- **Part 2**: Perl transposes columns to rows by building column strings character-by-character using string concatenation `$col_str .= $char`. Filters out spaces using regex substitution `$col_str =~ s/^\s+|\s+$//g` and extracts digits using `join('', grep { /\d/ } split //, $col_str)`. Parses column strings as integers, then reverses the array with `reverse(@col_strings)`. Applies operators using the same idiomatic accumulation pattern: `$result += $_ for @numbers_vert` for sums and `$result *= $_ for @numbers_vert` for products.

### Python
- **Part 1**: List comprehensions and type hints
- **Part 2**: List comprehensions for transpose; `''.join()` for string building, `[::-1]` for reversal, `c.isdigit()` filtering; initially attempted complex column grouping (incorrect)

### Ruby
- **Part 1**: Dynamic typing and blocks
- **Part 2**: Elegant array method chaining; uses range `(start...end)`, `reject`, `map`, `join`, `gsub`, `reverse`; `sum` and `reduce` for operators

### Rust
- **Part 1**: Ownership, borrowing, pattern matching
- **Part 2**: Iterator chains with functional style; uses `iter()`, `filter_map()`, `collect()`, `rev()`; iterator-based parsing throughout

### TypeScript
- **Part 1**: Strong typing and array methods
- **Part 2**: Array functional methods with `filter()`, `map()`, `reduce()` chains; regex for digit filtering

## Key Observations

[Notable differences, trade-offs, or interesting aspects across implementations, with emphasis on how language paradigms necessitating such differences]

## Notes

[To be filled by dev personally]
