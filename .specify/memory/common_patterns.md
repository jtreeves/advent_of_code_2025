# Common Patterns

## Reusable Patterns Across Implementations

This document captures patterns and approaches that have been successful across multiple days and can be applied to future problems.

## Input Reading Pattern

### Pattern
All solutions should use utility functions to read input:
- `get_input()` or `read_input()` - Reads from `input.txt` or specified test file
- Supports both official input (`input.txt`) and test cases (`test_1.txt`, `test_2.txt`, etc.)
- Returns data in appropriate format (lines, raw string, parsed structure)

### Implementation Guidelines
- Utility functions should be in language-specific utility files
- Solutions should NOT have inline file reading code
- Error handling should be consistent across languages

## Two-Part Problem Pattern

### Pattern
Most AOC problems have Part 1 and Part 2:
- Part 1: Initial problem statement
- Part 2: Extension or modification of Part 1

### Implementation Approach
1. Read input once
2. Solve Part 1
3. Solve Part 2 (may reuse Part 1 solution or require new approach)
4. Output both answers in consistent format: `Part 1: <answer>\nPart 2: <answer>`

## Parsing Patterns

### Common Input Types
- **Line-by-line**: One data point per line
- **Grid/Matrix**: Multi-line grid of characters or numbers
- **Structured**: JSON, key-value pairs, or delimited data
- **Blank-separated groups**: Multiple data points separated by blank lines

### Parsing Utilities
Each language should have utilities for:
- Parsing integers from strings/lines
- Splitting lines into fields
- Converting between string and numeric types
- Handling edge cases (empty lines, whitespace)

## Algorithm Patterns

### Frequently Used Algorithms
- **Graph traversal**: BFS, DFS for pathfinding and search
- **Dynamic programming**: Memoization for optimization
- **String manipulation**: Pattern matching, parsing
- **Set operations**: Union, intersection, difference
- **Mathematical operations**: GCD, LCM, modular arithmetic

## Testing Pattern

### Test-Driven Approach
1. Start with example/test cases from problem description
2. Create `test_1.txt`, `test_2.txt`, etc. with known and generated examples
3. Verify solutions work with test cases before running on full input
4. Test inputs are committed to git (unlike user-specific `input.txt`)

## Code Organization Pattern

### Solution Structure
```python
# Example structure (language-specific syntax)
def solve_part1(input_data):
    # Part 1 logic
    pass

def solve_part2(input_data):
    # Part 2 logic
    pass

def main():
    from utilities.language import get_input
    input_data = get_input()
    part1 = solve_part1(input_data)
    part2 = solve_part2(input_data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
```

## Error Handling Pattern

- Fail fast on invalid input
- Provide clear error messages
- Handle edge cases (empty input, single line, etc.)
- Use language-appropriate error handling mechanisms

## Performance Optimization Pattern

- Profile if needed, but prioritize correctness and readability
- Use appropriate data structures (sets for lookups, lists for iteration)
- Consider time/space complexity trade-offs
- Do not shy away from premature optimization since the second part will usually require such optimizations
