# Technical Plan: Day 06

## Algorithms and Data Structures

### Algorithm Overview

The problem requires parsing a math worksheet with cephalopod math notation and solving multiple arithmetic problems.

**Part 1**: Read numbers horizontally in rows, identify problems separated by space columns, apply operators, and sum all problem results.

**Part 2**: Read numbers vertically in columns (right-to-left), with digits stacked (most significant at top), identify problems, apply operators, and sum all results.

### Key Algorithms

1. **Input Parsing**:
   - Read all lines from input file
   - Identify the operator row (last row containing `+` or `*`)
   - Split input into number rows (all rows except the last) and operator row
   - For Part 1: Parse numbers horizontally from each row
   - For Part 2: Parse numbers vertically from columns, reading right-to-left

2. **Problem Identification (Part 1)**:
   - Detect problem boundaries by finding columns that are entirely spaces
   - Group adjacent non-space columns into problems
   - Extract numbers from each row within each problem group
   - Extract operator from the bottom row for each problem

3. **Problem Identification (Part 2)**:
   - Detect problem boundaries by finding columns that are entirely spaces
   - Within each problem, read columns right-to-left
   - Group adjacent digit columns to form numbers
   - Digits in a column are read top-to-bottom (most significant to least significant)
   - Extract operator from the bottom row for each problem

4. **Problem Solving**:
   - For each problem:
     - Apply the operator (either `+` or `*`) to all numbers in that problem
     - Store the result
   - Sum all problem results
   - Return the grand total

### Data Structures

- **Input lines**: Array/list of strings (one per line)
- **Grid representation**: 2D array/char grid OR column-wise processing
- **Problems**: List of problem structures containing:
  - List of numbers (integers)
  - Operator (character: `+` or `*`)
- **Results**: List of integers (one per problem), then sum them

### Why This Approach

- **Part 1**: Straightforward horizontal parsing is the most direct approach
- **Part 2**: Column-wise parsing is required by the problem specification. Right-to-left reading with digit stacking is the core challenge.

### Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(n * m) where n = number of rows, m = number of columns - need to scan all characters
  - Part 2: O(n * m) - similar scanning, but with column-wise parsing
- **Space Complexity**: O(n * m) for storing the grid/input, plus O(k) for storing problems and results where k = number of problems

## Approach

### Part 1

1. **Read and Parse Input**:
   - Read all lines into an array
   - Identify operator row (last row)
   - Pad all lines to same length for consistent column access

2. **Identify Problems**:
   - Scan each column to find space-only columns (problem separators)
   - Group consecutive non-space columns into problems
   - For each problem:
     - Extract numbers by splitting on spaces horizontally within each row
     - Filter out empty strings
     - Extract operator from bottom row

3. **Solve Problems**:
   - For each problem, apply operator to all numbers
   - Sum all problem results
   - Return total

### Part 2

1. **Read and Parse Input**:
   - Same as Part 1

2. **Identify Problems**:
   - Scan columns to find space-only columns (problem separators)
   - Group consecutive non-space columns into problems

3. **Extract Numbers Column-Wise**:
   - For each problem (reading right-to-left):
     - Process columns from right to left
     - Within each column, read digits top-to-bottom
     - Group consecutive digit columns to form multi-digit numbers
     - Identify where number boundaries occur (transitions from digit columns to space columns, or operator column)

4. **Solve Problems**:
   - Same as Part 1

## Implementation Notes

- Handle ragged lines (different lengths) - pad with spaces to maximum length
- Be careful with leading spaces in numbers
- For Part 2, identifying number boundaries within a problem may require careful parsing logic
- Use appropriate integer types (potentially long/int64) to handle large results
- The exact rules for how columns form numbers in Part 2 will need to be clarified during implementation based on the example

## Edge Cases

- Single problem
- Single number per problem
- Very large numbers (potential overflow)
- Problems with many numbers
- Mixed operators (`+` and `*` in different problems)
