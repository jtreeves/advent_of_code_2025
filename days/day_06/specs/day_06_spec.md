# Specification: Day 06

## Problem Statement

After helping the Elves in the kitchen, you were taking a break and helping them re-enact a movie scene when you over-enthusiastically jumped into the garbage chute! A brief fall later, you find yourself in a garbage smasher. Unfortunately, the door's been magnetically sealed.

As you try to find a way out, you are approached by a family of cephalopods! They're pretty sure they can get the door open, but it will take some time. While you wait, they're curious if you can help the youngest cephalopod with her math homework.

Cephalopod math has special rules about how numbers are written and read.

## Requirements

### Part 1
- Parse a math worksheet from the input file
- The worksheet consists of multiple problems
- Each problem has numbers arranged horizontally in rows, with an operator row at the bottom
- Problems are separated by columns consisting only of spaces
- For each problem:
  - Read the numbers horizontally from each row (numbers are separated by spaces)
  - Identify the operator from the bottom row (either `+` or `*`)
  - Apply the operator to all the numbers in that problem
- Sum all the problem results
- Return the grand total

### Part 2
- Parse the same math worksheet format
- However, cephalopod math is written **right-to-left in columns**
- Each number is given in its own column, with the most significant digit at the top and the least significant digit at the bottom
- Problems are still separated by columns consisting only of spaces
- For each problem (reading right-to-left):
  - Read digits column by column to reconstruct numbers
  - Each column contains digits vertically (top to bottom = most to least significant)
  - Identify groups of adjacent columns that form numbers
  - Identify the operator from the bottom row of the problem area
  - Apply the operator to all the numbers in that problem
- Sum all the problem results
- Return the grand total

## Input Format

A text file with multiple rows of numbers and operators. The last row contains operators (`+` or `*`). Problems are separated by columns consisting entirely of spaces.

Example:
```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

This example has 4 problems, separated by space columns.

## Output Format

For Part 1: A single integer representing the grand total of all problem results.

For Part 2: A single integer representing the grand total of all problem results (using the right-to-left column reading method).

## Constraints

- Numbers can be multi-digit
- Operators are either `+` or `*`
- Problems are separated by one or more columns of spaces
- Numbers may have leading spaces for alignment (they may not all start at the same column)
- For Part 2, numbers are constructed from columns, with most significant digit at top
- Results can be very large (potentially exceeding 32-bit integer limits) - use appropriate data types
- The input may have ragged lines (different lengths) due to spacing

## Test Cases

### Test Case 1 (Example from README)
**Input** (`test_1.txt`):
```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

**Expected Output Part 1**: 4277556

**Explanation**: 
- Problem 1 (leftmost, operator `*`): 123 * 45 * 6 = 33,210
- Problem 2 (operator `+`): 328 + 64 + 98 = 490
- Problem 3 (operator `*`): 51 * 387 * 215 = 4,243,455
- Problem 4 (rightmost, operator `+`): 64 + 23 + 314 = 401
- Grand total: 33,210 + 490 + 4,243,455 + 401 = 4,277,556

**Expected Output Part 2**: 3263827

**Explanation**: 
According to the README, reading right-to-left column by column:
- Rightmost problem (operator `+`): 4 + 431 + 623 = 1058
- Second from right (operator `*`): 175 * 581 * 32 = 3,253,600
- Third from right (operator `+`): 8 + 248 + 369 = 625
- Leftmost problem (operator `*`): 356 * 24 * 1 = 8,544
- Grand total: 1058 + 3,253,600 + 625 + 8,544 = 3,263,827

**Note**: Part 2 requires careful parsing to identify how columns form numbers within each problem. The exact column grouping rules will be clarified during implementation.

### Test Case 2 (Two problems, two rows)
**Input** (`test_2.txt`):
```
10  5
20  3
*   +
```

**Expected Output Part 1**: 208

**Explanation**:
- Problem 1 (left, operator `*`): 10 * 20 = 200
- Problem 2 (right, operator `+`): 5 + 3 = 8
- Grand total: 200 + 8 = 208

**Expected Output Part 2**: TBD (will be determined during implementation)

### Test Case 3 (Three problems)
**Input** (`test_3.txt`):
```
2  4  6
3  5  7
+  *  +
```

**Expected Output Part 1**: 38

**Explanation**:
- Problem 1 (left, operator `+`): 2 + 3 = 5
- Problem 2 (middle, operator `*`): 4 * 5 = 20
- Problem 3 (right, operator `+`): 6 + 7 = 13
- Grand total: 5 + 20 + 13 = 38

**Expected Output Part 2**: TBD

### Test Case 4 (Single problem, three numbers)
**Input** (`test_4.txt`):
```
1
2
3
+
```

**Expected Output Part 1**: 6

**Explanation**:
- Single problem with operator `+`: 1 + 2 + 3 = 6

**Expected Output Part 2**: TBD

### Test Case 5 (Multiplication problem)
**Input** (`test_5.txt`):
```
2  3
4  5
*   *
```

**Expected Output Part 1**: 23

**Explanation**:
- Problem 1 (left, operator `*`): 2 * 4 = 8
- Problem 2 (right, operator `*`): 3 * 5 = 15
- Grand total: 8 + 15 = 23

**Expected Output Part 2**: TBD
