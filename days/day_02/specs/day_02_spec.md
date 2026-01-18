# Specification: Day 02

## Problem Statement

You have a single line of input consisting of ranges of product IDs. The format is:

```
start1-end1, start2-end2, start3-end3, …
```

Each range `a-b` represents all integer IDs from `a` through `b`, inclusive. IDs are decimal numbers with no leading zeros.

An ID is **invalid** per certain digit‐pattern rules. You need to find _all_ invalid IDs within those ranges and **sum them up**.

## Requirements

### Part 1
- Parse input ranges in the format `start-end, start-end, ...`
- For each integer `n` in each range:
  - Convert `n` to string (or otherwise examine digits)
  - An ID is invalid if **exactly two** identical digit‐sequences repeat (i.e., the entire string consists of one sequence repeated twice)
  - Valid invalid patterns: `55` → "5" twice; `6464` → "64" twice; `123123` → "123" twice
  - Patterns like `111` ("1" thrice), or `1212` are *not* invalid under Part 1
- Sum all invalid IDs and return the sum

### Part 2
- Parse input ranges (same format as Part 1)
- For each integer `n` in each range:
  - Convert `n` to string
  - An ID is invalid if its digits consist of **some sequence repeated two or more times**, so that it covers the whole ID
  - This generalizes Part 1: includes Part 1 invalids, plus those where the pattern repeats 3+ times
  - Examples: `111` ("1" × 3) is invalid; `123123123` ("123" × 3) is invalid
- Sum all invalid IDs and return the sum

## Input Format

A single line containing comma-separated ranges, where each range is in the format `start-end`. Ranges may be separated by spaces or not.

Example:
```
55-55, 100-200, 1212-1212
```

Or:
```
1-100,200-300,1000-1005
```

## Output Format

For Part 1: A single integer representing the sum of all invalid IDs (where invalid means exactly two identical sequences).

For Part 2: A single integer representing the sum of all invalid IDs (where invalid means sequence repeated 2+ times).

## Constraints

- IDs have **no leading zeros**. Inputs won't contain something like `0101`.
- Ranges may include very large numbers (potentially exceeding 32-bit integer limits), so use types that support large numbers.
- IDs are positive integers.
- Ranges are inclusive on both ends.

## Test Cases

### Test Case 1 (Basic examples)
**Input** (`test_1.txt`):
```
55-55, 6464-6464, 1212-1212
```
**Expected Output Part 1**: 7116 (55 + 6464 = 6519, wait... let me recalculate: 55 + 6464 = 6519, but 1212 is NOT invalid for Part 1)
**Correct calculation**:
- 55: invalid (5 twice) → 55
- 6464: invalid (64 twice) → 6464  
- 1212: NOT invalid (would require "12" twice, but pattern is "12", "12" which matches - wait, let me check: "1212" length 4, half = 2, first half = "12", second half = "12" → YES, 1212 IS invalid for Part 1!

So: 55 + 6464 + 1212 = 7731

**Expected Output Part 2**: 7731 (all three are invalid: 55, 6464, and 1212)

**Explanation**:
- `55`: consists of "5" repeated twice → invalid in both parts
- `6464`: consists of "64" repeated twice → invalid in both parts
- `1212`: consists of "12" repeated twice → invalid in both parts

### Test Case 2 (Part 1 vs Part 2 difference)
**Input** (`test_2.txt`):
```
111-111, 123123123-123123123
```
**Expected Output Part 1**: 0
**Explanation**: 
- `111`: length 3, not even, so cannot be exactly two sequences → NOT invalid for Part 1
- `123123123`: length 9, half would be 4.5, not integer → NOT invalid for Part 1 (would need to be exactly 2 sequences)

**Expected Output Part 2**: 123234234
**Explanation**:
- `111`: consists of "1" repeated 3 times → invalid for Part 2
- `123123123`: consists of "123" repeated 3 times → invalid for Part 2

### Test Case 3 (Mixed valid and invalid)
**Input** (`test_3.txt`):
```
10-15, 55-55, 100-105
```
**Expected Output Part 1**: 66
**Explanation**:
- Range 10-15: 11 is invalid ("1" repeated twice) → 11
- Range 55-55: 55 is invalid ("5" repeated twice) → 55
- Range 100-105: none are invalid
- Total: 11 + 55 = 66

**Expected Output Part 2**: 66
**Explanation**:
- Range 10-15: 11 is invalid ("1" repeated twice) → 11
- Range 55-55: 55 is invalid ("5" repeated twice) → 55
- Range 100-105: none are invalid
- Total: 11 + 55 = 66

### Test Case 4 (Large range)
**Input** (`test_4.txt`):
```
1000-1010
```
**Expected Output Part 1**: 0
**Explanation**: None of these numbers consist of exactly two identical sequences.

**Expected Output Part 2**: 0
**Explanation**: None of these numbers consist of a sequence repeated 2+ times.

### Test Case 5 (Complex pattern)
**Input** (`test_5.txt`):
```
1-20
```
**Expected Output Part 1**: 33
**Explanation**:
- 11: invalid ("1" repeated twice) → 11
- 22: invalid ("2" repeated twice) → 22
- Others (1-10, 12-20): none match exactly two sequences pattern
- Total: 11 + 22 = 33

**Expected Output Part 2**: 33
**Explanation**:
- 11: "1" × 2 → invalid
- 22: "2" × 2 → invalid
- Others (1-10, 12-20): none match repeat patterns
- Total: 11 + 22 = 33
