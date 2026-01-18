# Specification: Day 05

## Problem Statement

The Elves are working on a cafeteria system for the North Pole base. They need to track which ingredients are fresh based on ingredient ID ranges.

## Requirements

### Part 1
- Parse input file with two sections separated by a blank line:
  1. Fresh ingredient ID ranges (one range per line, format: `start-end`)
  2. Specific ingredient IDs to check (one ID per line)
- For each specific ingredient ID, check if it falls within **any** of the fresh ranges
- Ranges are inclusive on both ends (e.g., `3-5` means IDs 3, 4, and 5 are all fresh)
- Count how many of the specific IDs are fresh (i.e., fall into at least one range)
- Return the count

### Part 2
- Parse input file with the same format as Part 1
- Find the **union** of all fresh ranges by merging overlapping and adjacent ranges
- Count the total number of unique ingredient IDs covered by all merged ranges
- Algorithm:
  1. Sort all ranges by their start value
  2. Walk through sorted ranges, merging overlapping or adjacent ranges
  3. Sum the lengths of all merged, non-overlapping intervals
- Return the total count of unique IDs covered

## Input Format

A text file with two sections separated by a blank line:

1. **Fresh ingredient ID ranges**: One range per line in format `start-end` (e.g., `3-5`, `10-14`)
2. **Specific ingredient IDs**: One ID per line (integer values)

Example:
```
3-5
10-14
12-18
16-20

1
5
8
11
17
32
```

The first section contains ranges, then a blank line, then IDs to check.

## Output Format

For Part 1: A single integer representing the count of specific ingredient IDs that fall into at least one fresh range.

For Part 2: A single integer representing the total number of unique ingredient IDs covered by all fresh ranges (after merging overlaps).

## Constraints

- Ranges are inclusive on both ends (both start and end are included in the range)
- Ranges may overlap or be adjacent
- Ingredient IDs are non-negative integers
- Ranges and IDs can be very large (potentially exceeding 32-bit integer limits)
- For Part 2, two ranges overlap if one contains any ID from the other
- Two ranges are adjacent if the end of one equals the start of the other minus 1 (e.g., `10-14` and `15-20` should merge to `10-20`)
- Be careful with off-by-one errors when calculating interval lengths (length = end - start + 1)

## Test Cases

### Test Case 1 (Basic example from README)
**Input** (`test_1.txt`):
```
3-5
10-14
12-18
16-20

1
5
8
11
17
32
```

**Expected Output Part 1**: 3

**Explanation**: 
- ID `1` is not in any range
- ID `5` is in range `3-5` ✓
- ID `8` is not in any range
- ID `11` is in range `10-14` ✓
- ID `17` is in range `12-18` (also in `16-20`) ✓
- ID `32` is not in any range
- Total: 3 IDs are fresh

**Expected Output Part 2**: 14

**Explanation**: 
- Original ranges: `3-5`, `10-14`, `12-18`, `16-20`
- `10-14` and `12-18` overlap (12, 13, 14 are in both)
- `12-18` and `16-20` overlap (16, 17, 18 are in both)
- Merged ranges: `3-5` and `10-20`
- `3-5` covers 3 IDs (3, 4, 5) = length 3
- `10-20` covers 11 IDs (10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20) = length 11
- Total: 3 + 11 = 14 unique IDs

### Test Case 2 (Non-overlapping ranges)
**Input** (`test_2.txt`):
```
1-5
10-15
20-25

3
8
12
22
30
```

**Expected Output Part 1**: 3

**Explanation**:
- ID `3` is in range `1-5` ✓
- ID `8` is not in any range
- ID `12` is in range `10-15` ✓
- ID `22` is in range `20-25` ✓
- ID `30` is not in any range
- Total: 3

**Expected Output Part 2**: 17

**Explanation**:
- All ranges are non-overlapping and non-adjacent
- `1-5`: length 5
- `10-15`: length 6
- `20-25`: length 6
- Total: 5 + 6 + 6 = 17

### Test Case 3 (Adjacent ranges)
**Input** (`test_3.txt`):
```
1-5
6-10
11-15

7
14
```

**Expected Output Part 1**: 2

**Explanation**:
- ID `7` is in range `6-10` ✓
- ID `14` is in range `11-15` ✓
- Total: 2

**Expected Output Part 2**: 15

**Explanation**:
- Ranges `1-5`, `6-10`, `11-15` are adjacent
- Merged: `1-15` (since 5 and 6 are adjacent, 10 and 11 are adjacent)
- Length: 15 - 1 + 1 = 15

### Test Case 4 (Single range, multiple overlaps)
**Input** (`test_4.txt`):
```
10-20
12-18
14-16

15
25
```

**Expected Output Part 1**: 1

**Explanation**:
- ID `15` is in all three ranges ✓
- ID `25` is not in any range
- Total: 1

**Expected Output Part 2**: 11

**Explanation**:
- All three ranges overlap and merge to `10-20`
- Length: 20 - 10 + 1 = 11

### Test Case 5 (Single-element range)
**Input** (`test_5.txt`):
```
5-5
10-15

5
7
12
```

**Expected Output Part 1**: 2

**Explanation**:
- ID `5` is in range `5-5` ✓
- ID `7` is not in any range
- ID `12` is in range `10-15` ✓
- Total: 2

**Expected Output Part 2**: 7

**Explanation**:
- Range `5-5`: length 1
- Range `10-15`: length 6
- Total: 1 + 6 = 7
