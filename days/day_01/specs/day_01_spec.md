# Specification: Day 01

## Problem Statement

The Elves have discovered project management, but they've realized they have a different emergency: according to their resource planning, none of them have any time left to decorate the North Pole!

You arrive at the secret entrance to the North Pole base ready to start decorating. Unfortunately, the password seems to have been changed, so you can't get in. A document taped to the wall explains:

"Due to new security protocols, the password is locked in the safe below. Please see the attached document for the new combination."

The safe has a dial with only an arrow on it; around the dial are the numbers 0 through 99 in order. As you turn the dial, it makes a small click noise as it reaches each number.

## Requirements

### Part 1
- Parse a sequence of rotations from the input file, where each rotation starts with L (left/toward lower numbers) or R (right/toward higher numbers) followed by a distance value
- Simulate the dial starting at position 50
- Apply each rotation in sequence (the dial wraps around: 0 wraps to 99 when going left, 99 wraps to 0 when going right)
- Count the number of times the dial is left pointing at 0 after any rotation in the sequence (only check at the end of each rotation, not during the rotation)
- Return this count as the password

### Part 2
- Parse a sequence of rotations from the input file (same format as Part 1)
- Simulate the dial starting at position 50
- Apply each rotation in sequence, but this time count the number of times the dial points at 0 during the entire process
- Count includes both:
  - Times when the dial ends at 0 after completing a rotation
  - Times when the dial passes through 0 during a rotation (e.g., going from 95 to 5 would pass through 0 once; going from 50 to 150 would pass through 0 ten times before wrapping back)
- Return this total count as the password

## Input Format

A text file with one rotation per line. Each line starts with either 'L' or 'R' (case-sensitive), followed immediately by a positive integer representing the distance to rotate. There are no spaces between the direction and the number.

Example:
```
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```

## Output Format

For Part 1: A single integer representing the count of times the dial points at 0 after completing any rotation.

For Part 2: A single integer representing the count of times the dial points at 0 during the entire sequence (both at the end of rotations and during rotations).

## Constraints

- The dial has 100 positions: 0 through 99 (circular)
- The dial starts at position 50
- Rotations wrap around: going left from 0 wraps to 99, going right from 99 wraps to 0
- Each rotation moves the dial exactly the specified distance in the specified direction
- Rotations can be arbitrarily large (e.g., R1000 would pass through 0 multiple times)
- Input file contains one rotation per line, with no empty lines
- All rotation distances are positive integers

## Test Cases

### Test Case 1 (Part 1)
**Input** (`test_1.txt`):
```
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
```
**Expected Output Part 1**: 3
**Explanation**: 
- Start: 50
- L68: 50 → 82 (doesn't end at 0)
- L30: 82 → 52 (doesn't end at 0)
- R48: 52 → 0 (ends at 0, count = 1)
- L5: 0 → 95 (doesn't end at 0)
- R60: 95 → 55 (doesn't end at 0)
- L55: 55 → 0 (ends at 0, count = 2)
- L1: 0 → 99 (doesn't end at 0)
- L99: 99 → 0 (ends at 0, count = 3)
- R14: 0 → 14 (doesn't end at 0)
- L82: 14 → 32 (doesn't end at 0)
- Total: 3

**Expected Output Part 2**: 6
**Explanation**:
- Start: 50
- L68: 50 → 82, passes through 0 once during rotation (count = 1)
- L30: 82 → 52, doesn't pass through 0
- R48: 52 → 0, ends at 0 (count = 2)
- L5: 0 → 95, doesn't pass through 0 again
- R60: 95 → 55, passes through 0 once during rotation (count = 3)
- L55: 55 → 0, ends at 0 (count = 4)
- L1: 0 → 99, doesn't pass through 0 again
- L99: 99 → 0, ends at 0 (count = 5)
- R14: 0 → 14, doesn't pass through 0 again
- L82: 14 → 32, passes through 0 once during rotation (count = 6)
- Total: 6

### Test Case 2 (Large rotation)
**Input** (`test_2.txt`):
```
R1000
```
**Expected Output Part 1**: 0
**Explanation**: Starting at 50, R1000: 50 → 50 (ends at 50, not 0). However, during the rotation it passes through 0 ten times.

**Expected Output Part 2**: 10
**Explanation**: Starting at 50, R1000 means: 50 → 51 → ... → 99 → 0 (1st) → 1 → ... → 0 (2nd) → ... → 0 (10th) → ... → 50. It passes through 0 exactly 10 times during the rotation.

### Test Case 3 (Single rotation to 0)
**Input** (`test_3.txt`):
```
L50
```
**Expected Output Part 1**: 1
**Explanation**: Start at 50, L50: 50 → 0 (ends at 0).

**Expected Output Part 2**: 1
**Explanation**: Same as Part 1 for this case.

### Test Case 4 (No zeros)
**Input** (`test_4.txt`):
```
R10
R20
R30
```
**Expected Output Part 1**: 0
**Explanation**: Start at 50, R10: 50 → 60, R20: 60 → 80, R30: 80 → 10. Never ends at 0.

**Expected Output Part 2**: 0
**Explanation**: Same as Part 1 - no zeros encountered during or at end of rotations.

### Test Case 5 (Multiple passes through 0)
**Input** (`test_5.txt`):
```
L95
R190
```
**Expected Output Part 1**: 1
**Explanation**: Start at 50, L95: 50 → 55 (doesn't end at 0), R190: 55 → 45 (doesn't end at 0).

**Expected Output Part 2**: 3
**Explanation**: 
- Start at 50
- L95: 50 → 55, passes through 0 once during rotation (count = 1)
- R190: 55 → 45, passes through 0 twice during rotation (wraps from 99 to 0, then 0 to 0 again) (count = 3)
- Total: 3