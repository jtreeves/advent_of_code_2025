# Specification: Day 07

## Problem Statement

You thank the cephalopods for the help and exit the trash compactor, finding yourself in the familiar halls of a North Pole research wing. Based on the large sign that says "teleporter hub", they seem to be researching teleportation; you can't help but try it for yourself and step onto the large yellow teleporter pad.

Suddenly, you find yourself in an unfamiliar room! The room has no doors; the only way out is the teleporter. Unfortunately, the teleporter seems to be leaking magic smoke. You quickly locate a diagram of the tachyon manifold (your puzzle input). A tachyon beam enters the manifold at the location marked `S`; tachyon beams always move downward. Tachyon beams pass freely through empty space (`.`). However, if a tachyon beam encounters a splitter (`^`), the beam is stopped; instead, a new tachyon beam continues from the immediate left and from the immediate right of the splitter.

## Requirements

### Part 1
- Parse a grid containing a starting position (`S`), empty cells (`.`), and splitters (`^`)
- Trace tachyon beam from `S` moving downward
- When a beam hits a splitter (`^`), it stops and two new beams are emitted from the cells immediately left and right of the splitter
- Count the total number of times a beam is split (i.e., how many splitters are encountered)
- Return the total split count

### Part 2
- Parse the same grid format as Part 1
- Trace all tachyon beams following the same splitting rules
- Track how many distinct beams reach any cell on the bottom row of the grid
- Beams can merge (multiple beams can reach the same cell, and each should be counted)
- Return the total number of beams that reach the bottom row

## Input Format

A text file containing a 2D grid of characters:
- `S` - starting position (exactly one)
- `.` - empty space (beams pass through)
- `^` - splitter (beams stop here and split into left and right)

Each line is a row of the grid. All rows have the same length (rectangular grid).

Example:
```
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
```

## Output Format

For Part 1: A single integer representing the total number of times beams are split.

For Part 2: A single integer representing the total number of beams that reach the bottom row.

## Constraints

- Grid is rectangular (all rows same length)
- Exactly one `S` starting position
- Beams always move downward
- When a beam hits a splitter, it stops and two new beams start from the cells immediately left and right of the splitter
- Beams can merge (multiple beams can occupy the same cell)
- The grid can be large (up to ~150 rows based on input)
- For Part 2, we need to track beam counts per cell (since beams can merge)

## Test Cases

### Test Case 1 (Example from README)
**Input** (`test_1.txt`):
```
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
```

**Expected Output Part 1**: 21

**Explanation**: This is the exact example from the problem description. The beams split 21 times total as they propagate through the grid.

**Expected Output Part 2**: Count of beams reaching bottom row (exact value TBD based on implementation)

### Test Case 2 (Simple single splitter)
**Input** (`test_2.txt`):
```
S
.
^
.
.
```

**Expected Output Part 1**: 1

**Explanation**: The beam starts at S, moves down, hits one splitter. Total splits = 1.

**Expected Output Part 2**: 2

**Explanation**: One split creates two beams, both reach the bottom row. So 2 beams reach the bottom.

### Test Case 3 (No splitters)
**Input** (`test_3.txt`):
```
S
.
.
.
.
```

**Expected Output Part 1**: 0

**Explanation**: No splitters, so no splits occur.

**Expected Output Part 2**: 1

**Explanation**: Single beam travels straight down and reaches the bottom row.

### Test Case 4 (Two splitters in a row)
**Input** (`test_4.txt`):
```
...S...
.......
...^...
.......
...^...
.......
.......
```

**Expected Output Part 1**: 3

**Explanation**: 
- Beam starts at S (column 3)
- Hits first splitter at row 2, column 3 → splits into columns 2 and 4 (2 splits)
- Each beam continues down
- Left beam (column 2) hits splitter at row 4, column 3? No, wait - the splitter at row 4 is also at column 3. Let me reconsider...
- Actually: First split creates beams at columns 2 and 4. Those beams continue down. One might hit the second splitter. This depends on exact positioning. For simplicity, let's say:
- Beam at S → hits splitter → creates beams at columns 2 and 4
- One of those beams hits the second splitter → splits again
- Total splits: 1 + 1 + 1 = 3 (first split plus two new splits)

**Expected Output Part 2**: TBD

### Test Case 5 (Splitters that cause beams to merge)
**Input** (`test_5.txt`):
```
S
.
^
.
.
^
.
.
```

**Expected Output Part 1**: 3

**Explanation**: Multiple splits occur, and some resulting beams may merge.

**Expected Output Part 2**: TBD (count of beams at bottom)

### Test Case 6 (Wider grid with multiple paths)
**Input** (`test_6.txt`):
```
....S....
.........
....^....
.........
..^...^..
.........
.^.....^.
.........
.........
```

**Expected Output Part 1**: TBD (count all splits)

**Expected Output Part 2**: TBD (count beams at bottom)
