# Specification: Day 10

## Problem Statement

The Elves have discovered a factory with machines that need to be repaired. Each machine is controlled by indicator lights and buttons.

## Requirements

### Part 1
- Parse machine descriptions from input file
- Each line describes one machine with:
  - Indicator lights pattern in square brackets `[...]` (`.` = OFF, `#` = ON)
  - Multiple buttons in parentheses `(...)` that toggle specific lights
  - Joltage requirements in curly braces `{...}` (ignored for Part 1)
- All lights start OFF
- Pressing a button toggles all lights it affects (ON ↔ OFF)
- Buttons can be pressed at most once each (pressing twice = no effect)
- Find the minimum number of button presses to reach the target light pattern
- Sum the minimum button presses across all machines

### Part 2
- Same machine description format
- Now each light has a joltage meter (initially 0) with a target joltage value
- Pressing a button increments the joltage meter of each light it affects by 1
- Can press the same button multiple times (no limit)
- Find the minimum total button presses to reach exact target joltage values for all lights
- Note: Pattern matching constraint needs verification - README doesn't explicitly mention it
- Sum the minimum button presses across all machines

## Input Format

Each line contains:
1. Light pattern in square brackets: `[.##.]` where `.` = OFF, `#` = ON
2. One or more buttons in parentheses: `(3)`, `(1,3)`, etc. (lights are 0-indexed)
3. Joltage requirements in curly braces: `{3,5,4,7}` (one value per light, 0-indexed)

Example line:
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

This means:
- 4 lights (indexed 0-3)
- Target pattern: OFF, ON, ON, OFF
- 6 buttons: `(3)`, `(1,3)`, `(2)`, `(2,3)`, `(0,2)`, `(0,1)`
- Joltage targets: 3, 5, 4, 7 (for lights 0, 1, 2, 3 respectively)

## Output Format

For Part 1: A single integer representing the sum of minimum button presses needed across all machines.

For Part 2: A single integer representing the sum of minimum button presses needed across all machines to reach target joltages.

## Constraints

- Lights are 0-indexed
- In Part 1, buttons can be pressed at most once (binary problem over GF(2))
- In Part 2, buttons can be pressed multiple times (integer linear programming problem)
- Joltage values are non-negative integers
- The problem may have multiple solutions; we need the minimum total presses
- Machines are independent; process each line separately and sum results
- Number of lights per machine varies
- Number of buttons per machine varies

## Test Cases

### Test Case 1 (Simple single-button toggle)
**Input** (`test_1.txt`):
```
[#] (0) {1}
```

**Expected Output Part 1**: 1

**Explanation**: 
- 1 light, target is ON
- 1 button `(0)` toggles light 0
- Press button once to turn light ON
- Minimum presses: 1

**Expected Output Part 2**: 1

**Explanation**:
- Target joltage for light 0 is 1
- Press button `(0)` once to get joltage 1
- Joltage 1 is odd → light is ON (matches pattern)
- Minimum presses: 1

### Test Case 2 (Example from README)
**Input** (`test_2.txt`):
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

**Expected Output Part 1**: TBD (will be determined during implementation)

**Explanation**: 
- 4 lights, target pattern: OFF, ON, ON, OFF
- All lights start OFF
- Need to find minimum button combination to toggle lights 1 and 2 ON
- This is a linear system over GF(2) - solve with Gaussian elimination or search

**Expected Output Part 2**: TBD (to be verified)

**Explanation**:
- Target joltages: light 0 = 3, light 1 = 5, light 2 = 4, light 3 = 7
- Need to find minimum button presses to reach these exact values
- Note: Pattern constraint may or may not apply - needs verification
- This is an integer linear programming problem

### Test Case 3 (Two machines)
**Input** (`test_3.txt`):
```
[#] (0) {1}
[..] () {0,0}
```

**Expected Output Part 1**: 1

**Explanation**:
- Machine 1: Press button `(0)` once → 1 press
- Machine 2: Target is all OFF, no buttons → 0 presses
- Total: 1 + 0 = 1

**Expected Output Part 2**: 1

**Explanation**:
- Machine 1: Press button `(0)` once to reach joltage 1 → 1 press
- Machine 2: Target is all 0, no buttons → 0 presses
- Total: 1 + 0 = 1

### Test Case 4 (Impossible pattern - will test edge cases)
**Input** (`test_4.txt`):
```
[##] (0) (1) {2,2}
```

**Expected Output Part 1**: TBD (depends on solvability)

**Explanation**:
- 2 lights, target: both ON
- Buttons: `(0)` toggles light 0, `(1)` toggles light 1
- If both start OFF, press both buttons once → both ON
- Minimum presses: 2

**Expected Output Part 2**: TBD

### Test Case 5 (Multiple buttons affecting same lights)
**Input** (`test_5.txt`):
```
[.#] (0,1) (0) {1,1}
```

**Expected Output Part 1**: 1

**Explanation**:
- 2 lights, target: OFF, ON
- Button `(0,1)` toggles both lights
- Button `(0)` toggles only light 0
- All lights start OFF: press `(0)` once → OFF, ON (matches pattern)
- Minimum presses: 1

**Expected Output Part 2**: 1

**Explanation**:
- Target joltages: light 0 = 1, light 1 = 1
- Button `(0,1)` affects both lights, button `(0)` affects only light 0
- Solution: press button `(0)` once → light 0 gets 1, light 1 gets 0 (from button 0)
- But we need light 1 to also be 1, so we need another press
- Actually: press button `(0,1)` once → both lights get 1
- Total: 1 press

### Test Case 7 (Complex example from README)
**Input** (`test_7.txt`):
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

**Expected Output Part 1**: 2

**Expected Output Part 2**: 10 (to be verified)

**Explanation**:
- 4 lights, target pattern: OFF, ON, ON, OFF
- Target joltages: 3, 5, 4, 7
- This is a complex ILP problem requiring Z3 or matrix elimination

### Test Case 8 (Multiple machines with edge cases)
**Input** (`test_8.txt`):
```
[#] (0) {2}
[.#] (0) (1) {1,1}
```

**Expected Output Part 1**: 2

**Expected Output Part 2**: 3 (to be verified)

**Explanation**:
- Machine 1: Pattern ON, joltage 2 (even) - may be invalid if pattern must match
- Machine 2: Pattern OFF, ON; joltages 1, 1 (both odd) - light 0 should be even for OFF

### Test Case 9 (Simple two-button case)
**Input** (`test_9.txt`):
```
[##] (0) (1) {3,3}
```

**Expected Output Part 1**: 2

**Expected Output Part 2**: 6 (to be verified)

**Explanation**:
- Pattern: ON, ON; joltages 3, 3 (both odd, matches pattern)
- Button 0 affects light 0, button 1 affects light 1
- Solution: press button 0 three times, button 1 three times = 6 total

### Test Case 10 (Overlapping buttons)
**Input** (`test_10.txt`):
```
[.#] (0,1) (0) {2,1}
```

**Expected Output Part 1**: 2

**Expected Output Part 2**: 2 (to be verified)

**Explanation**:
- Pattern: OFF, ON; joltages 2 (even/OFF ✓), 1 (odd/ON ✓)
- Button 0 `(0,1)` affects both lights, button 1 `(0)` affects only light 0
- Let a = presses of button 0, b = presses of button 1
- Light 0: a + b = 2
- Light 1: a = 1
- Solution: a = 1, b = 1, total = 2

### Test Case 11 (Simple single button)
**Input** (`test_11.txt`):
```
[#] (0) {1}
```

**Expected Output Part 1**: 1
**Expected Output Part 2**: 1

### Test Case 12 (Two independent buttons)
**Input** (`test_12.txt`):
```
[##] (0) (1) {1,1}
```

**Expected Output Part 1**: 2
**Expected Output Part 2**: 2

**Explanation**:
- Each button affects one light independently
- Press each button once to get joltage 1 on each light

### Test Case 13 (Same as test 10, for verification)
**Input** (`test_13.txt`):
```
[.#] (0,1) (0) {2,1}
```

**Expected Output Part 1**: 2
**Expected Output Part 2**: 2

### Test Case 6 (Larger example for testing algorithms)
**Input** (`test_6.txt`):
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[#.#] (0) (1) (2) {2,2,2}
```

**Expected Output Part 1**: TBD

**Expected Output Part 2**: TBD

### Test Case 14 (Complex multi-machine test - 10 lines)
**Input** (`test_14.txt`):
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[#.#] (0) (1) (2) {1,1,1}
[##] (0) (1) {2,2}
[.#] (0,1) (0) {1,1}
[#] (0) {1}
[.##] (0) (1) (2) (0,1) {2,2,2}
[#.#.] (0) (1) (2) (3) {1,1,1,1}
[##] (0,1) {2,2}
[.#] (0) (1) {1,1}
[.##.] (0,1) (1,2) (2,3) (0,3) {3,3,3,3}
```

**Expected Output Part 1**: 16

**Expected Output Part 2**: 37

**Explanation**:
- 10 machines with varying complexity
- Mix of simple and complex button configurations
- Tests both Part 1 (GF(2)) and Part 2 (ILP) algorithms
- Line-by-line breakdown:
  - Line 1: P1=2, P2=10 (complex 4-light system)
  - Line 2: P1=2, P2=3 (3 independent buttons)
  - Line 3: P1=2, P2=4 (2 independent buttons, each pressed twice)
  - Line 4: P1=2, P2=1 (overlapping buttons, optimized solution)
  - Line 5: P1=1, P2=1 (single button)
  - Line 6: P1=2, P2=4 (3 lights with overlapping)
  - Line 7: P1=2, P2=4 (4 independent buttons)
  - Line 8: P1=1, P2=2 (single button affecting both lights)
  - Line 9: P1=1, P2=2 (2 independent buttons)
  - Line 10: P1=1, P2=6 (4 lights in cycle, each needs 3)

### Test Case 15 (Uniform joltage patterns - 10 lines)
**Input** (`test_15.txt`):
```
[###] (0) (1) (2) (0,1) (1,2) {5,5,5}
[.##] (0) (1) (2) (0,1,2) {4,4,4}
[#.#] (0) (1) (2) (0,2) {3,3,3}
[##] (0) (1) (0,1) {6,6}
[.#] (0) (1) (0,1) {2,2}
[###] (0,1) (1,2) (0,2) {9,9,9}
[.##] (0) (1) (2) {6,6,6}
[#.#] (0,1) (1,2) {4,4,4}
[##] (0) (1) {8,8}
[.#] (0,1) {2,2}
```

**Expected Output Part 1**: 13

**Expected Output Part 2**: 64

**Explanation**:
- All machines have uniform joltage requirements (same value for all lights)
- Tests optimization when all targets are equal
- Mix of patterns and button configurations
- Tests divide-by-2 optimization when applicable

### Test Case 16 (Varied patterns and joltages - 10 lines)
**Input** (`test_16.txt`):
```
[.##.] (0) (1) (2) (3) (0,1) (1,2) (2,3) (0,3) {2,2,2,2}
[#.#.] (0) (1) (2) (3) (0,2) (1,3) {3,3,3,3}
[.##] (0) (1) (2) (0,1,2) {4,4,4}
[#.#] (0) (1) (2) (0,2) {5,5,5}
[##] (0) (1) (0,1) {6,6}
[.##.] (0,1) (1,2) (2,3) (0,3) {1,1,1,1}
[#.#.] (0,1) (1,2) (2,3) {2,2,2,2}
[.##] (0,1) (1,2) {3,3,3}
[#.#] (0,2) {4,4,4}
[##] (0,1) {5,5}
```

**Expected Output Part 1**: 12

**Expected Output Part 2**: 41

**Explanation**:
- Mix of 2, 3, and 4-light systems
- Uniform joltages within each machine but varying across machines
- Tests various button overlap patterns
- Includes cycle patterns (lines 1, 6)

### Test Case 17 (Increasing joltage complexity - 10 lines)
**Input** (`test_17.txt`):
```
[####] (0) (1) (2) (3) (0,1) (1,2) (2,3) (0,3) {1,1,1,1}
[.###] (0) (1) (2) (3) (0,1,2) (1,2,3) {2,2,2,2}
[##.#] (0) (1) (2) (3) (0,2) (1,3) {3,3,3,3}
[####] (0,1) (1,2) (2,3) (0,3) {4,4,4,4}
[.###] (0,1,2) (1,2,3) (0,3) {5,5,5,5}
[##.#] (0,2) (1,3) (0,1,2,3) {6,6,6,6}
[####] (0) (1) (2) (3) {7,7,7,7}
[.###] (0,1) (1,2) (2,3) {8,8,8,8}
[##.#] (0,3) (1,2) {9,9,9,9}
[####] (0,1,2,3) {10,10,10,10}
```

**Expected Output Part 1**: 13

**Expected Output Part 2**: 98

**Explanation**:
- All 4-light systems with increasing joltage requirements (1 to 10)
- Tests performance with larger joltage values
- Various button configurations from simple to complex
- Tests divide-by-2 optimization on even joltages

### Test Case 18 (Non-uniform joltages - 10 lines)
**Input** (`test_18.txt`):
```
[.##.] (0) (1) (2) (3) (0,1) (1,2) (2,3) (0,3) {1,2,3,4}
[#.#.] (0) (1) (2) (3) (0,2) (1,3) {2,3,4,5}
[.##] (0) (1) (2) (0,1,2) {3,4,5}
[#.#] (0) (1) (2) (0,2) {4,5,6}
[##] (0) (1) (0,1) {5,6}
[.##.] (0,1) (1,2) (2,3) (0,3) {6,7,8,9}
[#.#.] (0,1) (1,2) (2,3) {7,8,9,10}
[.##] (0,1) (1,2) {8,9,10}
[#.#] (0,2) {9,10,11}
[##] (0,1) {10,11}
```

**Expected Output Part 1**: 12

**Expected Output Part 2**: 38

**Explanation**:
- Non-uniform joltage requirements within each machine
- Tests ILP solver with varied constraints
- Increasing complexity across machines
- Tests optimization when targets differ per light
