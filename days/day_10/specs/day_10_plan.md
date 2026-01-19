# Technical Plan: Day 10

## Algorithms and Data Structures

### Algorithm Overview

The problem requires solving linear systems to find minimum button presses for machines with indicator lights.

**Part 1**: Binary linear system over GF(2) - find minimum button presses (0 or 1 press per button) to toggle lights to match target pattern.

**Part 2**: Integer linear programming - find minimum total button presses (non-negative integers) to reach exact target joltage values.

### Key Algorithms

1. **Input Parsing**:
   - Read each line from input file
   - Extract light pattern from `[...]` (parse `.` = OFF/0, `#` = ON/1)
   - Extract buttons from `(...)` sequences (parse as sets of light indices)
   - Extract joltage requirements from `{...}` (parse as list of integers)

2. **Part 1: Binary Linear System (GF(2))**:
   - Represent each button as a bitmask vector over GF(2)
   - Target pattern is also a bitmask vector
   - Solve: find minimum-weight solution to `Ax = b` over GF(2)
     - Where A is the button-to-light incidence matrix
     - b is the target pattern (1 = needs toggle, 0 = no toggle needed)
     - x is the button press vector (0 or 1 for each button)
   - Methods:
     - **Gaussian elimination mod 2**: Convert to RREF, count non-zero variables
     - **Bitmask BFS/BFS**: Search over all possible button combinations (feasible for small problems)
     - **Z3 or other solvers**: Use constraint solver (overkill for Part 1)
   - Minimize: count number of 1s in solution vector

3. **Part 2: Integer Linear Programming**:
   - Represent each button press as incrementing counters on affected lights
   - Target: exact joltage values for each light
   - Solve: find minimum-total solution to `Ax = b` where:
     - A is the button-to-light incidence matrix
     - b is the target joltage vector
     - x is the button press count vector (non-negative integers)
   - Methods:
     - **Integer Linear Programming (ILP)**: Use Z3, PuLP, or custom ILP solver
     - **Brute force with pruning**: For small problems only
     - **Hermite Normal Form**: For systems with structure
     - **Divide-and-conquer**: Reduce problem size when all targets are even
     - **GCD/Diophantine approach**: Some special cases can use number theory

### Data Structures

- **Machine representation**:
  - `num_lights`: Number of lights (integer)
  - `target_pattern`: Bitmask or boolean array (Part 1)
  - `target_joltages`: Array of integers (Part 2)
  - `buttons`: List of button masks (each button is set of light indices)
- **Button matrix**: `buttons × lights` incidence matrix
- **Solution vector**: Integer array for button press counts

### Why This Approach

- **Part 1**: GF(2) linear algebra is the natural mathematical framework for toggle problems. Gaussian elimination mod 2 is efficient and guarantees minimal solution.
- **Part 2**: ILP is the correct framework. Real-world inputs may require specialized solvers, but smaller instances can be solved with optimized search.

### Complexity Analysis

- **Time Complexity**: 
  - Part 1: O(b³ + b·l) where b = buttons, l = lights (Gaussian elimination mod 2)
  - Part 2: O(2^b) in worst case (brute force), or polynomial with ILP solvers (but ILP is NP-hard in general)
- **Space Complexity**: O(b·l) for button matrix, plus O(b) for solution vector

## Approach

### Part 1

1. **Parse Input**:
   - For each line, extract pattern, buttons, joltages
   - Build button incidence matrix

2. **Determine Required Toggles**:
   - Start state: all lights OFF (all 0s)
   - Target state: parse pattern from `[...]`
   - Required toggles: XOR of start and target (1 where state needs to change)

3. **Solve Linear System over GF(2)**:
   - Build matrix A where A[i][j] = 1 if button i toggles light j, else 0
   - Solve Ax = b where b is required toggle vector
   - Use Gaussian elimination mod 2:
     - Convert to reduced row echelon form
     - Count non-zero pivot variables as minimum presses
     - Handle free variables (multiple solutions) by choosing 0 for free vars

4. **Sum Results**:
   - For each machine, record minimum presses
   - Sum across all machines

### Part 2

1. **Parse Input** (same as Part 1):
   - Extract joltage requirements from `{...}`

2. **Set Up Integer Linear Program**:
   - Build matrix A where A[i][j] = 1 if button i affects light j, else 0
   - Solve Ax = b where:
     - b is target joltage vector
     - x is button press count vector (non-negative integers)
     - Minimize sum(x)

3. **Solve ILP**:
   - Method 1: Use Z3/PuLP solver if available
   - Method 2: Bounded search with pruning
   - Method 3: Special case optimizations (e.g., if all targets even, divide by 2)

4. **Verify Pattern Match**:
   - After solving, verify final light state matches Part 1 pattern
   - Final state: joltage % 2 determines ON/OFF (even = OFF, odd = ON)

5. **Sum Results**:
   - Same as Part 1

## Implementation Notes

- **GF(2) arithmetic**: Use XOR for addition, AND for multiplication
- **Bitmask representation**: Can use integer bitmasks for small problems (up to ~32-64 lights)
- **Gaussian elimination**: Standard algorithm, but work mod 2 (no division needed)
- **ILP solving**: May need external libraries or custom solver
- **Handling free variables**: In Part 1, free variables can be set to 0 to minimize presses
- **Edge cases**: Machines with no buttons, impossible targets, all-lights-ON patterns
- **Performance**: Real inputs may have many buttons and lights - optimize early

## Edge Cases

- Machine with no buttons (only solvable if target is all OFF / all zero)
- Machine with buttons that don't affect any lights (shouldn't happen, but handle gracefully)
- Target pattern already matches start state (0 presses needed)
- Impossible targets (e.g., Part 2: target joltages that can't be reached with given buttons)
- Very large numbers of buttons (may need optimization for brute force approaches)
- Very large joltage values (Part 2) - may need careful integer handling
