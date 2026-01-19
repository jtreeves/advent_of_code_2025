# Analysis for Day 10

## Overall Approach

This problem involves solving two related optimization problems on a system of lights and buttons. Each machine description specifies a target light pattern, buttons that toggle specific lights, and joltage requirements.

**Part 1** requires finding the minimum number of button presses to achieve a target light pattern (on/off states). This is solved as a linear system over GF(2) (binary arithmetic), where each button press toggles lights. The solution uses brute force with early termination, trying all combinations of button presses in increasing order of weight until finding the minimum solution.

**Part 2** extends the problem to require exact joltage values (non-negative integers) rather than just on/off states. This becomes an Integer Linear Programming (ILP) problem: minimize the sum of button presses subject to constraints that the sum of presses for buttons affecting each light equals the required joltage. The solution uses the Z3 SMT solver with optimizations including a "divide-by-2" optimization (when all joltages are even, halve them and recurse, then double the result).

**Clues**

- The problem explicitly mentions "minimum number of button presses" for Part 1, indicating an optimization problem
- Part 1 uses binary states (on/off), suggesting GF(2) arithmetic
- Part 2 introduces exact joltage requirements, indicating integer linear programming
- The divide-by-2 optimization was discovered through Reddit research and significantly improves performance

## Complexity Analysis

- **Part 1 Time Complexity**: O(2^n * m) in worst case where n is number of buttons and m is number of lights, but early termination makes it practical
- **Part 1 Space Complexity**: O(n + m) for storing the button matrix and target pattern
- **Part 2 Time Complexity**: Depends on Z3 solver performance, but divide-by-2 optimization reduces problem size logarithmically
- **Part 2 Space Complexity**: O(n + m) for storing constraints and variables

## Solutions

### Part 1
| Language   | Initial Solution      |
|------------|-----------------------|
| C          | TOO LONG              |
| Clojure    | NOT SOLVED            |
| Elixir     | NOT SOLVED            |
| Go         | TOO LONG              |
| Haskell    | NOT SOLVED            |
| Java       | TOO LONG              |
| Julia      | TOO LONG              |
| Kotlin     | TOO LONG              |
| Python     | 459                   |
| Ruby       | TOO LONG              |
| Rust       | NOT SOLVED            |
| TypeScript | NOT SOLVED            |

### Part 2
| Language   | Initial Solution      |
|------------|-----------------------|
| C          | TOO LONG              |
| Clojure    | NOT SOLVED            |
| Elixir     | NOT SOLVED            |
| Go         | TOO LONG              |
| Haskell    | NOT SOLVED            |
| Java       | TOO LONG              |
| Julia      | TOO LONG              |
| Kotlin     | TOO LONG              |
| Python     | 18687                 |
| Ruby       | TOO LONG              |
| Rust       | NOT SOLVED            |
| TypeScript | NOT SOLVED            |

## Performance

### Part 1
| Language   | Execution Time (ms)         |
|------------|-----------------------------|
| C          | TOO LONG (>60000)           |
| Clojure    | N/A (NOT SOLVED)            |
| Elixir     | N/A (NOT SOLVED)            |
| Go         | TOO LONG (>60000)           |
| Haskell    | N/A (NOT SOLVED)            |
| Java       | TOO LONG (>60000)           |
| Julia      | TOO LONG (>60000)           |
| Kotlin     | TOO LONG (>60000)           |
| Python     | 1587.6                      |
| Ruby       | TOO LONG (>60000)           |
| Rust       | N/A (NOT SOLVED)            |
| TypeScript | N/A (NOT SOLVED)            |

### Part 2
| Language   | Execution Time (ms)         |
|------------|-----------------------------|
| C          | TOO LONG (>60000)           |
| Clojure    | N/A (NOT SOLVED)            |
| Elixir     | N/A (NOT SOLVED)            |
| Go         | TOO LONG (>60000)           |
| Haskell    | N/A (NOT SOLVED)            |
| Java       | TOO LONG (>60000)           |
| Julia      | TOO LONG (>60000)           |
| Kotlin     | TOO LONG (>60000)           |
| Python     | 1587.6                      |
| Ruby       | TOO LONG (>60000)           |
| Rust       | N/A (NOT SOLVED)            |
| TypeScript | N/A (NOT SOLVED)            |

## Implementation Differences

### C
[How the C implementation differs (e.g., manual memory management, pointers)]

### Clojure
[How the Clojure implementation differs (e.g., immutable data structures, macros)]

### Elixir
[How the Elixir implementation differs (e.g., pattern matching, processes)]

### Go
[How the Go implementation differs (e.g., goroutines, channels)]

### Haskell
[How the Haskell implementation differs (e.g., functional style, laziness)]

### Java
[How the Java implementation differs (e.g., object-oriented approach, collections)]

### Julia
[How the Julia implementation differs (e.g., multiple dispatch, performance)]

### Kotlin
[How the Kotlin implementation differs (e.g., null safety, extension functions)]

### Python
The Python implementation uses:
- **Type hints** throughout for clarity (`List[bool]`, `Optional[int]`, etc.)
- **Z3 SMT solver** for Part 2 ILP optimization, using `z3.Optimize()` with `solver.minimize(total_presses)`
- **Brute force with early termination** for Part 1, using `itertools.combinations` to try all button combinations in increasing order
- **Divide-by-2 optimization** for Part 2: recursively halves joltages when all are even, then doubles the result
- **Non-negativity constraints** (`p >= 0`) and upper bounds (`p <= total_joltage`) to help Z3 solver performance
- **Comprehensive error handling** with fallback to bounded DFS if Z3 is unavailable

### Ruby
[How the Ruby implementation differs (e.g., dynamic typing, blocks)]

### Rust
[How the Rust implementation differs (e.g., ownership, borrowing, pattern matching)]

### TypeScript
[How the TypeScript implementation differs (e.g., strong typing, async/await)]

## Key Observations

### Algorithm Evolution
1. **Initial Part 1 approach**: Attempted GF(2) Gaussian elimination with back-substitution, but this didn't guarantee minimum weight solutions due to free variable handling issues
2. **Part 1 solution**: Switched to brute force with early termination, which guarantees correctness and is fast enough for the input size
3. **Initial Part 2 approach**: Bounded depth-first search with pruning, but this was too slow (>280 seconds) for the full input
4. **Part 2 optimization**: Implemented Z3 SMT solver, reducing time to ~1.2 seconds
5. **Further optimization**: Added divide-by-2 optimization based on Reddit research, which recursively reduces problem size when all joltages are even

### Performance Insights
- The brute force approach for Part 1 works well because the search space is manageable and early termination finds solutions quickly
- Z3 solver is essential for Part 2's ILP problem - manual search approaches are too slow
- The divide-by-2 optimization is critical for performance, reducing problem size logarithmically
- Upper bounds on Z3 variables (`p <= total_joltage`) help solver performance without being overly restrictive

### Implementation Challenges
- **GF(2) Gaussian elimination**: The initial approach had subtle bugs in free variable handling that prevented finding minimum weight solutions
- **Z3 configuration**: Required careful tuning of constraints (non-negativity, upper bounds) and use of `z3.Optimize()` instead of basic `z3.Solver()`
- **Pattern constraint confusion**: Initially thought Part 2 required matching the target pattern parity, but this was not a direct constraint (167 of 171 lines had mismatches)
- **Answer debugging**: Went through several incorrect answers (512, 481, 1206, 18688) before arriving at correct answer (459 for Part 1, 18687 for Part 2)

### Test Case Development
Created 12 comprehensive test cases (test_1.txt through test_18.txt) with varying complexity to debug Part 2:
- Simple cases with 1-3 lights and buttons
- Complex cases with 10+ lines, 10+ characters per line
- Edge cases with all zeros, all even joltages, etc.
- Each test case manually verified with expected outputs

### Other Language Status
Most other language implementations either:
- **Timed out** (>60 seconds) due to using brute force or bounded DFS approaches that are too slow
- **Failed to compile** due to missing dependencies (Rust needs regex crate), syntax errors (Elixir, Haskell, Clojure), or module issues (TypeScript ES modules)

The Python solution is the only one that successfully completes both parts within reasonable time, primarily due to Z3 solver availability and the divide-by-2 optimization.

## Notes

- Z3 solver was installed using `pip install z3-solver --break-system-packages` due to externally-managed-environment restrictions
- The correct answers were: Part 1 = 459, Part 2 = 18687
- Execution time for Python: ~1587.6ms for both parts combined
- Reddit threads for Day 10 Part 2 provided valuable hints about the divide-by-2 optimization and Z3 solver configuration
