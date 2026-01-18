# Utility Usage Rules

## When to Use Utilities

### Always Use Utilities for I/O Operations

Always use utility functions to extract data from files:
   - Reading input files (`get_input`, `get_test_input`)
   - Reading raw file content (`read_input_raw`)
   - Never write inline file reading code in solution files
   - If a day needs a different file reading approach, update or extend the utility functions

### When to Create New Utility Files

Create new utility files **only** for functionality that:
- Will probably be used by multiple problems (common across days)
- Is general-purpose and not algorithm-specific
- Examples:
  - File I/O operations
  - Common data parsing patterns (parsing integers, splitting formats, etc.)
  - Common data structures or helpers (grids, graphs, etc.)
  - Mathematical operations (GCD, LCM, etc.)

### When NOT to Create Utilities

**Do NOT create utilities for:**
1. **Day-specific logic**: Keep day-specific code in the solution file
2. **Algorithm-specific code**: Even if an algorithm might be reused, keep it in the solution file
   - Examples: BFS, DFS, dynamic programming implementations
   - The algorithm code should be clearly defined in the day's main solution file
   - This keeps solutions self-contained and readable

### Code Reuse Between Days

When solving subsequent days:
1. **Look at previous days' code** to see if anything can be reused
2. **Copy and paste** reusable code directly into the new solution file
3. **Don't create utilities** for code that's copied from previous days
4. This keeps each solution self-contained while allowing code reuse

## Examples

### ✅ Good: Using utilities for file I/O
```python
# solution.py
from utilities.python.get_input import get_input

def solve(lines):
    # Solution logic here
    pass

if __name__ == "__main__":
    lines = get_input(1)  # Uses utility
    solve(lines)
```

### ❌ Bad: Inline file reading
```python
# solution.py
with open("../data/input.txt", "r") as f:
    lines = f.readlines()  # Don't do this - use utilities instead
```

### ✅ Good: Keeping algorithm in solution file
```python
# solution.py - Day 05
def bfs(graph, start):
    # BFS implementation specific to this day's problem
    # Even if BFS is used again, keep it here
    pass
```

### ❌ Bad: Creating algorithm utilities
```python
# Don't create utilities/bfs.py
# Don't create utilities/algorithms.py
```

### ✅ Good: Copying from previous day
```python
# solution.py - Day 06
# Copy BFS from Day 05 if needed
def bfs(graph, start):
    # Same implementation from Day 05, copied here
    pass
```

### ✅ Good: Creating general-purpose utilities
```python
# utilities/python/grid.py - if grids are used across multiple days
def parse_grid(lines):
    # General grid parsing used by multiple problems
    pass
```

### ❌ Bad: Creating day-specific utilities
```python
# Don't create utilities/day05_parser.py
# Keep day-specific code in days/day_05/solutions/solution.py
```

## Principle

**Solutions should be self-contained** (except for common utilities like file I/O), while **utilities should be truly general-purpose** and reusable across multiple days. When in doubt, keep code in the solution file rather than creating a utility.
