# Solution File Guidelines

## File I/O Operations

### Perform I/O Inline

All file I/O operations should be performed inline within solution files:
   - Reading input files should be done directly in the solution
   - No external utility modules or functions for file I/O
   - Solutions should be self-contained

### Examples

#### ✅ Good: Inline file reading
```python
# solution.py
def solve(data):
    # Solution logic here
    pass

if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        data = f.read()
    solve(data)
```

#### ✅ Good: Inline file reading (other languages)
```go
// solution.go
func main() {
    data, err := os.ReadFile("../data/input.txt")
    if err != nil {
        panic(err)
    }
    solve(string(data))
}
```

## Code Organization

### Keep Solutions Self-Contained

**Solutions should be self-contained:**
1. **Day-specific logic**: Keep all day-specific code in the solution file
2. **Algorithm-specific code**: Even if an algorithm might be reused, keep it in the solution file
   - Examples: BFS, DFS, dynamic programming implementations
   - The algorithm code should be clearly defined in the day's main solution file
   - This keeps solutions self-contained and readable

### Code Reuse Between Days

When solving subsequent days:
1. **Look at previous days' code** to see if anything can be reused
2. **Copy and paste** reusable code directly into the new solution file
3. **Don't create utility modules** for code that's copied from previous days
4. This keeps each solution self-contained while allowing code reuse

## Examples

### ✅ Good: Keeping algorithm in solution file
```python
# solution.py - Day 05
def bfs(graph, start):
    # BFS implementation specific to this day's problem
    # Even if BFS is used again, keep it here
    pass
```

### ✅ Good: Copying from previous day
```python
# solution.py - Day 06
# Copy BFS from Day 05 if needed
def bfs(graph, start):
    # Same implementation from Day 05, copied here
    pass
```

## Principle

**Solutions should be completely self-contained** with no external dependencies. When in doubt, keep code in the solution file.
