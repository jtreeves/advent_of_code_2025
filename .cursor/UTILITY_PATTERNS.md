# Utility Patterns

## Organization

Utilities are organized by language and split into separate files by function category:
- `get_input.*` - Input reading functions
- `parse.*` - Parsing utilities (if needed)
- `common.*` or `utils.*` - Other shared utilities

This organization makes it easier to:
- Find specific utility functions
- Understand dependencies
- Maintain and update utilities independently

## Input Reading Pattern

### Functions

Each language should have `get_input` utilities with:
1. **`get_input(day)`** - Reads from `input.txt` (main input)
2. **`get_test_input(day, test_num)`** - Reads from `test_N.txt` (test cases)
3. **`read_input(file_path)`** - Generic file reading (used internally)
4. **`read_input_raw(file_path)`** - Read raw file content (if needed)

### File Paths
- Solutions are in: `days/day_NN/solutions/`
- Input files are in: `days/day_NN/data/`
  - `input.txt` - Official input (gitignored)
  - `test_1.txt`, `test_2.txt`, etc. - Test cases (committed to git)

### Path Resolution
Utilities use relative paths (`../data/input.txt`) which are resolved relative to the solution file location.

## Language-Specific Notes

### C
- Header file: `get_input.h`
- Implementation: `get_input.c`
- Uses `InputLines` struct to hold lines
- Must free memory with `free_input_lines()`

### Python
- Module: `get_input.py`
- Uses `pathlib.Path` for path resolution
- Returns list of strings

### TypeScript
- Module: `get_input.ts`
- Uses `path.resolve()` for path resolution
- Returns array of strings

### Go
- Package: `utils` (in `get_input.go`)
- Uses `filepath.Abs()` for path resolution
- Returns `([]string, error)`

### Rust
- Module: `get_input.rs`
- Uses `Path` for path resolution
- Returns `Result<Vec<String>, std::io::Error>`

### Other Languages
Similar patterns apply for Java, Kotlin, Haskell, Ruby, Clojure, Elixir, and Julia.

## Solution File Pattern

Solutions should import and use utility functions instead of inline file reading:

```python
# Python example
from utilities.python.get_input import get_input

def solve(data):
    # Process data
    pass

if __name__ == "__main__":
    lines = get_input(1)  # Day 1
    # Process lines
```

```c
// C example
#include "../../utilities/c/get_input.h"

int main() {
    InputLines* input = get_input(1);  // Day 1
    // Process input->lines
    free_input_lines(input);
    return 0;
}
```
