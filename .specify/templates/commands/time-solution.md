# Timing Scripts

## time_solution.sh

Reusable script for measuring execution times of Advent of Code solutions in milliseconds.

### Usage

```bash
bash .specify/scripts/bash/time_solution.sh <day_number> <language> [solution_file]
```

### Parameters

- `day_number`: Day number (1-25), defaults to current day if not provided
- `language`: Language code (c, clj, ex, go, hs, java, jl, kt, py, rb, rs, ts)
- `solution_file`: Optional, defaults to `solution.<language>`

### Examples

```bash
# Time Python solution for Day 5
bash .specify/scripts/bash/time_solution.sh 5 py

# Time C solution for Day 1
bash .specify/scripts/bash/time_solution.sh 1 c

# Time with custom solution file
bash .specify/scripts/bash/time_solution.sh 5 py my_solution.py
```

### Supported Languages

- `c` - C (compiles with gcc)
- `clj` - Clojure (runs with `clojure -M`)
- `ex` - Elixir (runs with `elixir`)
- `go` - Go (runs with `go run`)
- `hs` - Haskell (compiles with ghc)
- `java` - Java (compiles with javac, runs with java)
- `jl` - Julia (runs with julia)
- `kt` - Kotlin (compiles to JAR, runs with java)
- `py` - Python (runs with python3)
- `rb` - Ruby (runs with ruby)
- `rs` - Rust (compiles with rustc)
- `ts` - TypeScript (runs with npx ts-node)

### Output

Returns execution time in milliseconds as a single integer.

### Notes

- For compiled languages (C, Rust, Haskell, Kotlin, Java), execution time includes compilation time
- For interpreted languages (Python, Ruby, TypeScript, Clojure, Elixir, Julia), time represents pure execution
- Go times include compilation via `go run`
- The script automatically cleans up temporary files (executables, object files, etc.)
- For accurate measurements, run multiple times and average the results

### Integration with Workflow

This script **MUST** be used in Step 5 of the `/day` command workflow:
- **CRITICAL**: Timing measurements are mandatory and cannot be skipped
- Run the script 3+ times per language and average for accuracy
- Update ANALYSIS.md performance tables with actual measurements
- Do NOT leave placeholder values like `[To be measured]` or `[C_EXECUTION_TIME]`
