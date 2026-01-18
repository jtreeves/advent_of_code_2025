# /generate_solutions Command

Spins up 12 separate agents to generate solution code in each language for the specified day.

## Usage

```
/generate_solutions [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. For each of the 12 languages (C, Clojure, Elixir, Go, Haskell, Java, Julia, Kotlin, Python, Ruby, Rust, TypeScript):
   - Creates or updates `days/day_NN/solutions/solution.{ext}`
   - Uses specifications from `days/day_NN/specs/` as guidance
   - Follows language-specific idioms and best practices
2. Each language solution should:
   - Read from `data/input.txt` and other test files
   - Solve Part 1 initially and then Part 2 when available
   - Output answers in a format that can be parsed
   - Follow the project's code standards
3. **MUST** run linting on all generated solution files after creation
4. **MUST** fix any linter errors before proceeding to the next step

## Quality Assurance

- **MUST** run `read_lints` tool on all generated solution files after creation
- **MUST** fix any linter errors (compilation errors, type errors, etc.) before continuing
- **MUST** verify all 12 solution files are created and valid
- Document any warnings that cannot be fixed (e.g., dynamic imports in Python) with `# type: ignore` comments

## Languages and Extensions

- C - `solution.c`
- Clojure - `solution.clj`
- Elixir - `solution.ex`
- Go - `solution.go`
- Haskell - `solution.hs`
- Java - `solution.java`
- Julia - `solution.jl`
- Kotlin - `solution.kt`
- Python - `solution.py`
- Ruby - `solution.rb`
- Rust - `solution.rs`
- TypeScript - `solution.ts`

## Example

```
/generate_solutions 05
```

This generates all 12 solution files for Day 05.
