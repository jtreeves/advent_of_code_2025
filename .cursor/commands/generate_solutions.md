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
   - Read from `data/input.txt`
   - Solve both Part 1 and Part 2 if possible
   - Output answers in a format that can be parsed
   - Follow the project's code standards

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
