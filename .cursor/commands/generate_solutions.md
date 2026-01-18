# /generate_solutions Command

Spins up 12 separate agents to generate solution code in each language for the specified day.

## Usage

```
/generate_solutions [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. For each of the 12 languages (C, Python, TypeScript, Java, Rust, Kotlin, Haskell, Go, Ruby, Clojure, Elixir, Julia):
   - Creates or updates `days/day_NN/solutions/solution.{ext}`
   - Uses specifications from `days/day_NN/specs/` as guidance
   - Follows language-specific idioms and best practices
2. Each language solution should:
   - Read from `data/input.txt`
   - Solve both Part 1 and Part 2 if possible
   - Output answers in a format that can be parsed
   - Follow the project's code standards

## Languages and Extensions

1. C - `solution.c`
2. Python - `solution.py`
3. TypeScript - `solution.ts`
4. Java - `solution.java`
5. Rust - `solution.rs`
6. Kotlin - `solution.kt`
7. Haskell - `solution.hs`
8. Go - `solution.go`
9. Ruby - `solution.rb`
10. Clojure - `solution.clj`
11. Elixir - `solution.ex`
12. Julia - `solution.jl`

## Example

```
/generate_solutions 05
```

This generates all 12 solution files for Day 05.
