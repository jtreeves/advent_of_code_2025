# /run_solutions Command

Runs all solution files for a day, extracts answers, and updates the README with results.

## Usage

```
/run_solutions [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. For each language solution file:
   - Compiles/transpiles the code (if needed)
   - Runs the solution with `data/input.txt`
   - Captures the output (Part 1 and Part 2 answers)
   - Handles errors gracefully
2. Updates `days/day_NN/README.md`:
   - Sets "Part 1 Answer" section with the result
   - Sets "Part 2 Answer" section with the result (if available)
   - Preserves other README content

## Output Format

Solutions should output answers in a parseable format, e.g.:
```
Part 1: <answer>
Part 2: <answer>
```

Or:
```
<answer1>
<answer2>
```

## Error Handling

- If a solution fails to compile or run, logs the error but continues with other languages
- Missing answers are noted in the README

## Example

```
/run_solutions 05
```

This runs all solutions for Day 05 and updates the README with answers.
