# /run_solutions Command

Runs all solution files for a day, extracts answers, and updates the README with results. Use 12 separate agents for each language. If agents previously created for generating solutions are already available, then use those.

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
2. Updates `days/day_NN/ANALYSIS.md`:
   - Fills in its portion of the solutions and performance tables with its results and execution time for part 1 (then part 2)
   - Preserves other ANALYSIS content and does not block other agents from simultaneously updating the same file
3. Updates `days/day_NN/README.md`:
   - ONLY IF all languages yield the same result in the solutions table, set "Part 1 Solution" section with the result (then part 2)
   - IF a discrepancy occurs, then describe that in the "Part 1[OR 2] Solution" section
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

They should also output execution time in ms.

## Error Handling

- If a solution fails to compile or run, logs the error but continues with other languages
- Missing answers are noted in the ANALYSIS

## Example

```
/run_solutions 05
```

This runs all solutions for Day 05 and updates the ANALYSIS and README with answers.
