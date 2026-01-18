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
   - **MUST** fill in the solutions tables with Part 1 results for each language
   - **MUST** fill in the solutions tables with Part 2 results for each language
   - **MUST** fill in the performance tables with execution times (in milliseconds) for each language for both Part 1 and Part 2
   - **MUST NOT** leave placeholder values like `[C_SOLUTION]` or `[C_EXECUTION_TIME]`
   - Preserves other ANALYSIS content and does not block other agents from simultaneously updating the same file
3. Updates `days/day_NN/README.md`:
   - ONLY IF all languages yield the same result in the solutions table, set "Part 1 Solution" section with the result (then part 2)
   - IF a discrepancy occurs, then describe that in the "Part 1[OR 2] Solution" section
   - Preserves other README content
4. **MUST** clean up nonsense files from `solutions/` folder:
   - Remove any compiled files (`.o`, `.hi`, `.class`, executables, etc.)
   - Remove any temporary files or build artifacts
   - Keep only source files (`solution.c`, `solution.py`, etc.)
   - This cleanup should happen after running solutions to capture execution times

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
- Missing answers are noted in the ANALYSIS with `[Needs fix]` or similar notation
- **MUST** run all 12 solutions - do not skip languages unless explicitly requested

## Quality Assurance

- **MUST ALWAYS** run linting on all solution files after any step that involves touching solution files (using `read_lints` tool)
- **MUST** fix any linter errors immediately after they are found - do not proceed to the next step until all linting errors are resolved
- **MUST** verify all solutions table cells are filled with actual values (not placeholders)
- **MUST** verify all performance table cells are filled with execution times in milliseconds

## Linting Requirements

- **CRITICAL**: After ANY step that modifies solution files (generation, updates, fixes), you MUST:
  1. Run `read_lints` on all solution files in `days/day_NN/solutions/`
  2. Fix all linter errors before proceeding, including:
     - Removing unused imports (e.g., unused Java imports, unused Python imports)
     - Fixing type errors
     - Fixing compilation errors
     - Fixing any other linter warnings or errors
  3. Re-run linting to verify all errors are resolved
- This applies to: generating solutions, fixing bugs, updating code, or any other modification to solution files
- **MUST** check for and remove unused imports in all languages:
  - Java: Remove unused `import` statements
  - Python: Remove unused imports from `from` and `import` statements
  - TypeScript: Remove unused `import` statements
  - Other languages: Remove any unused imports/use statements as appropriate

## Example

```
/run_solutions 05
```

This runs all solutions for Day 05 and updates the ANALYSIS and README with answers.
