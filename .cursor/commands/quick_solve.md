# /quick_solve Command

Quick workflow for users who pull down the repo with working code and need to: get their input, run solutions, and update documentation.

## Usage

```
/quick_solve [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

This command orchestrates the following workflow:

1. **Get Input** (`/get_input`):
   - Fetches input for the day from AOC
   - Overwrites `days/day_NN/data/input.txt`

2. **Run Solutions** (`/run_solutions`):
   - Runs all 12 solution files for both Part 1 and Part 2
   - **CRITICAL**: Measures execution times using `.specify/scripts/bash/time_solution.sh` (mandatory, cannot be skipped)
   - Extracts answers and updates README
   - Updates ANALYSIS.md with execution times (performance tables)
   - Cleans up nonsense files from solutions folder

3. **Sync Specs** (`/sync_specs`):
   - Updates spec files to match actual implementations
   - Marks completed tasks in tasks file

## Use Case

For users who:
- Have pulled the repo with pre-existing solution code
- Need to use their own input files
- Want to quickly get answers and update documentation
- Want to sync specs with code

## Prerequisites

- Session cookie must be configured
- Input file must be available on AOC
- Solutions must already exist in the repo

## Example

```
/quick_solve 05
```

This executes the complete quick workflow for Day 05: gets input, runs solutions, updates docs, syncs specs, and cleans up.
