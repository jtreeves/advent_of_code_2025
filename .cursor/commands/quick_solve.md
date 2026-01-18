# /quick_solve Command

Quick workflow for users who pull down the repo with working code and need to: get their input, run solutions, submit answers, and push to git.

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
   - Runs all 12 solution files
   - Extracts answers and updates README

3. **Submit Answers** (`/submit_answer`):
   - Submits Part 1 answer to AOC
   - Submits Part 2 answer to AOC (after Part 1 succeeds)

4. **Push to GitHub** (`/push_to_gh`):
   - Commits changes
   - Pushes to current branch

## Use Case

For users who:
- Have pulled the repo with pre-existing solution code
- Need to use their own input files
- Want to quickly get answers and submit them
- Want to save their progress to git

## Prerequisites

- Session cookie must be configured
- Input file must be available on AOC
- Solutions must already exist in the repo

## Example

```
/quick_solve 05
```

This executes the complete quick workflow for Day 05.
