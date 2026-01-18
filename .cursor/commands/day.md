# /day Command

Master orchestrator command that runs the complete end-to-end workflow for a day, executing multiple commands in sequence with user prompts between each step.

## Usage

```
/day <day_number>
```

## Workflow

This command orchestrates the following commands in sequence, prompting for user permission before proceeding from one step to the next:

1. **`/get_description`** - Fetch problem description from AOC and populate README.md
2. **`/get_input`** - Download input file for the day from AOC
3. **`/create_specs`** - Generate spec-kit artifacts (spec, plan, tasks) for the day
4. **`/generate_solutions`** - Generate solution code in all 12 languages (C, Clojure, Elixir, Go, Haskell, Java, Julia, Kotlin, Python, Ruby, Rust, TypeScript)
5. **`/run_solutions`** - Run all solutions, extract answers, update README.md and ANALYSIS.md with results
6. **`/submit_answer`** - Submit Part 1 answer to AOC, then prompt for Part 2 submission
7. **`/sync_specs`** - Adjust spec files to match the resultant code implementations
8. **`/write_analysis`** - Generate comprehensive ANALYSIS.md comparing all 12 implementations
9. **`/push_to_gh`** - Commit and push code to GitHub

## Behavior

### Sequential Execution with Prompts

The command executes each step in order, and **prompts the user for permission** before proceeding to the next step. This allows:

- Review of results before continuing
- Manual intervention if needed
- Skipping optional steps if desired
- Early termination if issues arise

### User Prompts

After each step completes, the command should:
- Display the results of the step
- Ask the user if they want to proceed to the next step
- Allow the user to continue, skip the next step, or stop entirely

### Error Handling

If a step fails:
- Display the error clearly
- Ask the user if they want to:
  - Retry the step
  - Skip to the next step
  - Stop the workflow
- Do not automatically proceed on errors

### Folder Structure Created

When invoked, the command creates or updates `days/day_NN/` with:
- `README.md` - Populated with AOC problem description (updated at step 1, then step 5)
- `ANALYSIS.md` - Template for solution analysis (updated at steps 5, 7, 8)
- `data/input.txt` - Official input (downloaded at step 2, gitignored)
- `data/test_1.txt`, `test_2.txt`, etc. - Test inputs (created during spec phase, committed to git)
- `solutions/` - Solution files for all 12 languages (created at step 4)
- `specs/` - Spec-kit artifacts (created at step 3, updated at step 7)
  - `day_NN_spec.md` - Specification
  - `day_NN_plan.md` - Technical plan
  - `day_NN_tasks.md` - Task breakdown

## Prerequisites

- Session cookie must be configured (via `get_session_cookie.sh` or `.env` file)
- AOC problem must be available for the specified day

## Example

```
/day 05
```

This would:
1. Fetch problem description for Day 05
2. Prompt: "Ready to download input? (y/n/skip)"
3. Download input file
4. Prompt: "Ready to create specs? (y/n/skip)"
5. Generate spec files
6. Prompt: "Ready to generate solutions? (y/n/skip)"
7. Generate all 12 solution files
8. Prompt: "Ready to run solutions? (y/n/skip)"
9. Run all solutions, update README and ANALYSIS
10. Prompt: "Ready to submit Part 1 answer? (y/n/skip)"
11. Submit Part 1
12. Prompt: "Ready to sync specs? (y/n/skip)"
13. Sync spec files with code
14. Prompt: "Ready to write analysis? (y/n/skip)"
15. Generate ANALYSIS.md
16. Prompt: "Ready to push to GitHub? (y/n/skip)"
17. Commit and push to GitHub
18. Prompt: "Ready to get Part 2 description? (y/n/skip)"
19. Fetch prompt description for Part 2 of Day 05
20. Prompt: "Ready to update specs to handle the new part? (y/n/skip)"
21. Update spec files
22. Prompt: "Ready to generate solutions for the new part? (y/n/skip)"
23. Update all 12 solution files
24. Prompt: "Ready to run solutions for the new part? (y/n/skip)"
9. Run all solutions, update README and ANALYSIS
10. Prompt: "Ready to submit Part 2 answer? (y/n/skip)"
11. Submit Part 2
12. Prompt: "Ready to sync specs? (y/n/skip)"
13. Sync spec files with code
14. Prompt: "Ready to update analysis? (y/n/skip)"
15. Update ANALYSIS.md with anything new from Part 2
16. Prompt: "Ready to push to GitHub? (y/n/skip)"
17. Commit and push to GitHub

## Integration with Individual Commands

Each step calls the corresponding individual command:
- Step 1 → `/get_description`
- Step 2 → `/get_input`
- Step 3 → `/create_specs`
- Step 4 → `/generate_solutions`
- Step 5 → `/run_solutions`
- Step 6 → `/submit_answer`
- Step 7 → `/sync_specs`
- Step 8 → `/write_analysis`
- Step 9 → `/push_to_gh`

Users can run individual commands separately if they don't want the full workflow, but `/day` provides a convenient end-to-end orchestration with guided prompts.
