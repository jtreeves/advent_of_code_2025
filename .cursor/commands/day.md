# /day Command

Master orchestrator command that runs the complete end-to-end workflow for a day, executing multiple commands in sequence with user prompts between each step.

## Usage

```
/day <day_number>
```

## Workflow

This command orchestrates the following commands in sequence, prompting for user permission before proceeding from one step to the next:

1. **`/get_description`** - **MANDATORY**: Fetch problem description from AOC and populate README.md with verbatim text from the AOC site
2. **`/get_input`** - **MANDATORY**: Download input file for the day from AOC and save to `data/input.txt`
3. **`/create_specs`** - **MANDATORY**: Generate spec-kit artifacts (spec, plan, tasks) for the day in `specs/` folder
4. **`/generate_solutions`** - Generate solution code in all 12 languages (C, Clojure, Elixir, Go, Haskell, Java, Julia, Kotlin, Python, Ruby, Rust, TypeScript)
5. **`/run_solutions`** - **MANDATORY**: Run all solutions, extract answers, update ANALYSIS.md tables with results and execution times, update README.md with solutions if all languages agree
6. **`/submit_answer`** - Submit Part 1 answer to AOC, then prompt for Part 2 submission
7. **`/sync_specs`** - Adjust spec files to match the resultant code implementations
8. **`/write_analysis`** - Generate comprehensive ANALYSIS.md comparing all 12 implementations
9. **`/push_to_gh`** - Commit and push code to GitHub

## Critical Requirements

The following steps are **MANDATORY** and must not be skipped:

### Step 1: Get Description
- **MUST** fetch the problem description verbatim from the AOC website
- **MUST** populate `days/day_NN/README.md` with the exact text from AOC (both Part 1 and Part 2)
- **MUST NOT** use paraphrased or summarized text - use the exact description from the site
- Must match the structure used in `days/day_01/README.md` as a reference

### Step 2: Get Input
- **MUST** download the actual input file from AOC using the session cookie
- **MUST** save to `days/day_NN/data/input.txt`
- **MUST NOT** leave placeholder text like `<official input here>`
- Uses `.specify/scripts/bash/get_session_cookie.sh` and `.specify/scripts/bash/get_input.sh`

### Step 3: Create Specs
- **MUST** create all three spec files in `days/day_NN/specs/`:
  - `day_NN_spec.md` - Problem specification with requirements, I/O format, constraints, test cases
  - `day_NN_plan.md` - Technical plan with algorithms, data structures, complexity analysis
  - `day_NN_tasks.md` - Task breakdown for implementation
- **MUST NOT** skip spec creation - these are essential for the workflow
- Must include test data files (`test_1.txt`, `test_2.txt`, etc.) if applicable

### Step 5: Run Solutions
- **MUST** run all 12 solution files against `data/input.txt`
- **MUST** update `ANALYSIS.md` solutions tables with:
  - Part 1 results for each language
  - Part 2 results for each language
  - Execution times in milliseconds for each language
- **MUST** update `README.md` with solutions if all languages produce the same result
- **MUST** handle errors gracefully but note them in ANALYSIS.md

### Linting Requirements
- **MUST** run linting on all newly created or modified files after generation
- **MUST** fix any linter errors before proceeding to the next step
- This includes:
  - Solution files (all 12 languages)
  - Spec files (if they have lintable syntax)
  - README.md and ANALYSIS.md (markdown linting if configured)
- Use `read_lints` tool to check for errors and fix them before continuing

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
  - Skip to the next step (NOT ALLOWED for mandatory steps)
  - Stop the workflow
- Do not automatically proceed on errors
- **Mandatory steps (1, 2, 3, 5) cannot be skipped** - they must complete successfully

### Quality Assurance

Before proceeding from one step to the next:

1. **Verify completion**: Check that all mandatory outputs exist:
   - Step 1: README.md has problem description (check file size/content)
   - Step 2: `data/input.txt` exists and is not a placeholder
   - Step 3: All three spec files exist in `specs/` folder
   - Step 5: ANALYSIS.md tables are populated with results and times

2. **Run linting**: After generating/modifying code files:
   - Run `read_lints` on all new/modified files
   - Fix any errors before continuing
   - Document any warnings that cannot be fixed (e.g., dynamic imports in Python)

3. **Validate outputs**: Ensure outputs are in expected format:
   - Solutions output "Part 1: <answer>" and "Part 2: <answer>" format
   - Execution times are captured in milliseconds
   - ANALYSIS.md tables are properly formatted markdown tables

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
