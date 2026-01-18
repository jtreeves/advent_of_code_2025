# Repository Structure and Flow

## Overall Flow

This repository is organized to solve Advent of Code 2025 problems in 12 different programming languages, with one language per day. The workflow is designed to be automated as much as possible while maintaining flexibility for manual intervention.

### High-Level Workflow

1. **Initialize a Day**: Use `/day <NN>` command to fetch problem description, create folder structure, and set up specifications
2. **Create Specifications**: Generate specs, plans, and tasks using spec-kit (stored per-day in `days/day_NN/specs/`)
3. **Generate Solutions**: Use 12 separate agents to generate code in each language
4. **Test and Verify**: Run solutions, get answers, and update README with results
5. **Submit to AOC**: Submit answers for Part 1 and Part 2
6. **Documentation**: Write ANALYSIS.md comparing implementations
7. **Version Control**: Push code to GitHub

### For Repository Users

If someone pulls the repo with existing solutions, they can:
- Get their own input files (inputs are gitignored)
- Run the solutions to get answers
- Submit their answers to AOC
- Push to their own fork/branch

---

## .specify Directory Structure

The `.specify/` folder contains files for the spec-kit tool, which uses Spec-Driven Development (SDD) principles.

### Subfolders

#### `.specify/memory/`
Contains project-wide memory files that inform the AI about the project context:

- **`constitution.md`**: Project principles, goals, and code standards
- **Future additions**: 
  - `project_context.md` - Overall project architecture and decisions
  - `common_patterns.md` - Reusable patterns across implementations
  - `lessons_learned.md` - Insights from previous days that should inform future work

#### `.specify/scripts/`
Contains utility scripts for automation:

- **`bash/aoc_helper.sh`**: (Legacy - being replaced)
- **`bash/get_session_cookie.sh`**: Manages AOC session cookie with smart fallback logic
- **`bash/get_problem_description.sh`**: Fetches problem description and updates README.md
- **`bash/get_input.sh`**: Downloads input for a specific day
- **`bash/submit_answer.sh`**: Submits answers to AOC (handles Part 1/Part 2 automatically)

#### `.specify/templates/`
Contains templates used when generating new files:

- **`spec-template.md`**: Template for spec-kit specifications
- **`plan-template.md`**: Template for technical plans
- **`tasks-template.md`**: Template for task breakdowns
- **`README-template.md`**: Template for day README files (NEW)
- **`ANALYSIS-template.md`**: Template for ANALYSIS.md files (NEW)
- **`commands/`**: (Potential location for spec-kit command definitions, if using spec-kit's command system)

**Note**: Cursor commands (`.cursor/commands/`) are separate from spec-kit commands. Cursor commands are for Cursor IDE interaction, while spec-kit commands are for the spec-kit tool's own command system. They can work together but serve different purposes.

---

## .cursor Directory Structure

The `.cursor/` folder contains Cursor IDE-specific configuration that helps guide the AI assistant's behavior.

### Subfolders

#### `.cursor/rules/`
Contains rule files that define **how the AI should behave** - guidelines, conventions, and constraints:

- **`aoc.md`**: Project structure rules, language assignments, workflow conventions
- **Potential additions**:
  - `code_style.md` - Formatting and style guidelines per language
  - `testing.md` - Testing conventions and requirements
  - `git_workflow.md` - Git commit message conventions, branching strategy

**Rules are passive** - they inform the AI's decision-making but don't directly execute actions.

#### `.cursor/commands/`
Contains command files that define **what the AI should do** - actionable commands the user can invoke:

- **`day.md`**: Master command that orchestrates the full day workflow
- **`get_description.md`**: Fetch and populate README.md with problem description
- **`get_input.md`**: Download input for the day
- **`create_specs.md`**: Generate spec-kit artifacts for the day
- **`generate_solutions.md`**: Spin up 12 agents to generate code in each language
- **`run_solutions.md`**: Run solutions, get answers, update README
- **`submit_answer.md`**: Submit answer to AOC (handles Part 1/Part 2)
- **`push_to_gh.md`**: Push code to GitHub
- **`sync_specs.md`**: Adjust spec files to match resultant code
- **`write_analysis.md`**: Generate ANALYSIS.md file
- **`quick_solve.md`**: Quick workflow for users pulling the repo (commands 2, 5, 6, 7)

**Commands are active** - they describe workflows that can be invoked via `/command_name`.

### Rules vs Commands

- **Rules** (`.cursor/rules/`): Define constraints and guidelines ("how should the AI think/behave?")
- **Commands** (`.cursor/commands/`): Define workflows and actions ("what should the AI do?")

### Commands vs .specify Files

- **Cursor Commands** (`.cursor/commands/`): High-level workflows, user-facing actions, can invoke scripts and tools
- **Spec-kit Templates** (`.specify/templates/`): File templates for spec-kit to use when generating artifacts
- **Spec-kit Commands**: (If used) Would be spec-kit's own command definitions - these are separate from Cursor commands

---

## Specs Organization

### Previous Structure
- `specs/` at root level (spec-kit convention)

### New Structure
- `days/day_NN/specs/` - Each day has its own specs folder
- This allows specs to live alongside the code they describe
- Spec-kit can be configured to work with this structure

### Files per Day
- `days/day_NN/specs/day_NN_spec.md` - Specification
- `days/day_NN/specs/day_NN_plan.md` - Technical plan
- `days/day_NN/specs/day_NN_tasks.md` - Task breakdown

---

## Input Files and Git

Input files (`days/day_NN/data/input.txt`) are **gitignored** because:
- Each user needs their own input file from AOC
- Input files are unique per AOC account
- When someone pulls the repo, they should download their own input

Test input files (`test_input.txt`) may remain in git if they're small examples, or can also be gitignored if they're user-specific.

---

## Command Hierarchy

### Full Workflow Commands
1. `/day` - Master command orchestrating everything (calls other commands with pauses)

### Individual Workflow Commands
2. `/get_description` - Get problem description, populate README.md
3. `/get_input` - Get input content
4. `/create_specs` - Create the specs
5. `/generate_solutions` - Spin up 12 agents to generate code
6. `/run_solutions` - Run code, get answers, update README
7. `/submit_answer` - Submit answer to AOC
8. `/push_to_gh` - Push code to GitHub
9. `/sync_specs` - Adjust spec files to match code
10. `/write_analysis` - Write ANALYSIS.md

### Quick Workflow Commands
11. `/quick_solve` - For users pulling the repo: get input, run solutions, submit, push (commands 2, 5, 6, 7)

These commands are modular - users can run individual ones or combine them as needed.
