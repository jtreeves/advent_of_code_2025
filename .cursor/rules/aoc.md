# Advent of Code 2025 Project Rules

## Project Structure

- Each day has its own folder: `days/day_NN/`
- Solutions are in `days/day_NN/solutions/` with one file per language
- Data files are in `days/day_NN/data/`
  - `input.txt` - Official input (gitignored, user-specific)
  - `test_1.txt`, `test_2.txt`, etc. - Test inputs (committed to git, shared examples)
- Specs live in `days/day_NN/specs/` folder (per-day specs)
  - `day_NN_spec.md` - Specification for the day
  - `day_NN_plan.md` - Technical plan
  - `day_NN_tasks.md` - Task breakdown

## Languages

Each day uses all 12 language:
- C
- Clojure
- Elixir
- Go
- Haskell
- Java
- Julia
- Kotlin
- Python
- Ruby
- Rust
- TypeScript

## Workflow

1. Use `/day <NN>` to initialize a new day
2. Use `/speckit.specify` as part of that flow to create specifications
3. Use `/speckit.plan` as part of that flow to create technical plans
4. Use `/speckit.tasks` as part of that flow to break down tasks
5. Implement solutions in all languages
   - Design Part 1 with Part 2 efficiency in mind
   - Prioritize efficient algorithms and data structures from the start
   - Use minimal external dependencies
6. Run solutions and measure performance
   - **CRITICAL**: Execution time measurements (in milliseconds) are **mandatory** and **cannot be skipped**
   - **MUST** use `.specify/scripts/bash/time_solution.sh` for all timing measurements
   - Update ANALYSIS.md performance tables with actual measurements (not placeholders)
   - See `.cursor/commands/run_solutions.md` and `.cursor/commands/day.md` for detailed requirements
7. Write ANALYSIS.md comparing implementations

## Efficiency Requirements

- Always generate efficient code from the beginning
- Part 2 typically requires efficiency, so consider it during Part 1 design
- Avoid rewriting code between Part 1 and Part 2
- Use as few external libraries as possible (prefer standard library)
- See `.cursor/rules/code_style.md` for detailed efficiency guidelines

## AOC Integration

- Problem descriptions should be fetched from AOC
- Input files should be downloaded (requires session cookie)
- Test inputs can be manually created or extracted from examples

## Utilities

- Common utility functions live in `utilities/<language>/`
- Solutions should **always** use utilities for file I/O operations
- See `.cursor/rules/utility_usage.md` for detailed rules on when to use/create utilities
- Solutions should be self-contained (except for common utilities like file I/O)
