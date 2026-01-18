# Advent of Code 2025

This repository contains solutions to the Advent of Code 2025 challenge problems. Each day's problem is solved using 12 different programming languages.

## Structure

- `days/` - Contains folders for each day (day_01 through day_12)
  - Each day folder contains:
    - `README.md` - Problem description and key info from AOC
    - `ANALYSIS.md` - Overall approach and language implementation differences
    - `data/` - Input files (input.txt, test_input.txt)
    - `solutions/` - Solution files for each language
    - `specs/` - Spec-kit specifications, plans, and tasks
- `utilities/` - Common utility functions organized by language
- `.cursor/` - Configurations for Cursor
- `.specify/` - Configurations for Spec-kit

## Languages Used

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

## Setup

This project uses [spec-kit](https://github.com/github/spec-kit) for Spec-Driven Development (SDD). 

### Initializing spec-kit

```bash
uvx --from git+https://github.com/github/spec-kit.git specify init . --ai cursor
```

### Custom Slash Commands

- `/day <NN>` - Fetch AOC problem info for day NN, initialize spec process, create solution code in separate languages, and submit to AOC
- `/quick-solve <NN>` - Fetch individual input data, use existing code to determine custom solutions, then submit to AOC for users who pull down the repo

## Usage

Each day's solution can be run independently. See individual day READMEs for instructions on compiling and running solutions in each language.

## Spec-Driven Development Workflow

1. `/speckit.constitution` - Define project principles
2. `/day <NN>` - Initialize a new day (fetches AOC problem, creates specs)
3. `/speckit.specify` - Define requirements
4. `/speckit.plan` - Create technical plan
5. `/speckit.tasks` - Generate tasks
6. `/speckit.implement` - Implement solutions
