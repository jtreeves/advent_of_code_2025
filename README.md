# Advent of Code 2025

This repository contains solutions to the Advent of Code 2025 challenge problems. Each day's problem is solved using a different programming language, with a total of 12 languages used across the 12 days.

## Structure

- `days/` - Contains folders for each day (day_01 through day_12)
  - Each day folder contains:
    - `README.md` - Problem description and key info from AOC
    - `ANALYSIS.md` - Overall approach and language implementation differences
    - `data/` - Input files (input.txt, test_input.txt)
    - `solutions/` - Solution files for each language
- `utilities/` - Common utility functions organized by language
- `specs/` - Spec-kit specifications, plans, and tasks

## Languages Used

1. C
2. Python (with types)
3. TypeScript
4. Java
5. Rust
6. Kotlin
7. Haskell
8. Go
9. Ruby
10. Clojure
11. Elixir
12. Julia

## Setup

This project uses [spec-kit](https://github.com/github/spec-kit) for Spec-Driven Development (SDD). 

### Initializing spec-kit

```bash
uvx --from git+https://github.com/github/spec-kit.git specify init . --ai cursor
```

### Custom Slash Commands

- `/day <NN>` - Fetch AOC problem info for day NN, initialize spec process, and create day structure

## Usage

Each day's solution can be run independently. See individual day READMEs for instructions on compiling and running solutions in each language.

## Spec-Driven Development Workflow

1. `/speckit.constitution` - Define project principles
2. `/day <NN>` - Initialize a new day (fetches AOC problem, creates specs)
3. `/speckit.specify` - Define requirements
4. `/speckit.plan` - Create technical plan
5. `/speckit.tasks` - Generate tasks
6. `/speckit.implement` - Implement solutions
