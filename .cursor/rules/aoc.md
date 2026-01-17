# Advent of Code 2025 Project Rules

## Project Structure

- Each day has its own folder: `days/day_NN/`
- Solutions are in `days/day_NN/solutions/` with one file per language
- Data files are in `days/day_NN/data/`
- Specs live in `specs/` folder (spec-kit convention)

## Language Assignment

Each day uses a different language:
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

## Workflow

1. Use `/day <NN>` to initialize a new day
2. Use `/speckit.specify` to create specifications
3. Use `/speckit.plan` to create technical plans
4. Use `/speckit.tasks` to break down tasks
5. Implement solutions in all languages
6. Write ANALYSIS.md comparing implementations

## AOC Integration

- Problem descriptions should be fetched from AOC
- Input files should be downloaded (requires session cookie)
- Test inputs can be manually created or extracted from examples

## Utilities

- Common utility functions live in `utilities/<language>/`
- Solutions can import/use these utilities as needed
