# Project Context

## Project Goals

This repository solves Advent of Code 2025 problems in 12 different programming languages, demonstrating:
- Language-specific idioms and patterns
- Different approaches to the same problem
- Trade-offs and considerations unique to each language

## Architecture Decisions

### Structure
- Each day has its own folder: `days/day_NN/`
- Solutions, data, specs, and docs are co-located per day
- Specs live in `days/day_NN/specs/` (not in root `specs/` folder)
- Utilities are organized by language in `utilities/<language>/`

### Utilities Organization
Utilities are split into separate files by function category:
- `get_input.go`, `get_input.py`, etc. - Input reading functions
- `parse.go`, `parse.py`, etc. - Parsing utilities
- `common.go`, `common.py`, etc. - Other shared utilities

This organization makes it easier to:
- Find specific utility functions
- Understand dependencies
- Maintain and update utilities independently

### Test Data
- Official input: `days/day_NN/data/input.txt` (gitignored, user-specific)
- Test inputs: `days/day_NN/data/test_1.txt`, `test_2.txt`, etc. (committed to git)
- Solutions can use utility functions to read from either input.txt or test_N.txt files

### Workflow
1. Use Cursor commands (`.cursor/commands/`) for high-level workflows
2. Use bash scripts (`.specify/scripts/bash/`) for AOC API interactions
3. Use spec-kit templates (`.specify/templates/`) for generating artifacts
4. Each day is self-contained with its own specs and solutions

## Design Principles

1. **Idiomatic Code**: Each solution should follow language-specific best practices
2. **Modularity**: Utilities are split by function to improve maintainability
3. **Consistency**: Similar structure across all days and languages
4. **Documentation**: ANALYSIS.md files compare implementations across languages
