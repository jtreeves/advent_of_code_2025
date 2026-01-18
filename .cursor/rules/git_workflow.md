# Git Workflow Rules

## Commit Message Format

### Imperative Style

- Use imperative verbs (e.g., "Solve", "Add", "Update", "Fix")
- Begin with a capital letter
- Keep messages concise and descriptive

### Examples

```
Solve day 3, part 1
Add analysis for day 5
Update README with solutions
Fix C implementation for day 2
```

## Commit Cadence

- Commit after completing Part 1 solutions
- Commit after completing Part 2 solutions
- Commit after writing analysis document
- Commit incrementally as work progresses (don't wait for everything to be done)

## What to Commit

### Always Commit

- Solution files (`days/day_NN/solutions/*`)
- Analysis files (`days/day_NN/ANALYSIS.md`)
- Spec files (`days/day_NN/specs/*`)
- Test input files (`days/day_NN/data/test_*.txt`)
- Documentation updates

### Never Commit

- Input files (`days/day_NN/data/input.txt`) - These are gitignored
- Session cookies or credentials (`.env` files)
