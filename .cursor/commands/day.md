# /day Command

This command initializes a new day for Advent of Code 2025.

## Usage

```
/day <day_number>
```

## Behavior

When invoked with a day number (e.g., `/day 01`), this command should:

1. Fetch the problem description from Advent of Code 2025 for the specified day
2. Create or update the `days/day_NN/` folder structure with:
   - `README.md` - Populated with AOC problem description
   - `ANALYSIS.md` - Template for solution analysis
   - `data/input.txt` - Placeholder for official input
   - `data/test_input.txt` - Placeholder for test input
   - `solutions/` - Solution files for all 12 languages
3. Initialize spec-kit artifacts in `days/day_NN/specs/`:
   - `day_NN_spec.md` - Specification for the day
   - `day_NN_plan.md` - Technical plan
   - `day_NN_tasks.md` - Task breakdown
4. Update the project structure as needed

## Integration with AOC

The command should:
- Use AOC API or web scraping to fetch problem descriptions
- Note: AOC requires authentication (session cookie) to fetch input files
- Store session cookie in a secure location (e.g., `.env` file, not in git)

## Example

```
/day 05
```

This would initialize day 05 with all necessary files and spec-kit artifacts.
