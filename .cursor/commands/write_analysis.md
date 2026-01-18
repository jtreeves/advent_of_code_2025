# /write_analysis Command

Generates the ANALYSIS.md file for the specified day, comparing implementations across all 12 languages.

## Usage

```
/write_analysis [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Analyzes all 12 solution files in `days/day_NN/solutions/`
2. Generates `days/day_NN/ANALYSIS.md` using the template from `.specify/templates/ANALYSIS-template.md`
3. Documents:
   - Overall approach and algorithm
   - Complexity analysis (time and space)
   - Language-specific implementation differences
   - Key observations and trade-offs
   - Performance notes (if applicable)
4. Highlights:
   - Idiomatic patterns in each language
   - Notable differences in approach
   - Pros/cons of each implementation

## Template

Uses `.specify/templates/ANALYSIS-template.md` as the base template.

## Example

```
/write_analysis 05
```

This generates `days/day_05/ANALYSIS.md` comparing all 12 implementations.
