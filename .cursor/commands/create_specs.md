# /create_specs Command

Creates spec-kit artifacts (specification, plan, tasks) for the specified day.

## Usage

```
/create_specs [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Creates `days/day_NN/specs/` folder (if it doesn't exist)
2. Generates spec-kit artifacts using templates:
   - `day_NN_spec.md` - Specification for the day
   - `day_NN_plan.md` - Technical plan
   - `day_NN_tasks.md` - Task breakdown
3. Uses templates from `.specify/templates/` folder
4. Can integrate with spec-kit commands if available

## Critical Requirements

- **MUST** create all three spec files in `days/day_NN/specs/`:
  - `day_NN_spec.md` - Complete specification with problem requirements, I/O format, constraints, test cases
  - `day_NN_plan.md` - Technical plan with algorithms, data structures, complexity analysis, implementation notes
  - `day_NN_tasks.md` - Task breakdown with checklist for implementation tracking
- **MUST NOT** skip spec creation - these are essential artifacts
- Must create test data files (`test_1.txt`, `test_2.txt`, etc.) in `days/day_NN/data/` if applicable
- Must populate all required sections, not leave placeholders

## Notes

- Specs are stored per-day in `days/day_NN/specs/` (not in root `specs/` folder)
- Templates can be customized in `.specify/templates/`

## Example

```
/create_specs 05
```

This creates spec files in `days/day_05/specs/`.
