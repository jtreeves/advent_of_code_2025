# /sync_specs Command

Adjusts spec files to match the resultant code files, ensuring specs reflect actual implementation.

## Usage

```
/sync_specs [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Analyzes all solution files in `days/day_NN/solutions/`
2. Compares against spec files in `days/day_NN/specs/`:
   - `day_NN_spec.md` - Updates requirements to match implementation
   - `day_NN_plan.md` - Updates algorithm descriptions
   - `day_NN_tasks.md` - Marks completed tasks
3. Updates spec files to reflect:
   - Actual algorithms used
   - Data structures implemented
   - Any deviations from original plan
   - Language-specific implementation details

## Use Case

After generating and running solutions, implementations may differ slightly from the original specs. This command ensures specs stay in sync with the actual code.

## Example

```
/sync_specs 05
```

This updates specs for Day 05 to match the actual implementations.
