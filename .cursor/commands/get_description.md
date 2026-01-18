# /get_description Command

Fetches the problem description for both Part 1 and Part 2 from Advent of Code via Google search and populates the README.md file for the specified day.

## Usage

```
/get_description [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Uses web search to find the problem description for the specified day
2. Retrieves **both Part 1 and Part 2** descriptions simultaneously via Google search
3. Updates `days/day_NN/README.md` with both problem descriptions
4. Preserves existing README structure

## Critical Requirements

- **MUST** fetch the problem description for **both Part 1 and Part 2** at the same time via Google search
- **MUST NOT** attempt to fetch directly from AOC website (only input can be fetched from AOC site)
- **MUST** use web search to find accurate problem descriptions
- **MUST** populate `days/day_NN/README.md` with complete problem statements for both parts
- **MUST** match the structure used in `days/day_01/README.md` as a reference
- Must include all sections: Problem description, examples, solution placeholders

## Prerequisites

- Internet connection for web search
- Advent of Code problem must be publicly available (descriptions can be found via search)

## Example

```
/get_description 05
```

This fetches the problem descriptions for both Part 1 and Part 2 of Day 05 via Google search and updates `days/day_05/README.md`.
