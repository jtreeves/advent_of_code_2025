# /get_description Command

Fetches the problem description from Advent of Code and populates the README.md file for the specified day.

## Usage

```
/get_description [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Calls `get_problem_description.sh` script to fetch problem description from AOC
2. Updates `days/day_NN/README.md` with the problem description
3. Handles Part 1 initially, then Part 2 when available
4. Preserves existing README structure

## Critical Requirements

- **MUST** fetch the problem description **verbatim** from the AOC website
- **MUST NOT** paraphrase, summarize, or modify the text from AOC
- **MUST** use the exact text as it appears on the AOC site, including:
  - Exact problem statements for Part 1 and Part 2
  - Exact example inputs and outputs
  - Exact explanations and formatting
- **MUST** match the structure and format used in `days/day_01/README.md` as a reference
- Must include all sections: Problem description, examples, solution placeholders

## Prerequisites

- Session cookie must be configured (via `get_session_cookie.sh` or `.env` file)

## Example

```
/get_description 05
```

This fetches the problem description for Day 05 and updates `days/day_05/README.md`.
