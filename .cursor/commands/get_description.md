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
4. Preserves existing README structure (Usage section, etc.)

## Prerequisites

- Session cookie must be configured (via `get_session_cookie.sh` or `.env` file)

## Example

```
/get_description 05
```

This fetches the problem description for Day 05 and updates `days/day_05/README.md`.
