# /get_input Command

Fetches the input file for a specific day from Advent of Code and overwrites the `input.txt` file.

## Usage

```
/get_input [day_number]
```

If no day number is provided, uses the current day (based on date).

## Behavior

1. Calls `get_input.sh` script to fetch input from AOC
2. Overwrites `days/day_NN/data/input.txt` with the fetched content
3. Creates the directory structure if it doesn't exist

## Critical Requirements

- **MUST** download the actual input file from AOC using the session cookie
- **MUST** save the fetched content to `days/day_NN/data/input.txt`
- **MUST NOT** leave placeholder text like `<official input here>` or similar
- **MUST** verify the file contains actual input data (check file size, content patterns)
- Uses `.specify/scripts/bash/get_session_cookie.sh` to get session cookie
- Uses `.specify/scripts/bash/get_input.sh` to fetch the input

## Prerequisites

- Session cookie must be configured (via `get_session_cookie.sh` or `.env` file)
- The day's problem must be available on AOC

## Example

```
/get_input 05
```

This fetches the input for Day 05 and saves it to `days/day_05/data/input.txt`.
