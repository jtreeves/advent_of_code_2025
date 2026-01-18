# /submit_answer Command

Submits an answer to Advent of Code for the specified day and part.

## Usage

```
/submit_answer <day_number> <answer> [part]
```

If `part` is not specified, defaults to Part 1. Can be `1` or `2`.

## Behavior

1. Calls `submit_answer.sh` script to submit the answer to AOC
2. Handles Part 1 and Part 2 automatically based on `part` parameter
3. Parses AOC response to determine success/failure
4. Provides feedback on:
   - Correct answers
   - Incorrect answers
   - Already completed parts
   - Authentication errors

## Prerequisites

- Session cookie must be configured
- Answer must be determined (can be extracted from README if previously run)

## Example

```
/submit_answer 05 12345 1
```

This submits answer `12345` for Day 05 Part 1.

## Integration

Can be combined with `/run_solutions` to automatically submit answers:
1. Run solutions to get answers
2. Submit Part 1 answer
3. Submit Part 2 answer (if Part 1 succeeds)
