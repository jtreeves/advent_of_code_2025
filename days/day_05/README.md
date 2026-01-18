# Day 05

[Cafeteria](https://adventofcode.com/2025/day/5)

## Part 1

### Problem

The Elves are working on a cafeteria system for the North Pole base. They need to track which ingredients are fresh based on ingredient ID ranges.

The puzzle input contains two sections:

1. **Fresh ingredient ID ranges** - Lines like `3-5`, `10-14`, `12-18`, etc. Each range defines a continuous interval of "fresh" ingredient IDs. Ranges are inclusive on both ends (e.g., `3-5` means IDs 3, 4, and 5 are all fresh).

2. **Specific ingredient IDs** - A separate list of ingredient ID numbers that need to be checked.

For example, suppose your input contains:

```
3-5
10-14
12-18
16-20

1
5
8
11
17
32
```

In this example:
- The fresh ranges are `3-5`, `10-14`, `12-18`, and `16-20`
- The IDs to check are `1`, `5`, `8`, `11`, `17`, and `32`

Your task is to count how many of the given specific ingredient IDs fall into **any** of the fresh ID ranges.

In the example above:
- ID `1` is not in any range
- ID `5` is in range `3-5`
- ID `8` is not in any range
- ID `11` is in range `10-14`
- ID `17` is in range `12-18` (also in `16-20`)
- ID `32` is not in any range

So the answer would be **3** (IDs 5, 11, and 17 are fresh).

Analyze your input and count how many of the specific ingredient IDs are fresh.

### Solution
525

## Part 2

### Problem

The Elves realize that they need to know the total coverage of fresh ingredients, not just which specific IDs are fresh. They want to know: what is the total number of unique ingredient IDs covered by all the fresh ranges combined?

This means you need to find the **union** of all fresh ranges - merging overlapping ranges and counting the total number of unique IDs covered.

For example, using the same ranges as before:
- `3-5` covers IDs 3, 4, 5 (3 IDs)
- `10-14` covers IDs 10, 11, 12, 13, 14 (5 IDs)
- `12-18` covers IDs 12, 13, 14, 15, 16, 17, 18 (7 IDs)
- `16-20` covers IDs 16, 17, 18, 19, 20 (5 IDs)

These ranges overlap:
- `10-14` and `12-18` overlap (12, 13, 14 are in both)
- `12-18` and `16-20` overlap (16, 17, 18 are in both)

When merged, the ranges become:
- `3-5` covers IDs 3, 4, 5
- `10-20` covers IDs 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 (merged from `10-14`, `12-18`, and `16-20`)

Total unique IDs covered: 3 + 11 = **14**.

The algorithm for merging ranges:
1. Sort all ranges by their start value
2. Walk through the sorted ranges, merging any overlapping or adjacent ranges
3. Sum the lengths of all merged, non-overlapping intervals

**Important notes:**
- Ranges are inclusive on both ends
- Two ranges overlap if one contains any ID from the other
- Two ranges are adjacent if the end of one equals the start of the other minus 1 (e.g., `10-14` and `15-20` are adjacent and should merge to `10-20`)
- Be careful with off-by-one errors when calculating interval lengths

What is the total number of unique ingredient IDs covered by all the fresh ranges in your input?

### Solution
333892124923577
