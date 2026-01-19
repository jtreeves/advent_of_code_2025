# Day 12

[Christmas Tree Farm](https://adventofcode.com/2025/day/12)

## Part 1

### Problem
You're given six (6) distinct present shapes, each defined as a polyomino in a 3×3 grid. Each shape uses `#` to represent filled cells and `.` for empty cells within the 3×3 bounding box. These shapes can be rotated and flipped (reflected).

Then, you're given a list of queries. Each query specifies:
1. A rectangular region size: width × height
2. For each of the 6 shapes, how many copies of that shape must be placed inside the rectangle

For each query, you need to determine whether it's possible to pack all the given shapes into the specified rectangular region without overlapping, allowing rotations and reflections. The goal is to count how many queries (regions) are possible to satisfy.

For example, suppose you have six shapes defined in 3×3 grids, and you're given a query like:
```
40x36: 17 35 30 26 24 23
```

This means a 40×36 rectangle that needs 17 copies of shape 0, 35 copies of shape 1, 30 copies of shape 2, 26 copies of shape 3, 24 copies of shape 4, and 23 copies of shape 5. You need to determine if all these shapes can fit without overlapping.

Note: While this is fundamentally a 2D packing problem (polyomino tiling) which is NP-hard in general, for the actual puzzle input, a simple area check suffices: if the total area occupied by all required shapes (sum of each shape's area × its count) is less than or equal to the area of the rectangular region, then the packing is possible.

What is the number of regions (queries) that are possible to satisfy?

### Solution
531

## Part 2

### Problem
There is no traditional Part 2 challenge for Day 12. Day 12 - "Christmas Tree Farm" is the finale of Advent of Code 2025. After completing Part 1 and having collected all stars from the previous 11 days, you earn the final star. The puzzle ends with a special message about the star appearing above the Christmas tree.

### Solution
Final star
