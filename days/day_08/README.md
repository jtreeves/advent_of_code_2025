# Day 08

[Circuit Assembly](https://adventofcode.com/2025/day/8)

## Part 1

### Problem

The facility has a collection of junction boxes positioned in 3D space. Each junction box has coordinates (x, y, z). You need to connect pairs of boxes to form circuits.

Your puzzle input contains the 3D coordinates of all the junction boxes, one per line. Each line has three integers representing the x, y, and z coordinates.

For example:
```
0,0,0
1,1,1
2,2,2
3,3,3
0,1,2
1,2,3
```

To form circuits, you need to:
1. Compute all pairwise distances between boxes (the Euclidean distance in 3D space).
2. Sort all pairs by increasing distance.
3. Process the first 1000 pairs (smallest distances) and "connect" those corresponding boxes.

When you connect two boxes, they form part of the same circuit (connected component). If two boxes are already in the same circuit, the connection attempt still counts toward the 1000 connections, but it doesn't change the circuit structure.

After making 1000 connection attempts, you need to:
- Determine the size of each circuit (connected component)
- Find the three largest circuits
- Multiply their sizes together

That product is your answer for Part 1.

For example, if after 1000 connections you have circuits of sizes 15, 12, 10, 8, 5, 3, then the three largest are 15, 12, and 10, and the answer would be 15 × 12 × 10 = 1800.

**Important**: All connection attempts count toward the 1000 limit, even if the boxes are already in the same circuit. Only process exactly 1000 pairs from the sorted list.

### Solution
32103

## Part 2

### Problem

The Elves realize that for the system to work properly, all junction boxes need to be connected into a single circuit. You should continue connecting pairs in order of increasing distance until all boxes are part of one connected component.

Once all boxes are in a single circuit, identify the final connection that was made (the pair that caused the last merge, completing the single circuit).

For Part 2, you need to compute the answer based on that final connection pair. Specifically, multiply the X coordinates of the two boxes in that final connection pair together.

For example, if the final connection was between boxes at coordinates (5, 10, 15) and (8, 20, 25), then the answer would be 5 × 8 = 40.

Continue connecting pairs from the sorted distance list until all boxes are in one circuit, then find the final connection pair and multiply their X coordinates together.

### Solution
8133642976
