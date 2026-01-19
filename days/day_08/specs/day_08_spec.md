# Specification: Day 08

## Problem Statement

The facility has a collection of junction boxes positioned in 3D space. Each junction box has coordinates (x, y, z). You need to connect pairs of boxes to form circuits using the closest pairs first. As connections are made, you'll track components (circuits), their sizes, etc.

## Requirements

### Part 1
- Parse 3D coordinates of all junction boxes from input file
- Compute all pairwise distances between boxes (Euclidean distance in 3D space)
- Sort all pairs by increasing distance
- Process exactly the first 1000 pairs (smallest distances) and "connect" those corresponding boxes
- Each connection attempt counts toward the 1000 limit, even if boxes are already in the same circuit
- After 1000 connection attempts, determine the size of each circuit (connected component)
- Find the three largest circuits
- Multiply their sizes together
- Return that product

### Part 2
- Parse 3D coordinates of all junction boxes from input file
- Compute all pairwise distances between boxes (Euclidean distance in 3D space)
- Sort all pairs by increasing distance
- Continue connecting pairs in order of increasing distance until all boxes are in a single circuit (one connected component)
- Identify the final connection pair (the pair that caused the last merge, completing the single circuit)
- Multiply the X coordinates of the two boxes in that final connection pair together
- Return that product

## Input Format

A text file with one 3D coordinate per line. Each line has three integers separated by commas: `x,y,z`

Example:
```
0,0,0
1,1,1
2,2,2
3,3,3
0,1,2
1,2,3
```

## Output Format

For Part 1: A single integer representing the product of the sizes of the three largest circuits after 1000 connections.

For Part 2: A single integer representing the product of the X coordinates of the two boxes in the final connection pair.

## Constraints

- Coordinates can be any integers (positive, negative, or zero)
- Euclidean distance in 3D: distance = sqrt((x2-x1)² + (y2-y1)² + (z2-z1)²)
- When comparing distances, you can use squared distances to avoid floating-point precision issues
- All connection attempts count toward the 1000 limit in Part 1, even if they don't change the circuit structure
- For Part 2, you must continue until exactly one connected component exists
- Use appropriate data types for large numbers (distances squared, circuit sizes, products)
- The number of boxes can be large, so efficient algorithms (like Union-Find) are recommended

## Test Cases

### Test Case 1 (Simple Example)
**Input** (`test_1.txt`):
```
0,0,0
1,0,0
2,0,0
3,0,0
```

**Expected Output Part 1**: For this example with 4 boxes and only 10 connections (fewer than 1000), we need to compute manually. If we process all 6 pairs (4 choose 2), all boxes will be connected. With only 4 boxes total, the three largest circuits would be 4, and the product would be 4 (but we need exactly 3, so this is a simplified case). Actually, we should create a test case that works with 1000 connections or fewer.

**Note**: Let's create a more realistic test case with more boxes to demonstrate the 1000 connection limit.

### Test Case 2 (Small with 1000 Connection Limit)
**Input** (`test_2.txt`):
```
0,0,0
1,0,0
2,0,0
3,0,0
4,0,0
5,0,0
6,0,0
7,0,0
8,0,0
9,0,0
```

**Expected Output Part 1**: With 10 boxes in a line, there are 45 pairs total. Processing the first 9 connections (smallest distances) connects all boxes into one circuit of size 10. The three largest circuits would be: 10, and then we need two more. Actually, with all boxes connected, there's only one circuit. So the product of the three largest would involve counting 10 three times? No, let me reconsider...

Actually, after 1000 connections (or all connections if there are fewer than 1000 pairs), we need the sizes of all circuits. If all boxes are connected, there's only one circuit of size 10. The three largest would be: 10, 0, 0, but that doesn't make sense. The problem asks for the three largest, implying there might be multiple circuits. 

Let me refine: In Part 1, we only process 1000 connections. If there are more than 1000 pairs total, we won't connect everything. Let me create a test case with enough boxes that not all pairs are processed.

### Test Case 3 (Realistic with Multiple Circuits)
**Input** (`test_3.txt`):
```
0,0,0
1,0,0
10,0,0
11,0,0
20,0,0
21,0,0
```

**Expected Output Part 1**: With 6 boxes, there are 15 pairs total. The pairs sorted by distance:
- (0,0,0)-(1,0,0): distance 1
- (10,0,0)-(11,0,0): distance 1
- (20,0,0)-(21,0,0): distance 1
- (1,0,0)-(10,0,0): distance 9
- (11,0,0)-(20,0,0): distance 9
- And other longer distances...

After the first 3 connections (if we're only doing 3 for this test), we'd have 3 circuits of size 2 each. The three largest would be: 2, 2, 2, product = 8.

However, we need to test with the actual requirement of 1000 connections. Let me create test cases that will work properly.

### Test Case 4 (From README pattern)
**Input** (`test_4.txt`): Using a pattern similar to described examples
```
0,0,0
1,1,1
2,2,2
3,3,3
0,1,2
1,2,3
2,3,4
3,4,5
4,5,6
5,6,7
```

**Expected Output Part 1**: [Will be determined by running solution]
**Expected Output Part 2**: [Will be determined by running solution]

### Test Case 5 (Edge Case - Few Boxes)
**Input** (`test_5.txt`):
```
0,0,0
100,0,0
0,100,0
0,0,100
```

**Expected Output Part 1**: With 4 boxes, there are 6 pairs. If we process only the first connection (closest pair), we get one circuit of size 2, and two circuits of size 1. The three largest: 2, 1, 1, product = 2.
**Expected Output Part 2**: After all connections, all 4 boxes are in one circuit. The final connection would be the one that merged the last two separate circuits. The product of X coordinates would depend on which pair that was.
