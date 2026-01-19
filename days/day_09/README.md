# Day 09

[Movie Theater](https://adventofcode.com/2025/day/9)

## Part 1

### Problem
The Elves have set up a movie theater at the North Pole, and they're planning to show some special holiday films. To make the viewing experience great, they've marked certain seats with red tiles that indicate the best viewing spots.

You're given a list of coordinates representing red tiles placed on a 2D grid. Each coordinate is provided as `x,y` on a single line, where both `x` and `y` are integers.

Your task is to find the largest rectangle (by area) that can be formed using two red tiles as diagonally opposite corners. The rectangle must be axis-aligned with the grid (meaning its sides are parallel to the x and y axes).

Important details:
- Both corner tiles must be red tiles (from your input)
- The rectangle includes all tiles in its interior and on its edges, but you don't need those interior points to be red tiles—only the two corner tiles matter
- The area of a rectangle with corners at `(x1, y1)` and `(x2, y2)` is calculated as: `(|x1 - x2| + 1) * (|y1 - y2| + 1)`

For example, if the red tiles are at `(0,0)`, `(3,0)`, `(0,2)`, and `(3,2)`, you could form a rectangle with corners at `(0,0)` and `(3,2)`, which would have an area of `(3 - 0 + 1) * (2 - 0 + 1) = 4 * 3 = 12`.

What is the area of the largest rectangle you can form using two red tiles as opposite corners?

### Solution
[Answer will be populated after running solutions]

## Part 2

### Problem
After setting up the red tiles, the Elves realize they need to mark a boundary around the seating area. They connect the red tiles sequentially (in the order they appear in the input) with green tiles, forming a closed loop or polygon. The region inside this boundary (including the boundary itself) is considered valid seating space.

Now, you need to find the largest rectangle (by area) that can be formed using two red tiles as diagonally opposite corners, with an additional constraint: the entire rectangle must lie entirely within or on the boundary of the polygon formed by the red and green tiles.

This means:
- The rectangle must be constructed from two red tile corners (same as Part 1)
- All tiles in the rectangle (interior and edges) must be within or on the polygon boundary—no part of the rectangle can extend outside
- The polygon is formed by connecting sequential red tiles (in input order) with horizontal and vertical line segments made of green tiles

The challenge here is efficiently checking whether a rectangle is fully contained within the polygon. You need to verify that:
- All four corners of the rectangle are inside or on the boundary
- All edges of the rectangle are inside or on the boundary
- All interior points of the rectangle are inside or on the boundary

What is the area of the largest rectangle you can form that is entirely contained within the polygon boundary?

### Solution
[Answer will be populated after running solutions]
