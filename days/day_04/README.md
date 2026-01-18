# Day 04

[Printing Department](https://adventofcode.com/2025/day/4)

## Part 1

### Problem
You ride the escalator down to the printing department. They're clearly getting ready for Christmas; they have lots of large rolls of paper everywhere, and there's even a massive printer in the corner (to handle the really big print jobs).

Decorating here will be easy: they can make their own decorations. What you really need is a way to get further into the North Pole base while the elevators are offline.

"Actually, maybe we can help with that," one of the Elves replies when you ask for help. "We're pretty sure there's a cafeteria on the other side of the back wall. If we could break through the wall, you'd be able to keep moving. It's too bad all of our forklifts are so busy moving those big rolls of paper around."

The rolls of paper (`@`) are arranged on a large grid; the Elves even have a helpful diagram (your puzzle input) indicating where everything is located.

The forklifts can only access a roll of paper if there are fewer than four rolls of paper in the eight adjacent positions. If you can figure out which rolls of paper the forklifts can access, they'll spend less time looking and more time breaking down the wall to the cafeteria.

In this example:

```
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
```

The forklifts can only access a roll of paper if among its eight neighbors there are fewer than four rolls of paper (`@`).

For that example grid, 13 rolls of paper are accessible.

Analyze the diagram in your puzzle input. What's the actual number of rolls of paper the forklifts can access?

### Solution
1587

## Part 2

### Problem
You're sure that's the right answer, but the forklifts still seem busy. You wonder if maybe you need to think about this differently.

As you're thinking about it, you realize something: once the forklifts access and remove some rolls, other rolls might become accessible! Maybe you need to keep removing accessible rolls until no more can be removed.

Remove all accessible rolls (those with fewer than four neighbors that are `@`). After removing them, new rolls might become accessible. Keep removing accessible rolls until no more can be removed. How many rolls of paper were removed in total?

### Solution
8946
