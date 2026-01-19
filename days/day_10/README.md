# Day 10

[Factory](https://adventofcode.com/2025/day/10)

## Part 1

### Problem
The Elves have discovered a factory with machines that need to be repaired. Each machine is controlled by indicator lights and buttons.

Each line of your puzzle input describes one machine. A machine description consists of:

1. **Indicator lights pattern** — in square brackets `[...]`. Each character is either `.` (light should be OFF) or `#` (light should be ON). All lights start OFF.
   Example: `[.##.]` means lights 0-3 should be: OFF, ON, ON, OFF.

2. **Buttons** — in multiple sets in parentheses `(...)`. Each button toggles a specified subset of lights. The lights are indexed starting from 0. For example, `(1,3)` means that button toggles lights 1 and 3. Buttons are listed in the same line, after the pattern.

3. **Joltage requirements** — in curly braces `{...}` at the end of each line. These are ignored for Part 1.

When you press a button, it toggles all the lights it affects (ON ↔ OFF). Buttons can be pressed at most once each: pressing twice is equivalent to not pressing at all (because toggling twice returns to the original state). The order of button presses doesn't matter (toggles commute).

What is the fewest number of button presses needed to configure the lights so that they match the target pattern given in `[...]`?

For example, suppose a machine has the following description:

```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

This means:
- Target pattern: lights 0-3 should be OFF, ON, ON, OFF
- Button 0: `(3)` toggles light 3
- Button 1: `(1,3)` toggles lights 1 and 3
- Button 2: `(2)` toggles light 2
- Button 3: `(2,3)` toggles lights 2 and 3
- Button 4: `(0,2)` toggles lights 0 and 2
- Button 5: `(0,1)` toggles lights 0 and 1
- Joltage requirements `{3,5,4,7}` are ignored for Part 1

All lights start OFF. To reach `[.##.]` (OFF, ON, ON, OFF), you need to find the minimum set of button presses.

### Solution
[Answer will be populated after running solutions]

## Part 2

### Problem
You're confident that's the right approach, but the machines still aren't working. You take a closer look and notice that each light also has a joltage meter with a target joltage requirement.

Now the behavior changes:
- Each light has a joltage meter, initially at 0
- Each light has a target joltage value (from the `{...}` list in each machine description)
- Pressing a button now increments the joltage meter of each light it affects by exactly 1
- You can press the same button multiple times (there's no limit like in Part 1)
- Meters cannot go negative

Your goal is to find the minimum total number of button presses (across all buttons, including possibly pressing the same button many times) such that all joltage meters hit their exact target values.

For the same example:
```
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

This means:
- Lights 0-3 have target joltages of 3, 5, 4, and 7 respectively
- You need to find the minimum total button presses to reach exactly these joltage values

Using the joltage method, what is the minimum total button presses needed across all machines?

### Solution
[Answer will be populated after running solutions]
