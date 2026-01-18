# Day 03

[Lobby](https://adventofcode.com/2025/day/3)

## Part 1

### Problem
You descend a short staircase, enter the surprisingly vast lobby, and are quickly cleared by the security checkpoint. When you get to the main elevators, however, you discover that each one has a red light above it: they're all offline.

"Sorry about that," an Elf apologizes as she tinkers with a nearby control panel. "Some kind of electrical surge seems to have fried them. I'll try to get them online soon."

You explain your need to get further underground. "Well, you could at least take the escalator down to the printing department, not that you'd get much further than that without the elevators working. That is, you could if the escalator weren't also offline."

"But, don't worry! It's not fried; it just needs power. Maybe you can get it running while I keep working on the elevators."

There are batteries nearby that can supply emergency power to the escalator for just such an occasion. The batteries are each labeled with their joltage rating, a value from 1 to 9. You make a note of their joltage ratings (your puzzle input). The batteries are arranged into banks; each line of digits in your input corresponds to a single bank of batteries. Within each bank, you need to turn on exactly two batteries; the joltage that the bank produces is equal to the number formed by the digits on the batteries you've turned on. For example, if you have a bank like `12345` and you turn on batteries `2` and `4`, the bank would produce `24` jolts. (You cannot rearrange batteries.)

You'll need to find the largest possible joltage each bank can produce. In the above example:

- In `987654321111111`, you can make the largest joltage possible, `98`, by turning on the **first two** batteries.  
- In `811111111111119`, you can make the largest joltage possible by turning on the batteries labeled `8` and `9`, producing `89`.  
- In `234234234234278`, you can make `78` by turning on the **last two** batteries (marked `7` and `8`).  
- In `818181911112111`, the largest joltage you can produce is `92`.  

The total output joltage is the sum of the maximum joltage from each bank, so in this example, the total output joltage is `98 + 89 + 78 + 92 = 357`.

What is the total output joltage?

### Solution
17535

## Part 2

### Problem
You're sure that's the right total output joltage, but the escalator won't start. You check the control panel, but nothing seems wrong. You build a snowman while you think.

As you're rolling the snowballs for your snowman, you find another instruction manual that must have fallen into the snow:

"Due to newer power requirements, please use joltage method 0x42415454455259 until further notice."

You remember from the training seminar that "method 0x42415454455259" means you're actually supposed to turn on exactly **twelve** batteries in each bank (instead of two) to form a twelve-digit number representing the bank's joltage output.

So, for each bank, you need to find the largest possible twelve-digit number you can form by turning on exactly twelve batteries in order.

What is the total output joltage using this new method?

### Solution
173577199527257
