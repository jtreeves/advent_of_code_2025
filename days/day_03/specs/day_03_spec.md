# Specification: Day 03

## Problem Statement

You find yourself in the lobby with broken escalators and elevators. You discover a set of batteries, each labeled with a joltage rating (a digit from 1 to 9). The batteries are arranged into banks; each line of digits in your input corresponds to a single bank of batteries.

## Requirements

### Part 1
- Parse input file where each line is a bank of batteries (a string of digits 1-9)
- For each bank, you must turn on exactly **two** batteries
- The joltage produced by a bank is equal to the number formed by the digits on the batteries you've turned on (in their original order)
- You cannot rearrange batteries - you must select two digits that appear in order within the string
- For each bank, find the **largest possible two-digit number** you can form
- Sum the maximum joltage from each bank and return this sum

### Part 2
- Parse input file (same format as Part 1)
- For each bank, you must turn on exactly **twelve** batteries (instead of two)
- The joltage produced by a bank is equal to the number formed by the twelve digits you've turned on (in their original order)
- You cannot rearrange batteries - you must select twelve digits that appear in order within the string
- For each bank, find the **largest possible twelve-digit number** you can form
- Sum the maximum joltage from each bank and return this sum

## Input Format

A text file with one bank per line. Each line contains a sequence of digits (1-9) with no spaces. The number of digits per line may vary.

Example:
```
987654321111111
811111111111119
234234234234278
818181911112111
```

## Output Format

For Part 1: A single integer representing the sum of the maximum two-digit joltage from each bank.

For Part 2: A single integer representing the sum of the maximum twelve-digit joltage from each bank.

## Constraints

- Each bank contains digits 1-9 only (no zeros)
- Banks can have varying lengths
- For Part 1, banks must have at least 2 digits
- For Part 2, banks must have at least 12 digits
- You must select digits in their original order (cannot rearrange)
- The problem reduces to finding the lexicographically largest subsequence of the required length

## Algorithm Notes

- This is a **greedy algorithm** problem
- For each bank, to find the largest N-digit number:
  - At each position, we want to choose the largest digit possible such that there are still enough digits remaining to complete the required length
  - Specifically: for position i (0-indexed), we need at least (N - i - 1) digits remaining after position i
  - Greedy approach: at each position, pick the maximum digit available in the remaining substring that allows us to complete the sequence

## Test Cases

### Test Case 1 (Part 1 example from problem)
**Input** (`test_1.txt`):
```
987654321111111
811111111111119
234234234234278
818181911112111
```
**Expected Output Part 1**: 357
**Explanation**: 
- `987654321111111`: Largest 2-digit number is `98` (first two digits: positions 0-1)
- `811111111111119`: Largest 2-digit number is `89` (digits at positions 0 and 15: 8 and 9)
- `234234234234278`: Largest 2-digit number is `78` (last two digits: positions 13-14)
- `818181911112111`: Largest 2-digit number is `92` (digits at positions 6 and 10: 9 and 2)
- Sum: 98 + 89 + 78 + 92 = 357

**Expected Output Part 2**: [To be calculated based on greedy algorithm - each bank produces a 12-digit number, sum all 12-digit numbers]

### Test Case 2 (Simple case - Part 1)
**Input** (`test_2.txt`):
```
99
88
77
```
**Expected Output Part 1**: 264
**Explanation**:
- `99`: Largest 2-digit is `99`
- `88`: Largest 2-digit is `88`
- `77`: Largest 2-digit is `77`
- Sum: 99 + 88 + 77 = 264

### Test Case 3 (All same digits - Part 1)
**Input** (`test_3.txt`):
```
11111
22222
```
**Expected Output Part 1**: 33
**Explanation**:
- `11111`: Largest 2-digit is `11` (any two consecutive 1s)
- `22222`: Largest 2-digit is `22` (any two consecutive 2s)
- Sum: 11 + 22 = 33

### Test Case 4 (Descending order - Part 1)
**Input** (`test_4.txt`):
```
987654321
```
**Expected Output Part 1**: 98
**Explanation**:
- `987654321`: Largest 2-digit is `98` (first two digits)

### Test Case 5 (Ascending order - Part 1)
**Input** (`test_5.txt`):
```
123456789
```
**Expected Output Part 1**: 89
**Explanation**:
- `123456789`: Largest 2-digit is `89` (last two digits)

### Test Case 6 (Part 2 - simple case with 12+ digits)
**Input** (`test_6.txt`):
```
987654321123456789
```
**Expected Output Part 1**: 98
**Explanation**: Largest 2-digit is `98` (first two digits)

**Expected Output Part 2**: 987654321123
**Explanation**: Largest 12-digit is `987654321123` (first 12 digits from the 18-digit string)
