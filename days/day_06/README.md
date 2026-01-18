# Day 06

[Trash Compactor](https://adventofcode.com/2025/day/6)

## Part 1

### Problem
After helping the Elves in the kitchen, you were taking a break and helping them re-enact a movie scene when you over-enthusiastically jumped into the garbage chute! A brief fall later, you find yourself in a garbage smasher. Unfortunately, the door's been magnetically sealed.

As you try to find a way out, you are approached by a family of cephalopods! They're pretty sure they can get the door open, but it will take some time. While you wait, they're curious if you can help the youngest cephalopod with her math homework.

Cephalopod math doesn't look that different from normal math. The math worksheet (your puzzle input) consists of a list of problems; each problem has a group of numbers that need to be either added (`+`) or multiplied (`*`) together.

For example, consider this worksheet:

```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

This worksheet contains four problems:
- The first problem has numbers 123, 45, and 6, and the operator is `*`. So the answer is `123 * 45 * 6 = 33,210`.
- The second problem has numbers 328, 64, and 98, and the operator is `+`. So the answer is `328 + 64 + 98 = 490`.
- The third problem has numbers 51, 387, and 215, and the operator is `*`. So the answer is `51 * 387 * 215 = 4,243,455`.
- The fourth problem has numbers 64, 23, and 314, and the operator is `+`. So the answer is `64 + 23 + 314 = 401`.

Adding all these answers together: `33,210 + 490 + 4,243,455 + 401 = 4,277,556`.

Solve the problems on the math worksheet. What is the grand total found by adding together all of the answers to the individual problems?

### Solution
5524274308182

## Part 2

### Problem
The big cephalopods come back to check on how things are going. When they see that your grand total doesn't match the one expected by the worksheet, they realize they forgot to explain how to read cephalopod math.

Cephalopod math is written **right-to-left in columns**. Each number is given in its own column, with the most significant digit at the top and the least significant digit at the bottom. (Problems are still separated with a column consisting only of spaces, and the symbol at the bottom of the problem is still the operator to use.)

Here's the example worksheet again:

```
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
```

Reading the problems right-to-left one column at a time, the problems are now quite different:

- The **rightmost** problem: Looking at the rightmost column (just before the `+`), you read down: `6`, then `4`, then `4` at the bottom. But wait - cephalopod numbers are written with the most significant digit at the top! So reading this rightmost column from top to bottom gives you the digits `6`, `4`, `4`, which forms the number `644`. However, since we're reading right-to-left, the rightmost column might represent different numbers. Actually, looking more carefully: the rightmost problem spans the last operator `+`. The columns before it (separated by space columns) contain: column with `64` on first line, `23` on second line, `314` on third line. Reading each column top-to-bottom:
  - Column 1: digits `6`, `4`, `4` → `644`? Or more carefully: the problem boundaries are determined by space columns. Let's re-read more carefully: the rightmost problem has operator `+` at the bottom. The numbers in this problem are read column by column from right to left. Each column contains digits, with most significant digit at top. So the rightmost column of digits gives us `6` (top), `4` (middle), `4` (bottom) = `644`. But wait, the actual example says the rightmost problem is `4 + 431 + 623 = 1058`.

Let me reconsider: The problems are separated by space columns. Reading right-to-left, each problem's numbers are constructed column-by-column. For the rightmost problem (operator `+`):
- The rightmost column of digits: `6` (top line), `4` (second line), `4` (third line) → reading top-to-bottom gives `644`? No, that doesn't match.

Actually, based on the description: "The **rightmost** problem is `4 + 431 + 623 = 1058`." This suggests that within the rightmost problem area, we need to identify multiple numbers. Each number is read from a set of adjacent columns, with digits stacked vertically.

Re-reading more carefully: For the rightmost problem with operator `+`, we need to identify groups of columns that form numbers. Reading from right to left within that problem:
- Rightmost column group: digits form one number
- Next column group to the left: digits form another number  
- And so on

But the example given is `4 + 431 + 623 = 1058`. So we have three numbers: `4`, `431`, and `623`.

Looking at the rightmost problem columns (before the `+`):
```
64
23
314
```

If we read columns right-to-left and build numbers:
- Rightmost column: `6`, `4`, `4` → but we need `4` somehow...
- Actually, maybe numbers span multiple columns? Let me think differently.

Given the answer `4 + 431 + 623 = 1058`, and seeing the pattern in the worksheet, it seems the rightmost problem area contains multiple numbers, each constructed from adjacent digit columns.

For the rightmost problem:
- Rightmost columns form `4` 
- Next columns form `431`
- Leftmost columns of that problem form `623`

So: `4 + 431 + 623 = 1058`.

For the second problem from right (operator `*`):
- Reading right-to-left, columns form `175`, `581`, `32`
- So: `175 * 581 * 32 = 3,253,600`

For the third problem from right (operator `+`):
- Reading right-to-left, columns form `8`, `248`, `369`  
- So: `8 + 248 + 369 = 625`

For the leftmost problem (operator `*`):
- Reading right-to-left, columns form `356`, `24`, `1`
- So: `356 * 24 * 1 = 8,544`

The grand total is: `1058 + 3253600 + 625 + 8544 = 3,263,827`.

**Solve the problems on the math worksheet again. What is the grand total found by adding together all of the answers to the individual problems?**

### Solution
8843673199391
