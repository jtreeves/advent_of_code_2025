# Specification: Day 11

## Problem Statement

You're examining a large toroidal reactor that should communicate with a new server rack. The input describes a directed graph of devices: each line lists a device followed by its output connections. Data flows only forward (from any given device, through its listed outputs), never backwards.

## Requirements

### Part 1
- Parse the input file containing device definitions
- Build a directed graph representation where devices are nodes and outputs are edges
- Find the device labeled `you` (starting point)
- Find the device labeled `out` (ending point)
- Count **every possible path** from `you` to `out` in the directed graph
- Return the total count of distinct paths

### Part 2
- Parse the same input file format
- Find the device labeled `svr` (starting point)
- Find the device labeled `out` (ending point)
- Count all paths from `svr` to `out` that **include both `fft` and `dac`** somewhere along the path
- Paths can include `fft` and `dac` in any order, but for the specific puzzle input, all valid paths hit `fft` before `dac`
- Return the total count of paths that satisfy this condition

## Input Format

Each line in the input file has the format:
```
<device_name>: <output1> <output2> ... <outputN>
```

- `<device_name>` is a label for the device
- The colon (`:`) separates the device name from its outputs
- `<output1>`, `<output2>`, etc. are space-separated device names that this device connects to
- A device may have zero or more outputs (if no outputs, just the device name and colon)
- Device names are alphanumeric strings

Example:
```
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
```

## Output Format

For Part 1: A single integer representing the number of distinct paths from `you` to `out`.

For Part 2: A single integer representing the number of distinct paths from `svr` to `out` that include both `fft` and `dac`.

## Constraints

- The graph is directed (data flows forward only, never backwards)
- The graph may contain cycles, but cycles cannot be part of a valid path (they would create infinite loops)
- Device names are unique identifiers
- The graph is guaranteed to have paths from the starting point to the ending point (for both parts)
- For Part 2, the graph is structured such that `fft` always comes before `dac` in valid paths
- Path counts can be very large - use appropriate data types (potentially big integers or long integers)
- There is exactly one `you` device in Part 1
- There is exactly one `svr` device in Part 2
- There is exactly one `out` device
- For Part 2, there is exactly one `fft` device and one `dac` device

## Test Cases

### Test Case 1 (Example from README - Part 1)
**Input** (`test_1.txt`):
```
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
```

**Expected Output Part 1**: 5

**Explanation**: 
The paths from `you` to `out` are:
- `you → bbb → ddd → ggg → out`
- `you → bbb → eee → out`
- `you → ccc → ddd → ggg → out`
- `you → ccc → eee → out`
- `you → ccc → fff → out`

Total: 5 paths.

**Expected Output Part 2**: N/A (different starting point and requirements)

### Test Case 2 (Simple linear path)
**Input** (`test_2.txt`):
```
start: mid
mid: end
end: out
```

**Expected Output Part 1**: 1

**Explanation**: Only one path: `start → mid → end → out`. However, this assumes `start` is labeled `you`. For actual Part 1, if we rename:
```
you: mid
mid: end
end: out
```
Then Part 1 result is 1.

**Note**: This test case uses the Part 1 format. For Part 2 testing, we'd need `svr`, `fft`, `dac`, and `out` in appropriate positions.

### Test Case 3 (Branching paths)
**Input** (`test_3.txt`):
```
you: a b
a: out
b: out
```

**Expected Output Part 1**: 2

**Explanation**: Two paths:
- `you → a → out`
- `you → b → out`

### Test Case 4 (Multiple levels of branching)
**Input** (`test_4.txt`):
```
you: a b
a: c d
b: c d
c: out
d: out
```

**Expected Output Part 1**: 4

**Explanation**: Four paths:
- `you → a → c → out`
- `you → a → d → out`
- `you → b → c → out`
- `you → b → d → out`

### Test Case 5 (Path with intermediate nodes for Part 2)
**Input** (`test_5.txt`):
```
svr: a b
a: fft
b: c
c: dac
fft: dac out
dac: out
```

**Expected Output Part 2**: 3

**Explanation**: Paths from `svr` to `out` that include both `fft` and `dac`:
- `svr → a → fft → dac → out`
- `svr → a → fft → out` (does NOT include dac - invalid)
- `svr → b → c → dac → out` (does NOT include fft - invalid)
- Valid paths: `svr → a → fft → dac → out`

Wait, this doesn't have 3 paths. Let me reconsider...

Actually, for Part 2, we need paths that include BOTH `fft` and `dac`. So:
- `svr → a → fft → dac → out` ✓ (includes both)

This is just 1 path. Let me create a better example:

Better example:
```
svr: fft a
a: fft
fft: dac b
b: dac
dac: out
```

Valid paths:
- `svr → fft → dac → out` ✓
- `svr → a → fft → dac → out` ✓

That's 2 paths. Let me adjust to get 3:
```
svr: fft a b
a: fft
b: fft
fft: dac
dac: out
```

Valid paths:
- `svr → fft → dac → out` ✓
- `svr → a → fft → dac → out` ✓
- `svr → b → fft → dac → out` ✓

So **Expected Output Part 2**: 3
