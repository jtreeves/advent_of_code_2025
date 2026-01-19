# Day 11

[Reactor](https://adventofcode.com/2025/day/11)

## Part 1

### Problem
You hear some loud beeping coming from a hatch in the floor of the factory, so you decide to check it out. Inside, you find several large electrical conduits and a ladder.

Climbing down the ladder, you discover the source of the beeping: a large, toroidal reactor which powers the factory above. Some Elves here are hurriedly running between the reactor and a nearby server rack, apparently trying to fix something.

One of the Elves notices you and rushes over. *"It's a good thing you're here! We just installed a new server rack, but we aren't having any luck getting the reactor to communicate with it!"* You glance around the room and see a tangle of cables and devices running from the server rack to the reactor. She rushes off, returning a moment later with a list of the devices and their outputs (your puzzle input).

For example:

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

Each line gives the name of a device followed by a list of the devices to which its outputs are attached. So, `bbb: ddd eee` means that device `bbb` has two outputs, one leading to device `ddd` and the other leading to device `eee`.

The Elves are pretty sure that the issue isn't due to any specific device, but rather that the issue is triggered by data following some specific path through the devices. Data only ever flows from a device through its outputs; it can't flow backwards.

After dividing up the work, the Elves would like you to focus on the devices starting with the one next to you (an Elf hastily attaches a label which just says `you`) and ending with the main output to the reactor (which is the device with the label `out`).

To help the Elves figure out which path is causing the issue, they need you to find **every path from `you` to `out`**.

In the example above, these are all of the paths from `you` to `out`:

- `you → bbb → ddd → ggg → out`
- `you → bbb → eee → out`
- `you → ccc → ddd → ggg → out`
- `you → ccc → eee → out`
- `you → ccc → fff → out`

In total, there are **5** different paths leading from `you` to `out`.

**Question:** *How many different paths lead from `you` to `out`?*

### Solution
497

## Part 2

### Problem
As you work through counting paths for Part 1, the Elves discover the issue lies in problematic data paths that specifically pass through two designated devices: `fft` and `dac`.

The starting point is now labeled `svr` instead of `you`, and you again aim to end at `out`.

You need to **count all paths from `svr` to `out`** that **include both `fft` and `dac` somewhere along the path** (in **any order**).

A key observation: for your specific puzzle input, **every valid path that includes both `fft` and `dac` hits `fft` *before* `dac`**. That is, no valid path goes through `dac` first then `fft`. This fact can simplify the counting.

**Question:** *How many different paths from `svr` to `out` include both `fft` and `dac`?*

### Solution
358564784931864
