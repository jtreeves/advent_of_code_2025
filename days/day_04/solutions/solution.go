package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func countNeighbors(grid []string, i, j, rows, cols int) int {
	count := 0
	for di := -1; di <= 1; di++ {
		for dj := -1; dj <= 1; dj++ {
			if di == 0 && dj == 0 {
				continue
			}
			ni, nj := i+di, j+dj
			if ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@' {
				count++
			}
		}
	}
	return count
}

func solve(lines []string) (string, string) {
	rows := len(lines)
	cols := 0
	if rows > 0 {
		cols = len(lines[0])
	}

	// Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
	part1Count := 0
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			if lines[i][j] == '@' {
				neighbors := countNeighbors(lines, i, j, rows, cols)
				if neighbors < 4 {
					part1Count++
				}
			}
		}
	}

	// Part 2: Iteratively remove accessible rolls until none can be removed
	grid := make([][]byte, rows)
	for i := range grid {
		grid[i] = make([]byte, cols)
		copy(grid[i], lines[i])
	}

	part2Count := 0
	for {
		toRemove := [][2]int{}
		for i := 0; i < rows; i++ {
			for j := 0; j < cols; j++ {
				if grid[i][j] == '@' {
					// Convert grid to string slice for neighbor counting
					gridStr := make([]string, rows)
					for k := range grid {
						gridStr[k] = string(grid[k])
					}
					neighbors := countNeighbors(gridStr, i, j, rows, cols)
					if neighbors < 4 {
						toRemove = append(toRemove, [2]int{i, j})
					}
				}
			}
		}

		if len(toRemove) == 0 {
			break
		}

		// Remove all marked positions
		for _, pos := range toRemove {
			grid[pos[0]][pos[1]] = '.'
		}
		part2Count += len(toRemove)
	}

	return strconv.Itoa(part1Count), strconv.Itoa(part2Count)
}

func main() {
	file, err := os.Open("../data/input.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()
	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, strings.TrimSpace(scanner.Text()))
	}
	if err := scanner.Err(); err != nil {
		panic(err)
	}

	part1, part2 := solve(lines)
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
