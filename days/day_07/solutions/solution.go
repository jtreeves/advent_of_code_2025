package main

import (
	"fmt"
	"os"
	"strings"
)

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	if len(lines) == 0 {
		return "0", "0"
	}
	
	grid := make([][]byte, len(lines))
	for i, line := range lines {
		grid[i] = []byte(line)
	}
	rows := len(grid)
	cols := 0
	if rows > 0 {
		cols = len(grid[0])
	}
	
	// Find starting position S
	startRow, startCol := -1, -1
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 'S' {
				startRow, startCol = r, c
				break
			}
		}
		if startRow != -1 {
			break
		}
	}
	
	if startRow == -1 {
		return "0", "0"
	}
	
	// Part 1: Count total splits
	splitCount := 0
	activeBeams := make(map[int]bool)
	activeBeams[startCol] = true
	
	// Process each row starting from the row after S
	for r := startRow + 1; r < rows; r++ {
		nextBeams := make(map[int]bool)
		for col := range activeBeams {
			if grid[r][col] == '.' {
				// Beam continues down
				nextBeams[col] = true
			} else if grid[r][col] == '^' {
				// Beam splits
				splitCount++
				// Add beams to left and right
				if col-1 >= 0 {
					nextBeams[col-1] = true
				}
				if col+1 < cols {
					nextBeams[col+1] = true
				}
			}
		}
		activeBeams = nextBeams
	}
	
	// Part 2: Count beams reaching bottom row
	beamCounts := make([][]int, rows)
	for r := range beamCounts {
		beamCounts[r] = make([]int, cols)
	}
	beamCounts[startRow][startCol] = 1 // Start with 1 beam at S
	
	// Process each row starting from the row after S
	for r := startRow + 1; r < rows; r++ {
		for c := 0; c < cols; c++ {
			prevCount := beamCounts[r-1][c]
			if prevCount > 0 {
				if grid[r][c] == '.' {
					// Beam continues down
					beamCounts[r][c] += prevCount
				} else if grid[r][c] == '^' {
					// Beam splits into left and right
					if c-1 >= 0 {
						beamCounts[r][c-1] += prevCount
					}
					if c+1 < cols {
						beamCounts[r][c+1] += prevCount
					}
				}
			}
		}
	}
	
	// Sum all beams in bottom row
	bottomBeamCount := 0
	for c := 0; c < cols; c++ {
		bottomBeamCount += beamCounts[rows-1][c]
	}
	
	return fmt.Sprintf("%d", splitCount), fmt.Sprintf("%d", bottomBeamCount)
}

func main() {
	inputData, err := os.ReadFile("../data/input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}
	part1, part2 := solve(string(inputData))
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
