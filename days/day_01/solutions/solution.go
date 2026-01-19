package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func solve(lines []string) (string, string) {
	// Part 1: Count times dial ends at 0 after a rotation
	position := 50
	countPart1 := 0

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		direction := line[0]
		distance, _ := strconv.Atoi(line[1:])

		// Apply rotation
		if direction == 'L' {
			position = ((position - distance) % 100 + 100) % 100
		} else { // direction == 'R'
			position = (position + distance) % 100
		}

		// Check if ended at 0
		if position == 0 {
			countPart1++
		}
	}

	// Part 2: Count times dial is at 0 during entire process
	position = 50
	countPart2 := 0

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		direction := line[0]
		distance, _ := strconv.Atoi(line[1:])

		startPos := position

		// Check each click position during rotation
		for click := 1; click <= distance; click++ {
			var clickPos int
			if direction == 'L' {
				clickPos = ((startPos - click) % 100 + 100) % 100
			} else { // direction == 'R'
				clickPos = (startPos + click) % 100
			}

			if clickPos == 0 {
				countPart2++
			}
		}

		// Update position after rotation
		if direction == 'L' {
			position = ((position - distance) % 100 + 100) % 100
		} else {
			position = (position + distance) % 100
		}
	}

	return strconv.Itoa(countPart1), strconv.Itoa(countPart2)
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
