package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"utils"
)

type Problem struct {
	startCol int
	endCol   int
	op       byte
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	if len(lines) == 0 {
		return "0", "0"
	}

	// Find maximum line length and pad all lines
	maxLen := 0
	for _, line := range lines {
		if len(line) > maxLen {
			maxLen = len(line)
		}
	}
	paddedLines := make([]string, len(lines))
	for i, line := range lines {
		paddedLines[i] = line + strings.Repeat(" ", maxLen-len(line))
	}

	// Operator row is the last row
	opRowIdx := len(paddedLines) - 1
	opRow := paddedLines[opRowIdx]
	numRows := paddedLines[:opRowIdx]

	// Part 1: Parse horizontally
	part1Total := int64(0)

	// Find problem boundaries (columns that are all spaces)
	isSpaceCol := make([]bool, maxLen)
	for col := 0; col < maxLen; col++ {
		allSpaces := true
		for _, line := range paddedLines {
			if col < len(line) && line[col] != ' ' {
				allSpaces = false
				break
			}
		}
		isSpaceCol[col] = allSpaces
	}

	// Group columns into problems
	var problems []Problem
	i := 0
	for i < maxLen {
		if !isSpaceCol[i] {
			// Start of a problem
			startCol := i
			for i < maxLen && !isSpaceCol[i] {
				i++
			}
			endCol := i
			// Extract operator for this problem
			var op byte
			for j := startCol; j < endCol; j++ {
				if opRow[j] == '+' || opRow[j] == '*' {
					op = opRow[j]
					break
				}
			}
			if op != 0 {
				problems = append(problems, Problem{startCol: startCol, endCol: endCol, op: op})
			}
		} else {
			i++
		}
	}

	// Solve Part 1: Extract numbers horizontally
	for _, prob := range problems {
		var numbers []int64
		// Extract numbers from each row in this problem area
		for _, row := range numRows {
			// Extract the substring for this problem
			if prob.endCol > len(row) {
				continue
			}
			problemStr := strings.TrimSpace(row[prob.startCol:prob.endCol])
			// Split by spaces and parse numbers
			parts := strings.Fields(problemStr)
			for _, part := range parts {
				if num, err := strconv.ParseInt(part, 10, 64); err == nil {
					numbers = append(numbers, num)
				}
			}
		}

		// Apply operator
		if len(numbers) > 0 {
			var result int64
			if prob.op == '+' {
				result = 0
				for _, num := range numbers {
					result += num
				}
			} else { // op == '*'
				result = 1
				for _, num := range numbers {
					result *= num
				}
			}
			part1Total += result
		}
	}

	// Part 2: Parse vertically (columns, right-to-left)
	// Approach: Transpose problem area, then parse numbers from transposed rows
	part2Total := int64(0)
	
	for _, prob := range problems {
		// Build transposed representation (columns become rows)
		transposed := []string{}
		for col := prob.startCol; col < prob.endCol; col++ {
			if isSpaceCol[col] {
				continue
			}
			colStr := ""
			for _, row := range numRows {
				if col < len(row) {
					ch := row[col]
					if ch >= '0' && ch <= '9' || ch == ' ' {
						colStr += string(ch)
					}
				}
			}
			if colStr != "" {
				transposed = append(transposed, strings.TrimSpace(colStr))
			}
		}
		
		// Parse numbers from each transposed row (reading right-to-left means reverse order)
		var numbers []int64
		for i := len(transposed) - 1; i >= 0; i-- {
			colStr := strings.ReplaceAll(transposed[i], " ", "")
			if colStr != "" {
				if num, err := strconv.ParseInt(colStr, 10, 64); err == nil {
					numbers = append(numbers, num)
				}
			}
		}
		
		// Apply operator
		if len(numbers) > 0 {
			var result int64
			if prob.op == '+' {
				result = 0
				for _, num := range numbers {
					result += num
				}
			} else {
				result = 1
				for _, num := range numbers {
					result *= num
				}
			}
			part2Total += result
		}
	}

	return fmt.Sprintf("%d", part1Total), fmt.Sprintf("%d", part2Total)
}

func main() {
	inputData, err := utils.ReadInputRaw("../data/input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
		os.Exit(1)
	}
	part1, part2 := solve(inputData)
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
