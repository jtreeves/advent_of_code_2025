package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Check if ID is invalid for Part 1: exactly two identical sequences
func isInvalidPart1(idStr string) bool {
	n := len(idStr)
	if n%2 != 0 {
		return false
	}
	half := n / 2
	return idStr[:half] == idStr[half:]
}

// Check if ID is invalid for Part 2: sequence repeated 2+ times
func isInvalidPart2(idStr string) bool {
	n := len(idStr)
	for k := 2; k <= n; k++ {
		if n%k == 0 {
			seqLen := n / k
			pattern := idStr[:seqLen]
			// Build string by repeating pattern k times
			var repeated strings.Builder
			for i := 0; i < k; i++ {
				repeated.WriteString(pattern)
			}
			if idStr == repeated.String() {
				return true
			}
		}
	}
	return false
}

// Parse a range string like "start-end"
func parseRange(rangeStr string) (int, int) {
	parts := strings.Split(rangeStr, "-")
	start, _ := strconv.Atoi(strings.TrimSpace(parts[0]))
	end, _ := strconv.Atoi(strings.TrimSpace(parts[1]))
	return start, end
}

// Parse a line of comma-separated ranges
func parseRanges(line string) [][2]int {
	ranges := [][2]int{}
	for _, rangeStr := range strings.Split(line, ",") {
		rangeStr = strings.TrimSpace(rangeStr)
		if rangeStr != "" {
			start, end := parseRange(rangeStr)
			ranges = append(ranges, [2]int{start, end})
		}
	}
	return ranges
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	var part1Sum int64 = 0
	var part2Sum int64 = 0
	
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		ranges := parseRanges(line)
		
		for _, r := range ranges {
			start, end := r[0], r[1]
			for num := start; num <= end; num++ {
				idStr := strconv.Itoa(num)
				
				if isInvalidPart1(idStr) {
					part1Sum += int64(num)
				}
				
				if isInvalidPart2(idStr) {
					part2Sum += int64(num)
				}
			}
		}
	}
	
	return fmt.Sprintf("%d", part1Sum), fmt.Sprintf("%d", part2Sum)
}

func main() {
	data, err := os.ReadFile("../data/input.txt")
	if err != nil {
		panic(err)
	}
	part1, part2 := solve(string(data))
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
