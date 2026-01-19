package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	if len(lines) == 0 {
		return "0", "Final star"
	}
	
	// Parse shapes (first 6 shapes, numbered 0-5)
	shapeAreas := make([]int, 6)
	i := 0
	shapeIdx := 0
	
	for i < len(lines) && shapeIdx < 6 {
		line := strings.TrimSpace(lines[i])
		// Check if this is a shape header (format: "number:")
		if len(line) > 0 && strings.HasSuffix(line, ":") {
			shapeNumStr := line[:len(line)-1]
			if shapeNum, err := strconv.Atoi(shapeNumStr); err == nil && shapeNum == shapeIdx {
				// Read the next 3 lines for the shape grid
				shapeGrid := make([]string, 3)
				for j := 0; j < 3; j++ {
					if i+1+j < len(lines) {
						shapeGrid[j] = strings.TrimSpace(lines[i+1+j])
					} else {
						shapeGrid[j] = ""
					}
				}
				
				// Count '#' characters in the shape
				area := 0
				for _, row := range shapeGrid {
					area += strings.Count(row, "#")
				}
				shapeAreas[shapeIdx] = area
				shapeIdx++
				i += 4 // Skip shape header + 3 grid lines + empty line (if present)
				continue
			}
		}
		i++
	}
	
	// Find where queries start (skip empty lines after shapes)
	queryStart := i
	for queryStart < len(lines) && strings.TrimSpace(lines[queryStart]) == "" {
		queryStart++
	}
	
	// Parse queries
	possibleCount := 0
	for lineIdx := queryStart; lineIdx < len(lines); lineIdx++ {
		line := strings.TrimSpace(lines[lineIdx])
		if line == "" {
			continue
		}
		
		// Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
		if !strings.Contains(line, ":") {
			continue
		}
		
		parts := strings.SplitN(line, ":", 2)
		if len(parts) != 2 {
			continue
		}
		
		// Parse dimensions
		dims := strings.TrimSpace(parts[0])
		if !strings.Contains(dims, "x") {
			continue
		}
		
		dimParts := strings.Split(dims, "x")
		if len(dimParts) != 2 {
			continue
		}
		
		width, err1 := strconv.Atoi(dimParts[0])
		height, err2 := strconv.Atoi(dimParts[1])
		if err1 != nil || err2 != nil {
			continue
		}
		
		// Parse counts
		countParts := strings.Fields(strings.TrimSpace(parts[1]))
		if len(countParts) != 6 {
			continue
		}
		
		counts := make([]int, 6)
		validCounts := true
		for j, c := range countParts {
			if count, err := strconv.Atoi(c); err == nil {
				counts[j] = count
			} else {
				validCounts = false
				break
			}
		}
		
		if !validCounts {
			continue
		}
		
		// Calculate area check
		regionArea := width * height
		requiredArea := 0
		for j := 0; j < 6; j++ {
			requiredArea += shapeAreas[j] * counts[j]
		}
		
		if requiredArea <= regionArea {
			possibleCount++
		}
	}
	
	// Part 2: Final star (no computation needed)
	part2 := "Final star"
	
	return strconv.Itoa(possibleCount), part2
}

func main() {
	file, err := os.Open("../data/input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening file: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()
	
	var content strings.Builder
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		content.WriteString(scanner.Text())
		content.WriteString("\n")
	}
	
	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading file: %v\n", err)
		os.Exit(1)
	}
	
	part1, part2 := solve(content.String())
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
