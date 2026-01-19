package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Range struct {
	start int64
	end   int64
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	// Find blank line separator
	blankIdx := -1
	for i, line := range lines {
		if strings.TrimSpace(line) == "" {
			blankIdx = i
			break
		}
	}
	
	// Parse ranges (first section)
	var ranges []Range
	for i := 0; i < blankIdx; i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		parts := strings.Split(line, "-")
		start, _ := strconv.ParseInt(parts[0], 10, 64)
		end, _ := strconv.ParseInt(parts[1], 10, 64)
		ranges = append(ranges, Range{start: start, end: end})
	}
	
	// Parse IDs to check (second section)
	var ids []int64
	for i := blankIdx + 1; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if line == "" {
			continue
		}
		id, _ := strconv.ParseInt(line, 10, 64)
		ids = append(ids, id)
	}
	
	// Part 1: Count how many IDs fall into any range
	part1Count := 0
	for _, idVal := range ids {
		for _, r := range ranges {
			if r.start <= idVal && idVal <= r.end {
				part1Count++
				break
			}
		}
	}
	
	// Part 2: Merge ranges and count total unique IDs covered
	// Sort ranges by start value
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].start < ranges[j].start
	})
	
	// Merge overlapping/adjacent ranges
	var merged []Range
	if len(ranges) > 0 {
		merged = append(merged, ranges[0])
		for i := 1; i < len(ranges); i++ {
			last := &merged[len(merged)-1]
			current := ranges[i]
			// Check if overlaps or is adjacent (current.start <= last.end + 1)
			if current.start <= last.end+1 {
				// Merge: update end to max of both ends
				if current.end > last.end {
					last.end = current.end
				}
			} else {
				// No overlap, add as new range
				merged = append(merged, current)
			}
		}
	}
	
	// Calculate total unique IDs covered
	var part2Total int64 = 0
	for _, r := range merged {
		part2Total += r.end - r.start + 1
	}
	
	return strconv.FormatInt(int64(part1Count), 10), strconv.FormatInt(part2Total, 10)
}

func main() {
	content, err := os.ReadFile("../data/input.txt")
	if err != nil {
		panic(err)
	}
	part1, part2 := solve(string(content))
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
