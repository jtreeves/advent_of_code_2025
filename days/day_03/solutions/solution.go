package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

// Find the largest N-digit number by selecting N digits in order from bank
func findLargestSubsequence(bank string, n int) int64 {
	bankLen := len(bank)
	if bankLen < n {
		return 0
	}
	
	var result strings.Builder
	start := 0
	
	for i := 0; i < n; i++ {
		remainingNeeded := n - i - 1
		end := bankLen - remainingNeeded
		
		maxDigit := bank[start]
		maxPos := start
		for j := start + 1; j < end; j++ {
			if bank[j] > maxDigit {
				maxDigit = bank[j]
				maxPos = j
			}
		}
		
		result.WriteByte(maxDigit)
		start = maxPos + 1
	}
	
	val, _ := strconv.ParseInt(result.String(), 10, 64)
	return val
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	var part1Sum int64 = 0
	var part2Sum int64 = 0
	
	for _, line := range lines {
		bank := strings.TrimSpace(line)
		if bank == "" {
			continue
		}
		
		part1Sum += findLargestSubsequence(bank, 2)
		
		if len(bank) >= 12 {
			part2Sum += findLargestSubsequence(bank, 12)
		}
	}
	
	return fmt.Sprintf("%d", part1Sum), fmt.Sprintf("%d", part2Sum)
}

func main() {
	data, err := ioutil.ReadFile("../data/input.txt")
	if err != nil {
		panic(err)
	}
	part1, part2 := solve(string(data))
	fmt.Printf("Part 1: %s\n", part1)
	fmt.Printf("Part 2: %s\n", part2)
}
