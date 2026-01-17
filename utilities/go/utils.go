package utils

import (
	"bufio"
	"os"
	"strconv"
	"strings"
)

// Common utility functions for Go solutions

func ReadInput(filePath string) ([]string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, strings.TrimSpace(scanner.Text()))
	}
	return lines, scanner.Err()
}

func ReadInputRaw(filePath string) (string, error) {
	data, err := os.ReadFile(filePath)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

func ParseInts(line string) []int {
	parts := strings.Fields(line)
	var ints []int
	for _, part := range parts {
		if num, err := strconv.Atoi(part); err == nil {
			ints = append(ints, num)
		}
	}
	return ints
}

// Placeholder for additional common utilities
