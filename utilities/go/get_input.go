package utils

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// GetInput reads input from input.txt for the given day.
// day should be a number like 1, 2, 3, etc.
// Returns lines as a slice of strings.
func GetInput(day int) ([]string, error) {
	path := GetInputPath(day)
	return ReadInput(path)
}

// GetTestInput reads input from test_N.txt for the given day and test number.
// day should be a number like 1, 2, 3, etc.
// testNum should be the test number (1, 2, 3, etc.)
// Returns lines as a slice of strings.
func GetTestInput(day int, testNum int) ([]string, error) {
	path := GetTestInputPath(day, testNum)
	return ReadInput(path)
}

// GetInputPath returns the path to input.txt for the given day.
func GetInputPath(day int) string {
	// Get the path relative to the solution file
	// Solutions are in days/day_NN/solutions/
	// Input is in days/day_NN/data/input.txt
	return fmt.Sprintf("../data/input.txt")
}

// GetTestInputPath returns the path to test_N.txt for the given day and test number.
func GetTestInputPath(day int, testNum int) string {
	return fmt.Sprintf("../data/test_%d.txt", testNum)
}

// ReadInput reads a file and returns lines as a slice of strings.
func ReadInput(filePath string) ([]string, error) {
	// Resolve relative paths from solution location
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return nil, fmt.Errorf("error resolving path: %w", err)
	}

	file, err := os.Open(absPath)
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

// ReadInputRaw reads a file and returns raw content as a string.
func ReadInputRaw(filePath string) (string, error) {
	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return "", fmt.Errorf("error resolving path: %w", err)
	}

	data, err := os.ReadFile(absPath)
	if err != nil {
		return "", err
	}
	return string(data), nil
}
