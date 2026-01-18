package utils

import (
	"strconv"
	"strings"
)

// ParseInts parses integers from a line of text.
// Splits the line by whitespace and extracts all integers.
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
