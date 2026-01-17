package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

// Placeholder for Day 7 Go solution
func solve(inputData string) (string, string) {
	fmt.Println("Day 7 Go placeholder")
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	// Part 1
	part1Result := "TODO"
	
	// Part 2
	part2Result := "TODO"
	
	return part1Result, part2Result
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
