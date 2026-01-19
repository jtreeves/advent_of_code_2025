package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	// Build graph: device -> list of outputs
	graph := make(map[string][]string)
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			continue
		}
		device := strings.TrimSpace(parts[0])
		outputsStr := strings.TrimSpace(parts[1])
		outputs := []string{}
		if outputsStr != "" {
			outputs = strings.Fields(outputsStr)
		}
		graph[device] = outputs
	}
	
	// Part 1: Count paths from "you" to "out"
	var countPathsPart1 func(string, map[string]int) int
	countPathsPart1 = func(node string, memo map[string]int) int {
		if node == "out" {
			return 1
		}
		if val, ok := memo[node]; ok {
			return val
		}
		
		count := 0
		for _, neighbor := range graph[node] {
			count += countPathsPart1(neighbor, memo)
		}
		
		memo[node] = count
		return count
	}
	
	part1Count := 0
	if _, ok := graph["you"]; ok {
		part1Memo := make(map[string]int)
		part1Count = countPathsPart1("you", part1Memo)
	}
	
	// Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
	type MemoKey struct {
		node string
		visitedFFT bool
		visitedDAC bool
	}
	
	var countPathsPart2 func(string, bool, bool, map[MemoKey]int) int
	countPathsPart2 = func(node string, visitedFFT bool, visitedDAC bool, memo map[MemoKey]int) int {
		if node == "out" {
			if visitedFFT && visitedDAC {
				return 1
			}
			return 0
		}
		
		key := MemoKey{node, visitedFFT, visitedDAC}
		if val, ok := memo[key]; ok {
			return val
		}
		
		// Update flags when visiting fft or dac
		newVisitedFFT := visitedFFT || (node == "fft")
		newVisitedDAC := visitedDAC || (node == "dac")
		
		count := 0
		for _, neighbor := range graph[node] {
			count += countPathsPart2(neighbor, newVisitedFFT, newVisitedDAC, memo)
		}
		
		memo[key] = count
		return count
	}
	
	part2Count := 0
	if _, ok := graph["svr"]; ok {
		part2Memo := make(map[MemoKey]int)
		part2Count = countPathsPart2("svr", false, false, part2Memo)
	}
	
	return strconv.Itoa(part1Count), strconv.Itoa(part2Count)
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
