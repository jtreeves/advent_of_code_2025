package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

type parseResult struct {
	targetPattern []bool
	buttons       [][]int
	joltages      []int
}

func parseLine(line string) parseResult {
	var result parseResult
	
	// Extract pattern: [.##.]
	patternRe := regexp.MustCompile(`\[([.#]+)\]`)
	patternMatch := patternRe.FindStringSubmatch(line)
	if patternMatch == nil {
		return result
	}
	patternStr := patternMatch[1]
	result.targetPattern = make([]bool, len(patternStr))
	for i, c := range patternStr {
		result.targetPattern[i] = c == '#'
	}
	
	// Extract buttons: (1,3) (2) etc.
	buttonRe := regexp.MustCompile(`\(([^)]*)\)`)
	buttonMatches := buttonRe.FindAllStringSubmatch(line, -1)
	for _, match := range buttonMatches {
		btnStr := strings.TrimSpace(match[1])
		if btnStr == "" {
			result.buttons = append(result.buttons, []int{})
		} else {
			parts := strings.Split(btnStr, ",")
			lights := make([]int, 0, len(parts))
			for _, part := range parts {
				if val, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
					lights = append(lights, val)
				}
			}
			result.buttons = append(result.buttons, lights)
		}
	}
	
	// Extract joltages: {3,5,4,7}
	joltageRe := regexp.MustCompile(`\{([^}]+)\}`)
	joltageMatch := joltageRe.FindStringSubmatch(line)
	if joltageMatch != nil {
		joltageStr := joltageMatch[1]
		parts := strings.Split(joltageStr, ",")
		result.joltages = make([]int, 0, len(parts))
		for _, part := range parts {
			if val, err := strconv.Atoi(strings.TrimSpace(part)); err == nil {
				result.joltages = append(result.joltages, val)
			}
		}
	}
	
	return result
}

func gaussianEliminationGF2(matrix [][]bool, target []bool) *int {
	numButtons := len(matrix)
	numLights := len(target)
	
	// Create augmented matrix [A | b]
	aug := make([][]bool, numLights)
	for i := 0; i < numLights; i++ {
		row := make([]bool, numButtons+1)
		for j := 0; j < numButtons; j++ {
			if j < len(matrix) && i < len(matrix[j]) {
				row[j] = matrix[j][i]
			}
		}
		row[numButtons] = target[i]
		aug[i] = row
	}
	
	// Gaussian elimination mod 2
	pivotRow := 0
	pivotCol := 0
	
	for pivotRow < numLights && pivotCol < numButtons {
		// Find pivot
		pivotIdx := -1
		for i := pivotRow; i < numLights; i++ {
			if aug[i][pivotCol] {
				pivotIdx = i
				break
			}
		}
		
		if pivotIdx == -1 {
			pivotCol++
			continue
		}
		
		// Swap rows
		if pivotIdx != pivotRow {
			aug[pivotRow], aug[pivotIdx] = aug[pivotIdx], aug[pivotRow]
		}
		
		// Eliminate
		for i := pivotRow + 1; i < numLights; i++ {
			if aug[i][pivotCol] {
				for j := 0; j < numButtons+1; j++ {
					aug[i][j] = aug[i][j] != aug[pivotRow][j]
				}
			}
		}
		
		pivotRow++
		pivotCol++
	}
	
	// Check for inconsistency (0 = 1)
	for i := pivotRow; i < numLights; i++ {
		if aug[i][numButtons] {
			hasNonZero := false
			for j := 0; j < numButtons; j++ {
				if aug[i][j] {
					hasNonZero = true
					break
				}
			}
			if !hasNonZero {
				return nil // No solution
			}
		}
	}
	
	// Back substitution to find solution (set free variables to 0 to minimize)
	solution := make([]bool, numButtons)
	usedRows := make(map[int]bool)
	
	// Process from bottom up
	for i := numLights - 1; i >= 0; i-- {
		// Find first non-zero column
		pivotColIdx := -1
		for j := 0; j < numButtons; j++ {
			if aug[i][j] && !usedRows[j] {
				pivotColIdx = j
				usedRows[j] = true
				break
			}
		}
		
		if pivotColIdx != -1 {
			// Calculate value
			val := aug[i][numButtons]
			for j := pivotColIdx + 1; j < numButtons; j++ {
				if aug[i][j] && solution[j] {
					val = !val
				}
			}
			solution[pivotColIdx] = val
		}
	}
	
	// Count number of presses
	count := 0
	for _, x := range solution {
		if x {
			count++
		}
	}
	return &count
}

func solvePart2ILP(buttons [][]int, joltages []int, targetPattern []bool) *int {
	numButtons := len(buttons)
	numLights := len(joltages)
	
	// Base case: all joltages are 0
	allZero := true
	for _, j := range joltages {
		if j != 0 {
			allZero = false
			break
		}
	}
	if allZero {
		result := 0
		return &result
	}
	
	// Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
	// that achieve the required parity, then divide by 2 and recurse
	buttonMatrix := make([][]bool, numButtons)
	for i := range buttonMatrix {
		buttonMatrix[i] = make([]bool, numLights)
	}
	for i, btn := range buttons {
		for _, light := range btn {
			if light >= 0 && light < numLights {
				buttonMatrix[i][light] = true
			}
		}
	}
	
	// Required parity: pattern ON = odd, pattern OFF = even
	requiredParity := make([]bool, numLights)
	copy(requiredParity, targetPattern)
	
	// Find minimum parity presses needed (Part 1 style)
	parityToggles := make([]bool, numLights)
	copy(parityToggles, requiredParity)
	minParityPresses := gaussianEliminationGF2(buttonMatrix, parityToggles)
	
	if minParityPresses != nil {
		// Helper to generate combinations
		var generateCombinations func(n, k int) [][]int
		generateCombinations = func(n, k int) [][]int {
			if k == 0 {
				return [][]int{{}}
			}
			if k > n {
				return [][]int{}
			}
			if k == n {
				result := make([]int, n)
				for i := 0; i < n; i++ {
					result[i] = i
				}
				return [][]int{result}
			}
			result := [][]int{}
			// Include first element
			for _, combo := range generateCombinations(n-1, k-1) {
				newCombo := append([]int{0}, combo...)
				for i := 1; i < len(newCombo); i++ {
					newCombo[i]++
				}
				result = append(result, newCombo)
			}
			// Exclude first element
			for _, combo := range generateCombinations(n-1, k) {
				newCombo := make([]int, len(combo))
				for i, v := range combo {
					newCombo[i] = v + 1
				}
				result = append(result, newCombo)
			}
			return result
		}
		
		bestResult := (*int)(nil)
		maxParityToTry := numButtons
		
		for k := *minParityPresses; k <= maxParityToTry; k++ {
			// Pruning
			if bestResult != nil && k > *bestResult {
				break
			}
			
			for _, buttonCombo := range generateCombinations(numButtons, k) {
				// Simulate pressing these buttons once
				resultingJoltages := make([]int, len(joltages))
				copy(resultingJoltages, joltages)
				for _, btnIdx := range buttonCombo {
					for _, light := range buttons[btnIdx] {
						if light >= 0 && light < numLights {
							resultingJoltages[light]++
						}
					}
				}
				
				// Check if parity matches
				resultingParity := make([]bool, numLights)
				for i, j := range resultingJoltages {
					resultingParity[i] = j%2 == 1
				}
				parityMatch := true
				for i := range resultingParity {
					if resultingParity[i] != requiredParity[i] {
						parityMatch = false
						break
					}
				}
				
				if parityMatch {
					// Check if all remaining are even (can divide by 2)
					allEven := true
					for _, j := range resultingJoltages {
						if j%2 != 0 {
							allEven = false
							break
						}
					}
					if allEven {
						// Divide by 2 and recurse
						halvedJoltages := make([]int, len(resultingJoltages))
						for i, j := range resultingJoltages {
							halvedJoltages[i] = j / 2
						}
						remainingResult := solvePart2ILP(buttons, halvedJoltages, targetPattern)
						if remainingResult != nil {
							total := k + *remainingResult*2
							if bestResult == nil || total < *bestResult {
								bestResult = &total
							}
						}
					}
				}
			}
		}
		
		if bestResult != nil {
			return bestResult
		}
	}
	
	// Fallback: divide-by-2 optimization
	allEven := true
	for _, j := range joltages {
		if j%2 != 0 {
			allEven = false
			break
		}
	}
	if allEven {
		hasNonZero := false
		for _, j := range joltages {
			if j > 0 {
				hasNonZero = true
				break
			}
		}
		if hasNonZero {
			halvedJoltages := make([]int, len(joltages))
			for i, j := range joltages {
				halvedJoltages[i] = j / 2
			}
			result := solvePart2ILP(buttons, halvedJoltages, targetPattern)
			if result != nil {
				total := *result * 2
				return &total
			}
			return nil
		}
	}
	
	// Final fallback: bounded DFS
	maxJoltage := 0
	for _, j := range joltages {
		if j > maxJoltage {
			maxJoltage = j
		}
	}
	
	var dfs func(buttonIdx int, currentJoltages []int, pressesSoFar int, best *int) *int
	dfs = func(buttonIdx int, currentJoltages []int, pressesSoFar int, best *int) *int {
		if buttonIdx >= numButtons {
			allMatch := true
			for i := range currentJoltages {
				if currentJoltages[i] != joltages[i] {
					allMatch = false
					break
				}
			}
			if allMatch {
				if best == nil {
					result := pressesSoFar
					return &result
				}
				if pressesSoFar < *best {
					result := pressesSoFar
					return &result
				}
			}
			return best
		}
		
		if best != nil && pressesSoFar >= *best {
			return best
		}
		
		bestResult := best
		for presses := 0; presses <= maxJoltage; presses++ {
			if best != nil && pressesSoFar+presses >= *best {
				break
			}
			
			newJoltages := make([]int, len(currentJoltages))
			copy(newJoltages, currentJoltages)
			for _, light := range buttons[buttonIdx] {
				if light >= 0 && light < numLights {
					newJoltages[light] += presses
				}
			}
			
			exceeds := false
			for i := range newJoltages {
				if newJoltages[i] > joltages[i] {
					exceeds = true
					break
				}
			}
			if exceeds {
				continue
			}
			
			result := dfs(buttonIdx+1, newJoltages, pressesSoFar+presses, bestResult)
			if result != nil {
				bestResult = result
			}
		}
		
		return bestResult
	}
	
	initialJoltages := make([]int, numLights)
	return dfs(0, initialJoltages, 0, nil)
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	if len(lines) == 0 {
		return "0", "0"
	}
	
	part1Total := 0
	part2Total := 0
	
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		
		parsed := parseLine(line)
		if len(parsed.targetPattern) == 0 {
			continue
		}
		
		numLights := len(parsed.targetPattern)
		
		// Part 1: GF(2) linear system
		// Build incidence matrix: matrix[i][j] = true if button i toggles light j
		buttonMatrix := make([][]bool, len(parsed.buttons))
		for i := range buttonMatrix {
			buttonMatrix[i] = make([]bool, numLights)
		}
		for i, btn := range parsed.buttons {
			for _, light := range btn {
				if light >= 0 && light < numLights {
					buttonMatrix[i][light] = true
				}
			}
		}
		
		// Target: all start OFF, need to toggle to match pattern
		// Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
		requiredToggles := make([]bool, len(parsed.targetPattern))
		copy(requiredToggles, parsed.targetPattern)
		
		result := gaussianEliminationGF2(buttonMatrix, requiredToggles)
		if result != nil {
			part1Total += *result
		}
		
		// Part 2: Integer Linear Programming
		if len(parsed.joltages) == numLights {
			result2 := solvePart2ILP(parsed.buttons, parsed.joltages, parsed.targetPattern)
			if result2 != nil {
				part2Total += *result2
			}
		}
	}
	
	return fmt.Sprintf("%d", part1Total), fmt.Sprintf("%d", part2Total)
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
