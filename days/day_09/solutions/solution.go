package main

import (
	"fmt"
	"io/ioutil"
	"math"
	"sort"
	"strconv"
	"strings"
)

type Point struct {
	x, y int64
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	
	// Parse coordinates
	var redTiles []Point
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		if strings.Contains(line, ",") {
			parts := strings.Split(line, ",")
			if len(parts) == 2 {
				x, err1 := strconv.ParseInt(strings.TrimSpace(parts[0]), 10, 64)
				y, err2 := strconv.ParseInt(strings.TrimSpace(parts[1]), 10, 64)
				if err1 == nil && err2 == nil {
					redTiles = append(redTiles, Point{x, y})
				}
			}
		}
	}
	
	if len(redTiles) < 2 {
		return "0", "0"
	}
	
	// Part 1: Find largest rectangle area using any two red tiles as corners
	var maxAreaPart1 int64 = 0
	for i := 0; i < len(redTiles); i++ {
		for j := i + 1; j < len(redTiles); j++ {
			x1, y1 := redTiles[i].x, redTiles[i].y
			x2, y2 := redTiles[j].x, redTiles[j].y
			width := int64(math.Abs(float64(x1-x2))) + 1
			height := int64(math.Abs(float64(y1-y2))) + 1
			area := width * height
			if area > maxAreaPart1 {
				maxAreaPart1 = area
			}
		}
	}
	
	// Part 2: Coordinate compression + flood-fill + prefix sums
	allXSet := make(map[int64]bool)
	allYSet := make(map[int64]bool)
	for _, pt := range redTiles {
		allXSet[pt.x] = true
		allXSet[pt.x+1] = true
		allYSet[pt.y] = true
		allYSet[pt.y+1] = true
	}
	
	var allX []int64
	for x := range allXSet {
		allX = append(allX, x)
	}
	sort.Slice(allX, func(i, j int) bool { return allX[i] < allX[j] })
	
	var allY []int64
	for y := range allYSet {
		allY = append(allY, y)
	}
	sort.Slice(allY, func(i, j int) bool { return allY[i] < allY[j] })
	
	xToCx := make(map[int64]int)
	yToCy := make(map[int64]int)
	for i, x := range allX {
		xToCx[x] = i
	}
	for i, y := range allY {
		yToCy[y] = i
	}
	
	width := len(allX)
	height := len(allY)
	
	// Build grid
	grid := make([][]bool, width)
	for i := range grid {
		grid[i] = make([]bool, height)
	}
	
	// Mark boundary
	for _, pt := range redTiles {
		if cx, ok := xToCx[pt.x]; ok {
			if cy, ok := yToCy[pt.y]; ok {
				grid[cx][cy] = true
			}
		}
	}
	
	// Connect consecutive red tiles
	for i := 0; i < len(redTiles); i++ {
		p1 := redTiles[i]
		p2 := redTiles[(i+1)%len(redTiles)]
		
		if p1.x == p2.x {
			startY := p1.y
			if p2.y < startY {
				startY = p2.y
			}
			endY := p1.y
			if p2.y > endY {
				endY = p2.y
			}
			for y := startY; y <= endY; y++ {
				if cx, ok := xToCx[p1.x]; ok {
					if cy, ok := yToCy[y]; ok {
						grid[cx][cy] = true
					}
				}
			}
		} else if p1.y == p2.y {
			startX := p1.x
			if p2.x < startX {
				startX = p2.x
			}
			endX := p1.x
			if p2.x > endX {
				endX = p2.x
			}
			for x := startX; x <= endX; x++ {
				if cx, ok := xToCx[x]; ok {
					if cy, ok := yToCy[p1.y]; ok {
						grid[cx][cy] = true
					}
				}
			}
		}
	}
	
	// Point-in-polygon
	pointInPolygon := func(px, py int64) bool {
		inside := false
		for i := 0; i < len(redTiles); i++ {
			p1 := redTiles[i]
			p2 := redTiles[(i+1)%len(redTiles)]
			if (p1.y > py) != (p2.y > py) {
				intersectX := float64(px)
				if p2.y != p1.y {
					intersectX = float64(py-p1.y)*float64(p2.x-p1.x)/float64(p2.y-p1.y) + float64(p1.x)
				}
				if float64(px) < intersectX {
					inside = !inside
				}
			}
		}
		return inside
	}
	
	// Flood fill interior
	foundInterior := false
	for cx := 0; cx < width && !foundInterior; cx++ {
		for cy := 0; cy < height && !foundInterior; cy++ {
			if !grid[cx][cy] {
				origX := allX[cx]
				origY := allY[cy]
				if pointInPolygon(origX, origY) {
					stack := [][2]int{{cx, cy}}
					for len(stack) > 0 {
						var pos [2]int
						pos, stack = stack[len(stack)-1], stack[:len(stack)-1]
						x, y := pos[0], pos[1]
						if x >= width || y >= height || grid[x][y] {
							continue
						}
						origX := allX[x]
						origY := allY[y]
						if pointInPolygon(origX, origY) {
							grid[x][y] = true
							if x > 0 {
								stack = append(stack, [2]int{x - 1, y})
							}
							if x+1 < width {
								stack = append(stack, [2]int{x + 1, y})
							}
							if y > 0 {
								stack = append(stack, [2]int{x, y - 1})
							}
							if y+1 < height {
								stack = append(stack, [2]int{x, y + 1})
							}
						}
					}
					foundInterior = true
				}
			}
		}
	}
	
	// Build 2D prefix sum
	prefix := make([][]int64, width+1)
	for i := range prefix {
		prefix[i] = make([]int64, height+1)
	}
	for cx := 0; cx < width; cx++ {
		for cy := 0; cy < height; cy++ {
			val := int64(0)
			if grid[cx][cy] {
				val = 1
			}
			prefix[cx+1][cy+1] = prefix[cx][cy+1] + prefix[cx+1][cy] - prefix[cx][cy] + val
		}
	}
	
	rectSum := func(cx1, cx2, cy1, cy2 int) int64 {
		return prefix[cx2+1][cy2+1] - prefix[cx1][cy2+1] - prefix[cx2+1][cy1] + prefix[cx1][cy1]
	}
	
	// Generate candidates sorted by area descending
	type Candidate struct {
		minX, maxX, minY, maxY int64
		area                    int64
		cx1, cx2, cy1, cy2      int
	}
	var candidates []Candidate
	for i := 0; i < len(redTiles); i++ {
		for j := i + 1; j < len(redTiles); j++ {
			p1 := redTiles[i]
			p2 := redTiles[j]
			minX := p1.x
			if p2.x < minX {
				minX = p2.x
			}
			maxX := p1.x
			if p2.x > maxX {
				maxX = p2.x
			}
			minY := p1.y
			if p2.y < minY {
				minY = p2.y
			}
			maxY := p1.y
			if p2.y > maxY {
				maxY = p2.y
			}
			area := (maxX - minX + 1) * (maxY - minY + 1)
			
			cx1, ok1 := xToCx[minX]
			cx2, ok2 := xToCx[maxX]
			cy1, ok3 := yToCy[minY]
			cy2, ok4 := yToCy[maxY]
			if ok1 && ok2 && ok3 && ok4 {
				candidates = append(candidates, Candidate{minX, maxX, minY, maxY, area, cx1, cx2, cy1, cy2})
			}
		}
	}
	
	sort.Slice(candidates, func(i, j int) bool {
		return candidates[i].area > candidates[j].area
	})
	
	// Check candidates
	var maxAreaPart2 int64 = 0
	for _, cand := range candidates {
		if cand.area <= maxAreaPart2 {
			break
		}
		
		validCount := rectSum(cand.cx1, cand.cx2, cand.cy1, cand.cy2)
		expectedCells := int64((cand.cx2 - cand.cx1 + 1) * (cand.cy2 - cand.cy1 + 1))
		
		if validCount == expectedCells {
			allValid := true
			corners := [][2]int64{{cand.minX, cand.minY}, {cand.minX, cand.maxY}, {cand.maxX, cand.minY}, {cand.maxX, cand.maxY}}
			for _, corner := range corners {
				x, y := corner[0], corner[1]
				if cx, ok := xToCx[x]; ok {
					if cy, ok := yToCy[y]; ok {
						if !grid[cx][cy] {
							allValid = false
							break
						}
					} else {
						if !pointInPolygon(x, y) {
							allValid = false
							break
						}
					}
				} else {
					if !pointInPolygon(x, y) {
						allValid = false
						break
					}
				}
			}
			
			if allValid {
				maxAreaPart2 = cand.area
				break
			}
		}
	}
	
	return strconv.FormatInt(maxAreaPart1, 10), strconv.FormatInt(maxAreaPart2, 10)
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
