package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

type Coord struct {
	x, y, z int
}

type Pair struct {
	i, j, distSq int
}

type UnionFind struct {
	parent        []int
	size          []int
	componentCount int
}

func NewUnionFind(n int) *UnionFind {
	parent := make([]int, n)
	size := make([]int, n)
	for i := range parent {
		parent[i] = i
		size[i] = 1
	}
	return &UnionFind{
		parent:        parent,
		size:          size,
		componentCount: n,
	}
}

func (uf *UnionFind) Find(x int) int {
	if uf.parent[x] != x {
		uf.parent[x] = uf.Find(uf.parent[x])
	}
	return uf.parent[x]
}

func (uf *UnionFind) Union(x, y int) bool {
	rootX := uf.Find(x)
	rootY := uf.Find(y)
	if rootX == rootY {
		return false
	}
	if uf.size[rootX] < uf.size[rootY] {
		rootX, rootY = rootY, rootX
	}
	uf.parent[rootY] = rootX
	uf.size[rootX] += uf.size[rootY]
	uf.componentCount--
	return true
}

func parseCoordinates(lines []string) []Coord {
	var coords []Coord
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		coords = append(coords, Coord{x: x, y: y, z: z})
	}
	return coords
}

func squaredDistance(p1, p2 Coord) int {
	dx := p2.x - p1.x
	dy := p2.y - p1.y
	dz := p2.z - p1.z
	return dx*dx + dy*dy + dz*dz
}

func solve(inputData string) (string, string) {
	lines := strings.Split(strings.TrimSpace(inputData), "\n")
	coords := parseCoordinates(lines)

	n := len(coords)
	if n == 0 {
		return "0", "0"
	}

	// Generate all pairs with squared distances
	var pairs []Pair
	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			distSq := squaredDistance(coords[i], coords[j])
			pairs = append(pairs, Pair{i: i, j: j, distSq: distSq})
		}
	}

	// Sort by distance
	sort.Slice(pairs, func(i, j int) bool {
		return pairs[i].distSq < pairs[j].distSq
	})

	// Part 1: Connect first 1000 pairs
	uf1 := NewUnionFind(n)
	connectionsMade := 0
	for _, pair := range pairs {
		if connectionsMade >= 1000 {
			break
		}
		uf1.Union(pair.i, pair.j)
		connectionsMade++
	}

	// Get component sizes
	componentSizes := make(map[int]int)
	for i := 0; i < n; i++ {
		root := uf1.Find(i)
		componentSizes[root] = uf1.size[root]
	}

	var sizes []int
	for _, size := range componentSizes {
		sizes = append(sizes, size)
	}
	sort.Sort(sort.Reverse(sort.IntSlice(sizes)))
	part1 := 0
	if len(sizes) >= 3 {
		part1 = sizes[0] * sizes[1] * sizes[2]
	}

	// Part 2: Connect until all in one circuit
	uf2 := NewUnionFind(n)
	var finalPair []int
	for _, pair := range pairs {
		if uf2.componentCount == 1 {
			break
		}
		if uf2.Union(pair.i, pair.j) {
			if uf2.componentCount == 1 {
				finalPair = []int{pair.i, pair.j}
				break
			}
		}
	}

	part2 := 0
	if finalPair != nil {
		part2 = coords[finalPair[0]].x * coords[finalPair[1]].x
	}

	return strconv.Itoa(part1), strconv.Itoa(part2)
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
