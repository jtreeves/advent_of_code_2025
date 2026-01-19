fun countNeighbors(grid: List<String>, i: Int, j: Int, rows: Int, cols: Int): Int {
    var count = 0
    for (di in -1..1) {
        for (dj in -1..1) {
            if (di == 0 && dj == 0) continue
            val ni = i + di
            val nj = j + dj
            if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@') {
                count++
            }
        }
    }
    return count
}

fun solve(lines: List<String>): Pair<Int, Int> {
    val rows = lines.size
    val cols = if (rows > 0) lines[0].length else 0
    
    // Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    var part1Count = 0
    for (i in 0 until rows) {
        for (j in 0 until cols) {
            if (lines[i][j] == '@') {
                val neighbors = countNeighbors(lines, i, j, rows, cols)
                if (neighbors < 4) {
                    part1Count++
                }
            }
        }
    }
    
    // Part 2: Iteratively remove accessible rolls until none can be removed
    val grid = lines.map { it.toCharArray().toMutableList() }.toMutableList()
    
    var part2Count = 0
    while (true) {
        val toRemove = mutableListOf<Pair<Int, Int>>()
        for (i in 0 until rows) {
            for (j in 0 until cols) {
                if (grid[i][j] == '@') {
                    // Convert grid to string list for neighbor counting
                    val gridStr = grid.map { it.joinToString("") }
                    val neighbors = countNeighbors(gridStr, i, j, rows, cols)
                    if (neighbors < 4) {
                        toRemove.add(Pair(i, j))
                    }
                }
            }
        }
        
        if (toRemove.isEmpty()) {
            break
        }
        
        // Remove all marked positions
        for ((i, j) in toRemove) {
            grid[i][j] = '.'
        }
        part2Count += toRemove.size
    }
    
    return Pair(part1Count, part2Count)
}

fun main() {
    val file = java.io.File("../data/input.txt")
    val lines = mutableListOf<String>()
    java.io.BufferedReader(java.io.FileReader(file)).use { reader ->
        reader.forEachLine { line ->
            lines.add(line.trim())
        }
    }
    val (part1, part2) = solve(lines)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
