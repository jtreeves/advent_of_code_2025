fun solve(inputData: String): Pair<Long, Long> {
    val lines = inputData.trim().split('\n')
    if (lines.isEmpty()) return Pair(0, 0)
    
    val grid = lines.map { it.toCharArray() }
    val rows = grid.size
    val cols = if (rows > 0) grid[0].size else 0
    
    // Find starting position S
    var startRow = -1
    var startCol = -1
    for (r in 0 until rows) {
        for (c in 0 until cols) {
            if (grid[r][c] == 'S') {
                startRow = r
                startCol = c
                break
            }
        }
        if (startRow != -1) break
    }
    
    if (startRow == -1) return Pair(0, 0)
    
    // Part 1: Count total splits
    var splitCount = 0
    var activeBeams = mutableSetOf(startCol)
    
    // Process each row starting from the row after S
    for (r in startRow + 1 until rows) {
        val nextBeams = mutableSetOf<Int>()
        for (col in activeBeams) {
            if (grid[r][col] == '.') {
                // Beam continues down
                nextBeams.add(col)
            } else if (grid[r][col] == '^') {
                // Beam splits
                splitCount++
                // Add beams to left and right
                if (col - 1 >= 0) nextBeams.add(col - 1)
                if (col + 1 < cols) nextBeams.add(col + 1)
            }
        }
        activeBeams = nextBeams
    }
    
    // Part 2: Count beams reaching bottom row
    val beamCounts = Array(rows) { LongArray(cols) }
    beamCounts[startRow][startCol] = 1 // Start with 1 beam at S
    
    // Process each row starting from the row after S
    for (r in startRow + 1 until rows) {
        for (c in 0 until cols) {
            val prevCount = beamCounts[r - 1][c]
            if (prevCount > 0) {
                if (grid[r][c] == '.') {
                    // Beam continues down
                    beamCounts[r][c] += prevCount
                } else if (grid[r][c] == '^') {
                    // Beam splits into left and right
                    if (c - 1 >= 0) beamCounts[r][c - 1] += prevCount
                    if (c + 1 < cols) beamCounts[r][c + 1] += prevCount
                }
            }
        }
    }
    
    // Sum all beams in bottom row
    val bottomBeamCount = beamCounts[rows - 1].sum()
    
    return Pair(splitCount.toLong(), bottomBeamCount)
}

fun main() {
    val content = readInputRaw("../data/input.txt")
    val (part1, part2) = solve(content)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
