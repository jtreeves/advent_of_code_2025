import java.io.File

data class Point(val x: Long, val y: Long)

fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n").map { it.trim() }.filter { it.isNotEmpty() }
    
    // Parse coordinates
    val redTiles = mutableListOf<Point>()
    for (line in lines) {
        if (',' in line) {
            val parts = line.split(',')
            if (parts.size == 2) {
                try {
                    val x = parts[0].trim().toLong()
                    val y = parts[1].trim().toLong()
                    redTiles.add(Point(x, y))
                } catch (e: NumberFormatException) {
                    // Skip invalid lines
                }
            }
        }
    }
    
    if (redTiles.size < 2) {
        return Pair("0", "0")
    }
    
    // Part 1: Find largest rectangle area using any two red tiles as corners
    var maxAreaPart1 = 0L
    for (i in redTiles.indices) {
        for (j in i + 1 until redTiles.size) {
            val (x1, y1) = redTiles[i]
            val (x2, y2) = redTiles[j]
            val width = kotlin.math.abs(x1 - x2) + 1
            val height = kotlin.math.abs(y1 - y2) + 1
            val area = width * height
            maxAreaPart1 = maxOf(maxAreaPart1, area)
        }
    }
    
    // Part 2: Coordinate compression + flood-fill + prefix sums
    val allXSet = mutableSetOf<Long>()
    val allYSet = mutableSetOf<Long>()
    for ((x, y) in redTiles) {
        allXSet.add(x)
        allXSet.add(x + 1)
        allYSet.add(y)
        allYSet.add(y + 1)
    }
    
    val allX = allXSet.sorted()
    val allY = allYSet.sorted()
    
    val xToCx = allX.mapIndexed { i, x -> x to i }.toMap()
    val yToCy = allY.mapIndexed { i, y -> y to i }.toMap()
    
    val width = allX.size
    val height = allY.size
    
    // Build grid
    val grid = Array(width) { BooleanArray(height) }
    
    // Mark boundary
    for ((x, y) in redTiles) {
        xToCx[x]?.let { cx ->
            yToCy[y]?.let { cy ->
                grid[cx][cy] = true
            }
        }
    }
    
    // Connect consecutive red tiles
    for (i in redTiles.indices) {
        val p1 = redTiles[i]
        val p2 = redTiles[(i + 1) % redTiles.size]
        
        if (p1.x == p2.x) {
            val startY = minOf(p1.y, p2.y)
            val endY = maxOf(p1.y, p2.y)
            for (y in startY..endY) {
                xToCx[p1.x]?.let { cx ->
                    yToCy[y]?.let { cy ->
                        grid[cx][cy] = true
                    }
                }
            }
        } else if (p1.y == p2.y) {
            val startX = minOf(p1.x, p2.x)
            val endX = maxOf(p1.x, p2.x)
            for (x in startX..endX) {
                xToCx[x]?.let { cx ->
                    yToCy[p1.y]?.let { cy ->
                        grid[cx][cy] = true
                    }
                }
            }
        }
    }
    
    // Point-in-polygon
    fun pointInPolygon(px: Long, py: Long): Boolean {
        var inside = false
        for (i in redTiles.indices) {
            val p1 = redTiles[i]
            val p2 = redTiles[(i + 1) % redTiles.size]
            if ((p1.y > py) != (p2.y > py)) {
                val intersectX = if (p2.y != p1.y) {
                    (py - p1.y).toDouble() * (p2.x - p1.x) / (p2.y - p1.y) + p1.x
                } else {
                    px.toDouble()
                }
                if (px < intersectX) {
                    inside = !inside
                }
            }
        }
        return inside
    }
    
    // Flood fill interior
    var foundInterior = false
    for (cx in 0 until width) {
        if (foundInterior) break
        for (cy in 0 until height) {
            if (foundInterior) break
            if (!grid[cx][cy]) {
                val origX = allX[cx]
                val origY = allY[cy]
                if (pointInPolygon(origX, origY)) {
                    val stack = mutableListOf(cx to cy)
                    while (stack.isNotEmpty()) {
                        val (x, y) = stack.removeAt(stack.size - 1)
                        if (x >= width || y >= height || grid[x][y]) continue
                        val origX2 = allX[x]
                        val origY2 = allY[y]
                        if (pointInPolygon(origX2, origY2)) {
                            grid[x][y] = true
                            if (x > 0) stack.add(x - 1 to y)
                            if (x + 1 < width) stack.add(x + 1 to y)
                            if (y > 0) stack.add(x to y - 1)
                            if (y + 1 < height) stack.add(x to y + 1)
                        }
                    }
                    foundInterior = true
                }
            }
        }
    }
    
    // Build 2D prefix sum
    val prefix = Array(width + 1) { LongArray(height + 1) }
    for (cx in 0 until width) {
        for (cy in 0 until height) {
            prefix[cx + 1][cy + 1] = prefix[cx][cy + 1] + prefix[cx + 1][cy] 
                - prefix[cx][cy] + if (grid[cx][cy]) 1 else 0
        }
    }
    
    fun rectSum(cx1: Int, cx2: Int, cy1: Int, cy2: Int): Long {
        return prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] 
            - prefix[cx2 + 1][cy1] + prefix[cx1][cy1]
    }
    
    // Generate candidates sorted by area descending
    data class Candidate(val minX: Long, val maxX: Long, val minY: Long, val maxY: Long, 
                        val area: Long, val cx1: Int, val cx2: Int, val cy1: Int, val cy2: Int)
    
    val candidates = mutableListOf<Candidate>()
    for (i in redTiles.indices) {
        for (j in i + 1 until redTiles.size) {
            val (x1, y1) = redTiles[i]
            val (x2, y2) = redTiles[j]
            val minX = minOf(x1, x2)
            val maxX = maxOf(x1, x2)
            val minY = minOf(y1, y2)
            val maxY = maxOf(y1, y2)
            val area = (maxX - minX + 1) * (maxY - minY + 1)
            
            val cx1 = xToCx[minX]
            val cx2 = xToCx[maxX]
            val cy1 = yToCy[minY]
            val cy2 = yToCy[maxY]
            if (cx1 != null && cx2 != null && cy1 != null && cy2 != null) {
                candidates.add(Candidate(minX, maxX, minY, maxY, area, cx1, cx2, cy1, cy2))
            }
        }
    }
    
    candidates.sortByDescending { it.area }
    
    // Check candidates
    var maxAreaPart2 = 0L
    for (cand in candidates) {
        if (cand.area <= maxAreaPart2) break
        
        val validCount = rectSum(cand.cx1, cand.cx2, cand.cy1, cand.cy2)
        val expectedCells = (cand.cx2 - cand.cx1 + 1).toLong() * (cand.cy2 - cand.cy1 + 1)
        
        if (validCount == expectedCells) {
            var allValid = true
            val corners = listOf(cand.minX to cand.minY, cand.minX to cand.maxY, 
                               cand.maxX to cand.minY, cand.maxX to cand.maxY)
            for ((x, y) in corners) {
                val cx = xToCx[x]
                val cy = yToCy[y]
                if (cx != null && cy != null) {
                    if (!grid[cx][cy]) {
                        allValid = false
                        break
                    }
                } else {
                    if (!pointInPolygon(x, y)) {
                        allValid = false
                        break
                    }
                }
            }
            
            if (allValid) {
                maxAreaPart2 = cand.area
                break
            }
        }
    }
    
    return Pair(maxAreaPart1.toString(), maxAreaPart2.toString())
}

fun main() {
    val data = File("../data/input.txt").readText()
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
