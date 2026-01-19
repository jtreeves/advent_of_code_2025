fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split('\n')
    if (lines.isEmpty()) return Pair("0", "Final star")
    
    // Parse shapes (first 6 shapes, numbered 0-5)
    val shapeAreas = IntArray(6)
    var i = 0
    var shapeIdx = 0
    
    while (i < lines.size && shapeIdx < 6) {
        val line = lines[i].trim()
        // Check if this is a shape header (format: "number:")
        if (line.isNotEmpty() && line.endsWith(':')) {
            val shapeNum = line.dropLast(1).toIntOrNull()
            if (shapeNum != null && shapeNum == shapeIdx) {
                // Read the next 3 lines for the shape grid
                val shapeGrid = (0..2).map { j ->
                    if (i + 1 + j < lines.size) lines[i + 1 + j].trim() else ""
                }
                
                // Count '#' characters in the shape
                val area = shapeGrid.sumOf { row -> row.count { it == '#' } }
                shapeAreas[shapeIdx] = area
                shapeIdx++
                i += 4 // Skip shape header + 3 grid lines + empty line (if present)
                continue
            }
        }
        i++
    }
    
    // Find where queries start (skip empty lines after shapes)
    var queryStart = i
    while (queryStart < lines.size && lines[queryStart].trim().isEmpty()) {
        queryStart++
    }
    
    // Parse queries
    var possibleCount = 0
    for (lineIdx in queryStart until lines.size) {
        val line = lines[lineIdx].trim()
        if (line.isEmpty()) continue
        
        // Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        if (!line.contains(':')) continue
        
        val parts = line.split(':', limit = 2)
        if (parts.size != 2) continue
        
        // Parse dimensions
        val dims = parts[0].trim()
        if (!dims.contains('x')) continue
        
        val dimParts = dims.split('x')
        if (dimParts.size != 2) continue
        
        val width = dimParts[0].toIntOrNull() ?: continue
        val height = dimParts[1].toIntOrNull() ?: continue
        
        // Parse counts
        val countParts = parts[1].trim().split(Regex("\\s+"))
        if (countParts.size != 6) continue
        
        val counts = countParts.mapNotNull { it.toIntOrNull() }
        if (counts.size != 6) continue
        
        // Calculate area check
        val regionArea = width.toLong() * height.toLong()
        var requiredArea = 0L
        for (j in 0 until 6) {
            requiredArea += shapeAreas[j].toLong() * counts[j].toLong()
        }
        
        if (requiredArea <= regionArea) {
            possibleCount++
        }
    }
    
    // Part 2: Final star (no computation needed)
    val part2 = "Final star"
    
    return Pair(possibleCount.toString(), part2)
}

fun main() {
    val data = readInputRaw("../data/input.txt")
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
