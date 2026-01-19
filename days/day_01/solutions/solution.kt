fun solve(lines: List<String>): Pair<Int, Int> {
    // Part 1: Count times dial ends at 0 after a rotation
    var position = 50
    var countPart1 = 0
    
    for (line in lines) {
        val trimmed = line.trim()
        if (trimmed.isEmpty()) continue
        
        val direction = trimmed[0]
        val distance = trimmed.substring(1).toInt()
        
        // Apply rotation
        position = if (direction == 'L') {
            ((position - distance) % 100 + 100) % 100
        } else { // direction == 'R'
            (position + distance) % 100
        }
        
        // Check if ended at 0
        if (position == 0) {
            countPart1++
        }
    }
    
    // Part 2: Count times dial is at 0 during entire process
    position = 50
    var countPart2 = 0
    
    for (line in lines) {
        val trimmed = line.trim()
        if (trimmed.isEmpty()) continue
        
        val direction = trimmed[0]
        val distance = trimmed.substring(1).toInt()
        
        val startPos = position
        
        // Check each click position during rotation
        for (click in 1..distance) {
            val clickPos = if (direction == 'L') {
                ((startPos - click) % 100 + 100) % 100
            } else { // direction == 'R'
                (startPos + click) % 100
            }
            
            if (clickPos == 0) {
                countPart2++
            }
        }
        
        // Update position after rotation
        position = if (direction == 'L') {
            ((position - distance) % 100 + 100) % 100
        } else {
            (position + distance) % 100
        }
    }
    
    return Pair(countPart1, countPart2)
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
