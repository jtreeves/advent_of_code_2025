import java.io.File

fun readInputRaw(filePath: String): String {
    return File(filePath).readText()
}

fun solve(inputData: String): Pair<Long, Long> {
    val lines = inputData.trim().split('\n')
    
    // Find blank line separator
    val blankIdx = lines.indexOfFirst { it.trim().isEmpty() }.let { 
        if (it == -1) lines.size else it 
    }
    
    // Parse ranges (first section)
    val ranges = mutableListOf<Pair<Long, Long>>()
    for (i in 0 until blankIdx) {
        val line = lines[i].trim()
        if (line.isEmpty()) continue
        val parts = line.split('-')
        val start = parts[0].toLong()
        val end = parts[1].toLong()
        ranges.add(Pair(start, end))
    }
    
    // Parse IDs to check (second section)
    val ids = mutableListOf<Long>()
    for (i in (blankIdx + 1) until lines.size) {
        val line = lines[i].trim()
        if (line.isEmpty()) continue
        ids.add(line.toLong())
    }
    
    // Part 1: Count how many IDs fall into any range
    var part1Count = 0
    for (idVal in ids) {
        for ((start, end) in ranges) {
            if (start <= idVal && idVal <= end) {
                part1Count++
                break
            }
        }
    }
    
    // Part 2: Merge ranges and count total unique IDs covered
    // Sort ranges by start value
    val sortedRanges = ranges.sortedBy { it.first }
    
    // Merge overlapping/adjacent ranges
    val merged = mutableListOf<Pair<Long, Long>>()
    if (sortedRanges.isNotEmpty()) {
        merged.add(sortedRanges[0])
        for (i in 1 until sortedRanges.size) {
            val (currStart, currEnd) = sortedRanges[i]
            val last = merged.last()
            // Check if overlaps or is adjacent (currStart <= last.second + 1)
            if (currStart <= last.second + 1) {
                // Merge: update end to max of both ends
                merged[merged.size - 1] = Pair(last.first, maxOf(last.second, currEnd))
            } else {
                // No overlap, add as new range
                merged.add(Pair(currStart, currEnd))
            }
        }
    }
    
    // Calculate total unique IDs covered
    val part2Total = merged.sumOf { (start, end) -> end - start + 1 }
    
    return Pair(part1Count.toLong(), part2Total)
}

fun main() {
    val content = readInputRaw("../data/input.txt")
    val (part1, part2) = solve(content)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
