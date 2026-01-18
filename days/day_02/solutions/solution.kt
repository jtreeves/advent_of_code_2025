import java.io.File

// Check if ID is invalid for Part 1: exactly two identical sequences
fun isInvalidPart1(idStr: String): Boolean {
    val n = idStr.length
    if (n % 2 != 0) return false
    val half = n / 2
    return idStr.substring(0, half) == idStr.substring(half)
}

// Check if ID is invalid for Part 2: sequence repeated 2+ times
fun isInvalidPart2(idStr: String): Boolean {
    val n = idStr.length
    for (k in 2..n) {
        if (n % k == 0) {
            val seqLen = n / k
            val pattern = idStr.substring(0, seqLen)
            val repeated = pattern.repeat(k)
            if (idStr == repeated) {
                return true
            }
        }
    }
    return false
}

// Parse a range string like "start-end"
fun parseRange(rangeStr: String): Pair<Long, Long> {
    val parts = rangeStr.split("-")
    val start = parts[0].trim().toLong()
    val end = parts[1].trim().toLong()
    return Pair(start, end)
}

// Parse a line of comma-separated ranges
fun parseRanges(line: String): List<Pair<Long, Long>> {
    return line.split(",")
        .map { it.trim() }
        .filter { it.isNotEmpty() }
        .map { parseRange(it) }
}

fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n")
    
    var part1Sum: Long = 0
    var part2Sum: Long = 0
    
    for (line in lines) {
        if (line.trim().isEmpty()) continue
        val ranges = parseRanges(line)
        
        for ((start, end) in ranges) {
            for (num in start..end step 1) {
                val idStr = num.toString()
                
                if (isInvalidPart1(idStr)) {
                    part1Sum += num
                }
                
                if (isInvalidPart2(idStr)) {
                    part2Sum += num
                }
            }
        }
    }
    
    return Pair(part1Sum.toString(), part2Sum.toString())
}

fun main() {
    val data = File("../data/input.txt").readText()
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
