import java.io.File

data class Problem(val startCol: Int, val endCol: Int, val op: Char)

fun readInputRaw(filePath: String): String = File(filePath).readText()

fun solve(inputData: String): Pair<Long, Long> {
    val lines = inputData.trim().split('\n')
    if (lines.isEmpty()) return Pair(0, 0)
    
    // Find maximum line length and pad all lines
    val maxLen = lines.maxOfOrNull { it.length } ?: 0
    val paddedLines = lines.map { it.padEnd(maxLen) }
    
    // Operator row is the last row
    val opRowIdx = paddedLines.size - 1
    val opRow = paddedLines[opRowIdx]
    val numRows = paddedLines.take(opRowIdx)
    
    // Part 1: Parse horizontally
    var part1Total = 0L
    
    // Find problem boundaries (columns that are all spaces)
    val isSpaceCol = BooleanArray(maxLen) { col ->
        paddedLines.all { col >= it.length || it[col] == ' ' }
    }
    
    // Group columns into problems
    val problems = mutableListOf<Problem>()
    var i = 0
    while (i < maxLen) {
        if (!isSpaceCol[i]) {
            val startCol = i
            while (i < maxLen && !isSpaceCol[i]) i++
            val endCol = i
            // Extract operator for this problem
            val op = opRow.substring(startCol, endCol).firstOrNull { it == '+' || it == '*' }
            op?.let { problems.add(Problem(startCol, endCol, it)) }
        } else {
            i++
        }
    }
    
    // Solve Part 1: Extract numbers horizontally
    for (prob in problems) {
        val numbers = mutableListOf<Long>()
        for (row in numRows) {
            if (prob.endCol > row.length) continue
            val problemStr = row.substring(prob.startCol, prob.endCol).trim()
            val parts = problemStr.split(Regex("\\s+"))
            parts.forEach { part ->
                part.toLongOrNull()?.let { numbers.add(it) }
            }
        }
        
        // Apply operator
        if (numbers.isNotEmpty()) {
            val result = if (prob.op == '+') {
                numbers.sum()
            } else {
                numbers.fold(1L) { acc, n -> acc * n }
            }
            part1Total += result
        }
    }
    
    // Part 2: Parse vertically (columns, right-to-left)
    // Approach: Use Kotlin's functional transformations
    var part2Total = 0L
    
    for (prob in problems) {
        // Extract column strings using functional style
        val colStrings = (prob.startCol until prob.endCol)
            .filter { !isSpaceCol[it] }
            .map { col ->
                numRows.mapNotNull { row -> 
                    row.getOrNull(col)?.takeIf { it.isDigit() || it == ' ' }
                }.joinToString("").trim()
            }
            .filter { it.isNotEmpty() }
            .reversed()  // Right-to-left reading
        
        // Parse numbers
        val numbers = colStrings
            .map { it.filter { c -> c.isDigit() } }
            .filter { it.isNotEmpty() }
            .mapNotNull { it.toLongOrNull() }
        
        // Apply operator
        if (numbers.isNotEmpty()) {
            val result = if (prob.op == '+') {
                numbers.sum()
            } else {
                numbers.fold(1L) { acc, n -> acc * n }
            }
            part2Total += result
        }
    }
    
    return Pair(part1Total, part2Total)
}

fun main() {
    val content = readInputRaw("../data/input.txt")
    val (part1, part2) = solve(content)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
