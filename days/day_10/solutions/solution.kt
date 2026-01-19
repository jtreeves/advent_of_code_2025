import java.io.File
import kotlin.text.Regex

data class ParseResult(
    val targetPattern: List<Boolean>,
    val buttons: List<List<Int>>,
    val joltages: List<Int>
)

fun parseLine(line: String): ParseResult {
    // Extract pattern: [.##.]
    val patternRe = Regex("\\[([.#]+)\\]")
    val patternMatch = patternRe.find(line)
    val targetPattern = patternMatch?.groupValues?.get(1)?.map { it == '#' } ?: emptyList()
    
    // Extract buttons: (1,3) (2) etc.
    val buttonRe = Regex("\\(([^)]*)\\)")
    val buttons = buttonRe.findAll(line).map { match ->
        val btnStr = match.groupValues[1].trim()
        if (btnStr.isEmpty()) {
            emptyList<Int>()
        } else {
            btnStr.split(",").mapNotNull { it.trim().toIntOrNull() }
        }
    }.toList()
    
    // Extract joltages: {3,5,4,7}
    val joltageRe = Regex("\\{([^}]+)\\}")
    val joltageMatch = joltageRe.find(line)
    val joltages = joltageMatch?.groupValues?.get(1)?.split(",")
        ?.mapNotNull { it.trim().toIntOrNull() } ?: emptyList()
    
    return ParseResult(targetPattern, buttons, joltages)
}

fun gaussianEliminationGF2(matrix: List<List<Boolean>>, target: List<Boolean>): Int? {
    val numButtons = matrix.size
    val numLights = target.size
    
    // Create augmented matrix [A | b]
    val aug = mutableListOf<MutableList<Boolean>>()
    for (i in 0 until numLights) {
        val row = mutableListOf<Boolean>()
        for (j in 0 until numButtons) {
            row.add(if (j < matrix.size && i < matrix[j].size) matrix[j][i] else false)
        }
        row.add(target[i])
        aug.add(row)
    }
    
    // Gaussian elimination mod 2
    var pivotRow = 0
    var pivotCol = 0
    
    while (pivotRow < numLights && pivotCol < numButtons) {
        // Find pivot
        var pivotIdx: Int? = null
        for (i in pivotRow until numLights) {
            if (aug[i][pivotCol]) {
                pivotIdx = i
                break
            }
        }
        
        if (pivotIdx == null) {
            pivotCol++
            continue
        }
        
        // Swap rows
        if (pivotIdx != pivotRow) {
            val temp = aug[pivotRow]
            aug[pivotRow] = aug[pivotIdx]
            aug[pivotIdx] = temp
        }
        
        // Eliminate
        for (i in (pivotRow + 1) until numLights) {
            if (aug[i][pivotCol]) {
                for (j in 0..numButtons) {
                    aug[i][j] = aug[i][j] != aug[pivotRow][j]
                }
            }
        }
        
        pivotRow++
        pivotCol++
    }
    
    // Check for inconsistency (0 = 1)
    for (i in pivotRow until numLights) {
        if (aug[i][numButtons] && aug[i].take(numButtons).none { it }) {
            return null // No solution
        }
    }
    
    // Back substitution to find solution (set free variables to 0 to minimize)
    val solution = MutableList(numButtons) { false }
    val usedRows = mutableSetOf<Int>()
    
    // Process from bottom up
    for (i in (numLights - 1) downTo 0) {
        // Find first non-zero column
        var pivotColIdx: Int? = null
        for (j in 0 until numButtons) {
            if (aug[i][j] && j !in usedRows) {
                pivotColIdx = j
                usedRows.add(j)
                break
            }
        }
        
        pivotColIdx?.let { idx ->
            // Calculate value
            var val_ = aug[i][numButtons]
            for (j in (idx + 1) until numButtons) {
                if (aug[i][j] && solution[j]) {
                    val_ = !val_
                }
            }
            solution[idx] = val_
        }
    }
    
    // Count number of presses
    return solution.count { it }
}

fun solvePart2ILP(buttons: List<List<Int>>, joltages: List<Int>): Int? {
    val numButtons = buttons.size
    val numLights = joltages.size
    
    // Bounded search: max presses per button is max joltage
    val maxJoltage = joltages.maxOrNull() ?: 0
    
    fun dfs(buttonIdx: Int, currentJoltages: List<Int>, pressesSoFar: Int, best: Int?): Int? {
        if (buttonIdx >= numButtons) {
            if (currentJoltages.zip(joltages).all { (a, b) -> a == b }) {
                return best?.let { minOf(it, pressesSoFar) } ?: pressesSoFar
            }
            return best
        }
        
        // Pruning: if current presses already exceed best, skip
        best?.let { if (pressesSoFar >= it) return best }
        
        // Calculate minimum additional presses needed (lower bound)
        val remainingNeeds = currentJoltages.zip(joltages).map { (curr, target) -> maxOf(0, target - curr) }
        val sumRemaining = remainingNeeds.sum()
        if (sumRemaining > 0) {
            val maxLightsPerButton = buttons.drop(buttonIdx).maxOfOrNull { it.size } ?: 1
            if (maxLightsPerButton > 0) {
                val minAdditional = (sumRemaining + maxLightsPerButton - 1) / maxLightsPerButton
                best?.let { if (pressesSoFar + minAdditional >= it) return best }
            }
        }
        
        // Try 0 to maxJoltage presses of current button
        var bestResult = best
        for (presses in 0..maxJoltage) {
            best?.let { if (pressesSoFar + presses >= it) break }
            
            val newJoltages = currentJoltages.toMutableList()
            for (light in buttons[buttonIdx]) {
                if (light >= 0 && light < numLights) {
                    newJoltages[light] += presses
                }
            }
            
            // Check if any exceeds target (pruning)
            if (newJoltages.zip(joltages).any { (a, b) -> a > b }) {
                continue
            }
            
            val result = dfs(buttonIdx + 1, newJoltages, pressesSoFar + presses, bestResult)
            result?.let { bestResult = bestResult?.let { minOf(it, result) } ?: result }
        }
        
        return bestResult
    }
    
    // Start with all joltages at 0
    val initialJoltages = List(numLights) { 0 }
    return dfs(0, initialJoltages, 0, null)
}

fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n")
    if (lines.isEmpty()) return Pair("0", "0")
    
    var part1Total = 0
    var part2Total = 0
    
    for (line in lines) {
        if (line.trim().isEmpty()) continue
        
        val parsed = parseLine(line)
        if (parsed.targetPattern.isEmpty()) continue
        
        val numLights = parsed.targetPattern.size
        
        // Part 1: GF(2) linear system
        // Build incidence matrix: matrix[i][j] = true if button i toggles light j
        val buttonMatrix = List(parsed.buttons.size) { MutableList(numLights) { false } }
        for ((i, btn) in parsed.buttons.withIndex()) {
            for (light in btn) {
                if (light >= 0 && light < numLights) {
                    buttonMatrix[i][light] = true
                }
            }
        }
        
        // Target: all start OFF, need to toggle to match pattern
        // Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        val requiredToggles = parsed.targetPattern.toList()
        
        val result = gaussianEliminationGF2(buttonMatrix.map { it.toList() }, requiredToggles)
        result?.let { part1Total += it }
        
        // Part 2: Integer Linear Programming
        if (parsed.joltages.size == numLights) {
            val result2 = solvePart2ILP(parsed.buttons, parsed.joltages)
            result2?.let { part2Total += it }
        }
    }
    
    return Pair(part1Total.toString(), part2Total.toString())
}

fun main() {
    val data = File("../data/input.txt").readText()
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
