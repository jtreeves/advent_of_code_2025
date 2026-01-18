// Placeholder for Day 01 Kotlin solution
// Note: Kotlin utility functions would be in utilities/kotlin/get_input.kt
// For now, using inline function - will be replaced with proper imports

fun readInputRaw(path: String): String = java.io.File(path).readText()

fun solve(inputData: String): Pair<String, String> {
    println("Day 01 Kotlin placeholder")
    val lines = inputData.trim().split("\n")
    println("Lines: $lines")
    
    // Part 1
    val part1Result = "TODO"
    
    // Part 2
    val part2Result = "TODO"
    
    return Pair(part1Result, part2Result)
}

fun main() {
    // Use utility function to get input
    val data = readInputRaw("../data/input.txt")
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
