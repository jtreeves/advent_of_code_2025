import java.io.File

data class MemoKey(val node: String, val visitedFFT: Boolean, val visitedDAC: Boolean)
fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n").filter { it.trim().isNotEmpty() }
    
    // Build graph: device -> list of outputs
    val graph = mutableMapOf<String, List<String>>()
    for (line in lines) {
        val parts = line.split(":")
        if (parts.size != 2) continue
        
        val device = parts[0].trim()
        val outputsStr = parts[1].trim()
        val outputs = if (outputsStr.isEmpty()) emptyList() else outputsStr.split(Regex("\\s+"))
        graph[device] = outputs
    }
    
    // Part 1: Count paths from "you" to "out"
    fun countPathsPart1(node: String, memo: MutableMap<String, Long>): Long {
        if (node == "out") {
            return 1
        }
        memo[node]?.let { return it }
        
        var count = 0L
        (graph[node] ?: emptyList()).forEach { neighbor ->
            count += countPathsPart1(neighbor, memo)
        }
        
        memo[node] = count
        return count
    }
    
    var part1Count = 0L
    if ("you" in graph) {
        val part1Memo = mutableMapOf<String, Long>()
        part1Count = countPathsPart1("you", part1Memo)
    }
    
    // Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    fun countPathsPart2(
        node: String,
        visitedFFT: Boolean,
        visitedDAC: Boolean,
        memo: MutableMap<MemoKey, Long>
    ): Long {
        if (node == "out") {
            return if (visitedFFT && visitedDAC) 1 else 0
        }
        
        val key = MemoKey(node, visitedFFT, visitedDAC)
        memo[key]?.let { return it }
        
        // Update flags when visiting fft or dac
        val newVisitedFFT = visitedFFT || (node == "fft")
        val newVisitedDAC = visitedDAC || (node == "dac")
        
        var count = 0L
        (graph[node] ?: emptyList()).forEach { neighbor ->
            count += countPathsPart2(neighbor, newVisitedFFT, newVisitedDAC, memo)
        }
        
        memo[key] = count
        return count
    }
    
    var part2Count = 0L
    if ("svr" in graph) {
        val part2Memo = mutableMapOf<MemoKey, Long>()
        part2Count = countPathsPart2("svr", false, false, part2Memo)
    }
    
    return Pair(part1Count.toString(), part2Count.toString())
}

fun main() {
    val inputData = File("../data/input.txt").readText()
    val (part1, part2) = solve(inputData)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
