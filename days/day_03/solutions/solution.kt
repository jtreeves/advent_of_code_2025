import java.io.File

// Find the largest N-digit number by selecting N digits in order from bank
fun findLargestSubsequence(bank: String, n: Int): Long {
    val bankLen = bank.length
    if (bankLen < n) {
        return 0
    }
    
    val result = StringBuilder()
    var start = 0
    
    for (i in 0 until n) {
        val remainingNeeded = n - i - 1
        val end = bankLen - remainingNeeded
        
        var maxDigit = bank[start]
        var maxPos = start
        for (j in (start + 1) until end) {
            if (bank[j] > maxDigit) {
                maxDigit = bank[j]
                maxPos = j
            }
        }
        
        result.append(maxDigit)
        start = maxPos + 1
    }
    
    return result.toString().toLong()
}

fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n")
    
    var part1Sum = 0L
    var part2Sum = 0L
    
    for (line in lines) {
        val bank = line.trim()
        if (bank.isEmpty()) {
            continue
        }
        
        part1Sum += findLargestSubsequence(bank, 2)
        
        if (bank.length >= 12) {
            part2Sum += findLargestSubsequence(bank, 12)
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
