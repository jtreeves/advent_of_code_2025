import java.io.File
import java.io.BufferedReader
import java.io.FileReader

/**
 * Input reading utilities for Kotlin solutions.
 */

/**
 * Read input from input.txt for the given day.
 */
fun getInput(day: Int): List<String> {
    val path = getInputPath(day)
    return readInput(path)
}

/**
 * Read input from test_N.txt for the given day and test number.
 */
fun getTestInput(day: Int, testNum: Int): List<String> {
    val path = getTestInputPath(day, testNum)
    return readInput(path)
}

/**
 * Return the path to input.txt for the given day.
 */
fun getInputPath(day: Int): String {
    return "../data/input.txt"
}

/**
 * Return the path to test_N.txt for the given day and test number.
 */
fun getTestInputPath(day: Int, testNum: Int): String {
    return "../data/test_$testNum.txt"
}

/**
 * Read input file and return lines as a list.
 */
fun readInput(filePath: String): List<String> {
    val file = File(filePath)
    val lines = mutableListOf<String>()
    BufferedReader(FileReader(file)).use { reader ->
        reader.forEachLine { line ->
            lines.add(line.trim())
        }
    }
    return lines
}

/**
 * Read input file and return raw content.
 */
fun readInputRaw(filePath: String): String {
    val file = File(filePath)
    val sb = StringBuilder()
    BufferedReader(FileReader(file)).use { reader ->
        reader.forEachLine { line ->
            sb.append(line).append("\n")
        }
    }
    // Remove trailing newline if file ended with one
    if (sb.isNotEmpty() && sb.last() == '\n') {
        sb.deleteCharAt(sb.length - 1)
    }
    return sb.toString()
}
