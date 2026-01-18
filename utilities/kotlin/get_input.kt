import java.io.File

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
    return File(filePath).readLines().map { it.trim() }
}

/**
 * Read input file and return raw content.
 */
fun readInputRaw(filePath: String): String {
    return File(filePath).readText()
}
