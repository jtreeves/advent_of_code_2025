import kotlin.text.Regex

/**
 * Parsing utilities for Kotlin solutions.
 */

/**
 * Parse integers from a line of text.
 */
fun parseInts(line: String): List<Int> {
    val regex = Regex("-?\\d+")
    return regex.findAll(line).map { it.value.toInt() }.toList()
}
