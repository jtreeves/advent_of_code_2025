import java.util.regex.Pattern
import java.util.regex.Matcher

/**
 * Parsing utilities for Kotlin solutions.
 */

/**
 * Parse integers from a line of text.
 */
fun parseInts(line: String): List<Int> {
    val pattern = Pattern.compile("-?\\d+")
    val matcher = pattern.matcher(line)
    val result = mutableListOf<Int>()
    while (matcher.find()) {
        result.add(matcher.group().toInt())
    }
    return result
}
