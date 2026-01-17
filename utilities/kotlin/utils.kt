import java.io.File
import kotlin.text.Regex

/**
 * Common utility functions for Kotlin solutions.
 */

fun readInput(filePath: String): List<String> {
    return File(filePath).readLines().map { it.trim() }
}

fun readInputRaw(filePath: String): String {
    return File(filePath).readText()
}

fun parseInts(line: String): List<Int> {
    val regex = Regex("-?\\d+")
    return regex.findAll(line).map { it.value.toInt() }.toList()
}

// Placeholder for additional common utilities
