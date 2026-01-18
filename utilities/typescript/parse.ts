/**
 * Parsing utilities for TypeScript solutions.
 */

/**
 * Parse integers from a line of text.
 * @param line Line of text containing integers
 * @returns Array of integers found in the line
 */
export function parseInts(line: string): number[] {
    return line.match(/-?\d+/g)?.map(Number) || [];
}
