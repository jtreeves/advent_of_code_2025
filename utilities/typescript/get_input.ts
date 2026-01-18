import * as fs from 'fs';
import * as path from 'path';

/**
 * Input reading utilities for TypeScript solutions.
 */

/**
 * Read input from input.txt for the given day.
 * @param day Day number (1, 2, 3, etc.)
 * @returns Array of lines as strings
 */
export function getInput(day: number): string[] {
    const filePath = getInputPath(day);
    return readInput(filePath);
}

/**
 * Read input from test_N.txt for the given day and test number.
 * @param day Day number (1, 2, 3, etc.)
 * @param testNum Test number (1, 2, 3, etc.)
 * @returns Array of lines as strings
 */
export function getTestInput(day: number, testNum: number): string[] {
    const filePath = getTestInputPath(day, testNum);
    return readInput(filePath);
}

/**
 * Return the path to input.txt for the given day.
 * Solutions are in days/day_NN/solutions/
 * Input is in days/day_NN/data/input.txt
 */
export function getInputPath(day: number): string {
    return "../data/input.txt";
}

/**
 * Return the path to test_N.txt for the given day and test number.
 */
export function getTestInputPath(day: number, testNum: number): string {
    return `../data/test_${testNum}.txt`;
}

/**
 * Read input file and return lines as an array.
 */
export function readInput(filePath: string): string[] {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8')
        .trim()
        .split('\n')
        .map(line => line.trim());
}

/**
 * Read input file and return raw content.
 */
export function readInputRaw(filePath: string): string {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
}
