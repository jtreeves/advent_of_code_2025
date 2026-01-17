import * as fs from 'fs';
import * as path from 'path';

/**
 * Common utility functions for TypeScript solutions.
 */

export function readInput(filePath: string): string[] {
    return fs.readFileSync(filePath, 'utf-8')
        .trim()
        .split('\n');
}

export function readInputRaw(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8');
}

export function parseInts(line: string): number[] {
    return line.match(/-?\d+/g)?.map(Number) || [];
}

// Placeholder for additional common utilities
