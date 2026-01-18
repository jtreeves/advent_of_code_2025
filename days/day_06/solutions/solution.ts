import * as fs from 'fs';
import * as path from 'path';

function readInputRaw(filePath: string): string {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    if (lines.length === 0) {
        return ["0", "0"];
    }
    
    // Find maximum line length and pad all lines
    const maxLen = Math.max(...lines.map(line => line.length));
    const paddedLines = lines.map(line => line.padEnd(maxLen));
    
    // Operator row is the last row
    const opRowIdx = paddedLines.length - 1;
    const opRow = paddedLines[opRowIdx];
    const numRows = paddedLines.slice(0, opRowIdx);
    
    // Part 1: Parse horizontally
    let part1Total = 0;
    
    // Find problem boundaries (columns that are all spaces)
    const isSpaceCol: boolean[] = [];
    for (let col = 0; col < maxLen; col++) {
        const allSpaces = paddedLines.every(line => line[col] === ' ');
        isSpaceCol.push(allSpaces);
    }
    
    // Group columns into problems
    const problems: Array<[number, number, string]> = [];
    let i = 0;
    while (i < maxLen) {
        if (!isSpaceCol[i]) {
            // Start of a problem
            const startCol = i;
            while (i < maxLen && !isSpaceCol[i]) {
                i++;
            }
            const endCol = i;
            // Extract operator for this problem
            let op: string | null = null;
            for (let j = startCol; j < endCol; j++) {
                if (opRow[j] === '+' || opRow[j] === '*') {
                    op = opRow[j];
                    break;
                }
            }
            if (op) {
                problems.push([startCol, endCol, op]);
            }
        } else {
            i++;
        }
    }
    
    // Solve Part 1: Extract numbers horizontally
    for (const [startCol, endCol, op] of problems) {
        const numbers: number[] = [];
        // Extract numbers from each row in this problem area
        for (const row of numRows) {
            // Extract the substring for this problem
            const problemStr = row.substring(startCol, endCol).trim();
            // Split by spaces and parse numbers
            const parts = problemStr.split(/\s+/);
            for (const part of parts) {
                const num = parseInt(part, 10);
                if (!isNaN(num)) {
                    numbers.push(num);
                }
            }
        }
        
        // Apply operator
        if (numbers.length > 0) {
            let result: number;
            if (op === '+') {
                result = numbers.reduce((sum, n) => sum + n, 0);
            } else {
                result = numbers.reduce((prod, n) => prod * n, 1);
            }
            part1Total += result;
        }
    }
    
    // Part 2: Parse vertically (columns, right-to-left)
    // Approach: Build numbers by reading each column's digits top-to-bottom,
    // then combine adjacent columns that form multi-digit numbers
    let part2Total = 0;
    
    for (const [startCol, endCol, op] of problems) {
        const numbers: number[] = [];
        
        // Read columns right-to-left, building numbers
        // Each column may contribute to forming numbers
        const colValues: string[] = [];
        
        // First pass: read each column top-to-bottom to get digit strings
        for (let col = endCol - 1; col >= startCol; col--) {
            if (isSpaceCol[col]) continue;
            
            let colStr = "";
            for (const row of numRows) {
                const ch = row[col] || ' ';
                if (/\d/.test(ch)) {
                    colStr += ch;
                }
            }
            if (colStr) {
                colValues.unshift(colStr);
            }
        }
        
        // Combine adjacent columns that should form multi-digit numbers
        // (heuristic: if a column has multiple digits, it might be its own number)
        // Try reading each column as a number, grouping as needed
        for (let i = 0; i < colValues.length; i++) {
            const colVal = colValues[i];
            if (colVal) {
                // Try parsing as a number - each column contributes one number
                const num = parseInt(colVal, 10);
                if (!isNaN(num)) {
                    numbers.push(num);
                }
            }
        }
        
        // Apply operator
        if (numbers.length > 0) {
            let result: number;
            if (op === '+') {
                result = numbers.reduce((sum, n) => sum + n, 0);
            } else {
                result = numbers.reduce((prod, n) => prod * n, 1);
            }
            part2Total += result;
        }
    }
    
    return [part1Total.toString(), part2Total.toString()];
}

const content = readInputRaw('../data/input.txt');
const [part1, part2] = solve(content);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
