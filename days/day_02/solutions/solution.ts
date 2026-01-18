import * as fs from 'fs';
import * as path from 'path';

// Check if ID is invalid for Part 1: exactly two identical sequences
function isInvalidPart1(idStr: string): boolean {
    const n = idStr.length;
    if (n % 2 !== 0) {
        return false;
    }
    const half = n / 2;
    return idStr.slice(0, half) === idStr.slice(half);
}

// Check if ID is invalid for Part 2: sequence repeated 2+ times
function isInvalidPart2(idStr: string): boolean {
    const n = idStr.length;
    for (let k = 2; k <= n; k++) {
        if (n % k === 0) {
            const seqLen = n / k;
            const pattern = idStr.slice(0, seqLen);
            const repeated = pattern.repeat(k);
            if (idStr === repeated) {
                return true;
            }
        }
    }
    return false;
}

// Parse a range string like "start-end"
function parseRange(rangeStr: string): [number, number] {
    const parts = rangeStr.split('-');
    const start = parseInt(parts[0].trim(), 10);
    const end = parseInt(parts[1].trim(), 10);
    return [start, end];
}

// Parse a line of comma-separated ranges
function parseRanges(line: string): Array<[number, number]> {
    return line.split(',')
        .map(s => s.trim())
        .filter(s => s.length > 0)
        .map(parseRange);
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    
    let part1Sum = 0;
    let part2Sum = 0;
    
    for (const line of lines) {
        if (line.trim() === '') {
            continue;
        }
        const ranges = parseRanges(line);
        
        for (const [start, end] of ranges) {
            for (let num = start; num <= end; num++) {
                const idStr = num.toString();
                
                if (isInvalidPart1(idStr)) {
                    part1Sum += num;
                }
                
                if (isInvalidPart2(idStr)) {
                    part2Sum += num;
                }
            }
        }
    }
    
    return [part1Sum.toString(), part2Sum.toString()];
}

function main(): void {
    const inputPath = path.join(__dirname, '../data/input.txt');
    const data = fs.readFileSync(inputPath, 'utf-8');
    const [part1, part2] = solve(data);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

main();
