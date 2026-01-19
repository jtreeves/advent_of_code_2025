import * as fs from 'fs';

function solve(inputData: string): [number, number] {
    const lines = inputData.trim().split('\n');
    
    // Find blank line separator
    let blankIdx = -1;
    for (let i = 0; i < lines.length; i++) {
        if (lines[i].trim() === '') {
            blankIdx = i;
            break;
        }
    }
    
    // Parse ranges (first section)
    const ranges: Array<[number, number]> = [];
    for (let i = 0; i < blankIdx; i++) {
        const line = lines[i].trim();
        if (line === '') continue;
        const parts = line.split('-');
        const start = parseInt(parts[0], 10);
        const end = parseInt(parts[1], 10);
        ranges.push([start, end]);
    }
    
    // Parse IDs to check (second section)
    const ids: number[] = [];
    for (let i = blankIdx + 1; i < lines.length; i++) {
        const line = lines[i].trim();
        if (line === '') continue;
        ids.push(parseInt(line, 10));
    }
    
    // Part 1: Count how many IDs fall into any range
    let part1Count = 0;
    for (const idVal of ids) {
        for (const [start, end] of ranges) {
            if (start <= idVal && idVal <= end) {
                part1Count++;
                break;
            }
        }
    }
    
    // Part 2: Merge ranges and count total unique IDs covered
    // Sort ranges by start value
    ranges.sort((a, b) => a[0] - b[0]);
    
    // Merge overlapping/adjacent ranges
    const merged: Array<[number, number]> = [];
    if (ranges.length > 0) {
        merged.push([ranges[0][0], ranges[0][1]]);
        for (let i = 1; i < ranges.length; i++) {
            const [start, end] = ranges[i];
            const last = merged[merged.length - 1];
            // Check if overlaps or is adjacent (start <= last[1] + 1)
            if (start <= last[1] + 1) {
                // Merge: update end to max of both ends
                last[1] = Math.max(last[1], end);
            } else {
                // No overlap, add as new range
                merged.push([start, end]);
            }
        }
    }
    
    // Calculate total unique IDs covered
    let part2Total = 0;
    for (const [start, end] of merged) {
        part2Total += end - start + 1;
    }
    
    return [part1Count, part2Total];
}

const content = fs.readFileSync('../data/input.txt', 'utf-8');
const [part1, part2] = solve(content);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
