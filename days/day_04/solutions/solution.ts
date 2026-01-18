import * as path from 'path';
import * as fs from 'fs';

// Import utilities
const getInputPath = (day: number): string => "../data/input.txt";
const readInputRaw = (filePath: string): string => {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
};

function countNeighbors(grid: string[], i: number, j: number, rows: number, cols: number): number {
    let count = 0;
    for (let di = -1; di <= 1; di++) {
        for (let dj = -1; dj <= 1; dj++) {
            if (di === 0 && dj === 0) continue;
            const ni = i + di;
            const nj = j + dj;
            if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] === '@') {
                count++;
            }
        }
    }
    return count;
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n').filter(line => line.trim());
    const rows = lines.length;
    const cols = rows > 0 ? lines[0].length : 0;
    
    // Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    let part1Count = 0;
    for (let i = 0; i < rows; i++) {
        for (let j = 0; j < cols; j++) {
            if (lines[i][j] === '@') {
                const neighbors = countNeighbors(lines, i, j, rows, cols);
                if (neighbors < 4) {
                    part1Count++;
                }
            }
        }
    }
    
    // Part 2: Iteratively remove accessible rolls until none can be removed
    const grid: string[] = lines.map(line => line);
    
    let part2Count = 0;
    while (true) {
        const toRemove: [number, number][] = [];
        for (let i = 0; i < rows; i++) {
            for (let j = 0; j < cols; j++) {
                if (grid[i][j] === '@') {
                    const neighbors = countNeighbors(grid, i, j, rows, cols);
                    if (neighbors < 4) {
                        toRemove.push([i, j]);
                    }
                }
            }
        }
        
        if (toRemove.length === 0) {
            break;
        }
        
        // Remove all marked positions
        for (const [i, j] of toRemove) {
            grid[i] = grid[i].substring(0, j) + '.' + grid[i].substring(j + 1);
        }
        part2Count += toRemove.length;
    }
    
    return [part1Count.toString(), part2Count.toString()];
}

const data = readInputRaw("../data/input.txt");
const [part1, part2] = solve(data);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
