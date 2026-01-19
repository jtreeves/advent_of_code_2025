import * as fs from 'fs';

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    if (lines.length === 0) {
        return ["0", "0"];
    }
    
    const grid = lines.map(line => line.split(''));
    const rows = grid.length;
    const cols = grid[0]?.length || 0;
    
    // Find starting position S
    let startRow = -1;
    let startCol = -1;
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 'S') {
                startRow = r;
                startCol = c;
                break;
            }
        }
        if (startRow !== -1) break;
    }
    
    if (startRow === -1) {
        return ["0", "0"];
    }
    
    // Part 1: Count total splits
    let splitCount = 0;
    let activeBeams = new Set<number>([startCol]);
    
    // Process each row starting from the row after S
    for (let r = startRow + 1; r < rows; r++) {
        const nextBeams = new Set<number>();
        for (const col of activeBeams) {
            if (grid[r][col] === '.') {
                // Beam continues down
                nextBeams.add(col);
            } else if (grid[r][col] === '^') {
                // Beam splits
                splitCount++;
                // Add beams to left and right
                if (col - 1 >= 0) {
                    nextBeams.add(col - 1);
                }
                if (col + 1 < cols) {
                    nextBeams.add(col + 1);
                }
            }
        }
        activeBeams = nextBeams;
    }
    
    // Part 2: Count beams reaching bottom row
    // Use a 2D count array to track beam counts per cell
    const beamCounts: number[][] = Array(rows).fill(0).map(() => Array(cols).fill(0));
    beamCounts[startRow][startCol] = 1; // Start with 1 beam at S
    
    // Process each row starting from the row after S
    for (let r = startRow + 1; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            const prevCount = beamCounts[r - 1][c];
            if (prevCount > 0) {
                if (grid[r][c] === '.') {
                    // Beam continues down
                    beamCounts[r][c] += prevCount;
                } else if (grid[r][c] === '^') {
                    // Beam splits into left and right
                    if (c - 1 >= 0) {
                        beamCounts[r][c - 1] += prevCount;
                    }
                    if (c + 1 < cols) {
                        beamCounts[r][c + 1] += prevCount;
                    }
                }
            }
        }
    }
    
    // Sum all beams in bottom row
    const bottomBeamCount = beamCounts[rows - 1].reduce((sum, count) => sum + count, 0);
    
    return [splitCount.toString(), bottomBeamCount.toString()];
}

const content = fs.readFileSync('../data/input.txt', 'utf-8');
const [part1, part2] = solve(content);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
