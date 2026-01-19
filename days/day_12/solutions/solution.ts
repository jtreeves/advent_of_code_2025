import * as fs from 'fs';
import * as path from 'path';

function readInputRaw(filePath: string): string {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    if (lines.length === 0) {
        return ["0", "Final star"];
    }
    
    // Parse shapes (first 6 shapes, numbered 0-5)
    const shapeAreas: number[] = [0, 0, 0, 0, 0, 0];
    let i = 0;
    let shapeIdx = 0;
    
    while (i < lines.length && shapeIdx < 6) {
        const line = lines[i].trim();
        // Check if this is a shape header (format: "number:")
        if (line && line.endsWith(':') && /^\d+$/.test(line.slice(0, -1))) {
            const shapeNum = parseInt(line.slice(0, -1), 10);
            if (shapeNum === shapeIdx) {
                // Read the next 3 lines for the shape grid
                const shapeGrid: string[] = [];
                for (let j = 0; j < 3; j++) {
                    if (i + 1 + j < lines.length) {
                        shapeGrid.push(lines[i + 1 + j].trim());
                    } else {
                        shapeGrid.push("");
                    }
                }
                
                // Count '#' characters in the shape
                let area = 0;
                for (const row of shapeGrid) {
                    for (const char of row) {
                        if (char === '#') {
                            area++;
                        }
                    }
                }
                shapeAreas[shapeIdx] = area;
                shapeIdx++;
                i += 4; // Skip shape header + 3 grid lines + empty line (if present)
                continue;
            }
        }
        i++;
    }
    
    // Find where queries start (skip empty lines after shapes)
    let queryStart = i;
    while (queryStart < lines.length && !lines[queryStart].trim()) {
        queryStart++;
    }
    
    // Parse queries
    let possibleCount = 0;
    for (let lineIdx = queryStart; lineIdx < lines.length; lineIdx++) {
        const line = lines[lineIdx].trim();
        if (!line) {
            continue;
        }
        
        // Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        if (!line.includes(':')) {
            continue;
        }
        
        const parts = line.split(':', 2);
        if (parts.length !== 2) {
            continue;
        }
        
        // Parse dimensions
        const dims = parts[0].trim();
        if (!dims.includes('x')) {
            continue;
        }
        
        const dimParts = dims.split('x');
        if (dimParts.length !== 2) {
            continue;
        }
        
        const width = parseInt(dimParts[0], 10);
        const height = parseInt(dimParts[1], 10);
        if (isNaN(width) || isNaN(height)) {
            continue;
        }
        
        // Parse counts
        const countParts = parts[1].trim().split(/\s+/);
        if (countParts.length !== 6) {
            continue;
        }
        
        const counts: number[] = [];
        let validCounts = true;
        for (const c of countParts) {
            const count = parseInt(c, 10);
            if (isNaN(count)) {
                validCounts = false;
                break;
            }
            counts.push(count);
        }
        
        if (!validCounts) {
            continue;
        }
        
        // Calculate area check
        const regionArea = width * height;
        let requiredArea = 0;
        for (let j = 0; j < 6; j++) {
            requiredArea += shapeAreas[j] * counts[j];
        }
        
        if (requiredArea <= regionArea) {
            possibleCount++;
        }
    }
    
    // Part 2: Final star (no computation needed)
    const part2 = "Final star";
    
    return [possibleCount.toString(), part2];
}

if (require.main === module) {
    const data = readInputRaw("../data/input.txt");
    const [part1, part2] = solve(data);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}
