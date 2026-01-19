import * as fs from 'fs';
import * as path from 'path';

type Point = [number, number];

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n').map(l => l.trim()).filter(l => l);
    
    // Parse coordinates
    const redTiles: Point[] = [];
    for (const line of lines) {
        if (line.includes(',')) {
            const parts = line.split(',');
            const x = parseInt(parts[0], 10);
            const y = parseInt(parts[1], 10);
            redTiles.push([x, y]);
        }
    }
    
    if (redTiles.length < 2) {
        return ["0", "0"];
    }
    
    // Part 1: Find largest rectangle area using any two red tiles as corners
    let maxAreaPart1 = 0;
    for (let i = 0; i < redTiles.length; i++) {
        for (let j = i + 1; j < redTiles.length; j++) {
            const [x1, y1] = redTiles[i];
            const [x2, y2] = redTiles[j];
            const width = Math.abs(x1 - x2) + 1;
            const height = Math.abs(y1 - y2) + 1;
            const area = width * height;
            maxAreaPart1 = Math.max(maxAreaPart1, area);
        }
    }
    
    // Part 2: Coordinate compression + flood-fill + prefix sums
    const allXSet = new Set<number>();
    const allYSet = new Set<number>();
    for (const [x, y] of redTiles) {
        allXSet.add(x);
        allXSet.add(x + 1);
        allYSet.add(y);
        allYSet.add(y + 1);
    }
    
    const allX = Array.from(allXSet).sort((a, b) => a - b);
    const allY = Array.from(allYSet).sort((a, b) => a - b);
    
    const xToCx = new Map<number, number>();
    const yToCy = new Map<number, number>();
    allX.forEach((x, i) => xToCx.set(x, i));
    allY.forEach((y, i) => yToCy.set(y, i));
    
    const width = allX.length;
    const height = allY.length;
    
    // Build grid
    const grid: boolean[][] = Array(width).fill(0).map(() => Array(height).fill(false));
    
    // Mark boundary
    for (const [x, y] of redTiles) {
        const cx = xToCx.get(x);
        const cy = yToCy.get(y);
        if (cx !== undefined && cy !== undefined) {
            grid[cx][cy] = true;
        }
    }
    
    // Connect consecutive red tiles
    for (let i = 0; i < redTiles.length; i++) {
        const [x1, y1] = redTiles[i];
        const [x2, y2] = redTiles[(i + 1) % redTiles.length];
        
        if (x1 === x2) {
            const startY = Math.min(y1, y2);
            const endY = Math.max(y1, y2);
            for (let y = startY; y <= endY; y++) {
                const cx = xToCx.get(x1);
                const cy = yToCy.get(y);
                if (cx !== undefined && cy !== undefined) {
                    grid[cx][cy] = true;
                }
            }
        } else if (y1 === y2) {
            const startX = Math.min(x1, x2);
            const endX = Math.max(x1, x2);
            for (let x = startX; x <= endX; x++) {
                const cx = xToCx.get(x);
                const cy = yToCy.get(y1);
                if (cx !== undefined && cy !== undefined) {
                    grid[cx][cy] = true;
                }
            }
        }
    }
    
    // Point-in-polygon
    function pointInPolygon(px: number, py: number): boolean {
        let inside = false;
        for (let i = 0; i < redTiles.length; i++) {
            const [x1, y1] = redTiles[i];
            const [x2, y2] = redTiles[(i + 1) % redTiles.length];
            if ((y1 > py) !== (y2 > py)) {
                const intersectX = y2 !== y1 
                    ? (py - y1) * (x2 - x1) / (y2 - y1) + x1
                    : px;
                if (px < intersectX) {
                    inside = !inside;
                }
            }
        }
        return inside;
    }
    
    // Flood fill interior
    let foundInterior = false;
    for (let cx = 0; cx < width && !foundInterior; cx++) {
        for (let cy = 0; cy < height && !foundInterior; cy++) {
            if (!grid[cx][cy]) {
                const origX = allX[cx];
                const origY = allY[cy];
                if (pointInPolygon(origX, origY)) {
                    const stack: [number, number][] = [[cx, cy]];
                    while (stack.length > 0) {
                        const [x, y] = stack.pop()!;
                        if (x >= width || y >= height || grid[x][y]) continue;
                        const origX = allX[x];
                        const origY = allY[y];
                        if (pointInPolygon(origX, origY)) {
                            grid[x][y] = true;
                            if (x > 0) stack.push([x - 1, y]);
                            if (x + 1 < width) stack.push([x + 1, y]);
                            if (y > 0) stack.push([x, y - 1]);
                            if (y + 1 < height) stack.push([x, y + 1]);
                        }
                    }
                    foundInterior = true;
                }
            }
        }
    }
    
    // Build 2D prefix sum
    const prefix: number[][] = Array(width + 1).fill(0).map(() => Array(height + 1).fill(0));
    for (let cx = 0; cx < width; cx++) {
        for (let cy = 0; cy < height; cy++) {
            prefix[cx + 1][cy + 1] = prefix[cx][cy + 1] + prefix[cx + 1][cy] 
                - prefix[cx][cy] + (grid[cx][cy] ? 1 : 0);
        }
    }
    
    function rectSum(cx1: number, cx2: number, cy1: number, cy2: number): number {
        return prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] 
            - prefix[cx2 + 1][cy1] + prefix[cx1][cy1];
    }
    
    // Generate candidates sorted by area descending
    const candidates: Array<[number, number, number, number, number, number, number, number, number]> = [];
    for (let i = 0; i < redTiles.length; i++) {
        for (let j = i + 1; j < redTiles.length; j++) {
            const [x1, y1] = redTiles[i];
            const [x2, y2] = redTiles[j];
            const minX = Math.min(x1, x2);
            const maxX = Math.max(x1, x2);
            const minY = Math.min(y1, y2);
            const maxY = Math.max(y1, y2);
            const area = (maxX - minX + 1) * (maxY - minY + 1);
            
            const cx1 = xToCx.get(minX);
            const cx2 = xToCx.get(maxX);
            const cy1 = yToCy.get(minY);
            const cy2 = yToCy.get(maxY);
            if (cx1 !== undefined && cx2 !== undefined && cy1 !== undefined && cy2 !== undefined) {
                candidates.push([minX, maxX, minY, maxY, area, cx1, cx2, cy1, cy2]);
            }
        }
    }
    
    candidates.sort((a, b) => b[4] - a[4]);
    
    // Check candidates
    let maxAreaPart2 = 0;
    for (const [minX, maxX, minY, maxY, area, cx1, cx2, cy1, cy2] of candidates) {
        if (area <= maxAreaPart2) break;
        
        const validCount = rectSum(cx1, cx2, cy1, cy2);
        const expectedCells = (cx2 - cx1 + 1) * (cy2 - cy1 + 1);
        
        if (validCount === expectedCells) {
            let allValid = true;
            for (const [x, y] of [[minX, minY], [minX, maxY], [maxX, minY], [maxX, maxY]]) {
                const cx = xToCx.get(x);
                const cy = yToCy.get(y);
                if (cx !== undefined && cy !== undefined) {
                    if (!grid[cx][cy]) {
                        allValid = false;
                        break;
                    }
                } else {
                    if (!pointInPolygon(x, y)) {
                        allValid = false;
                        break;
                    }
                }
            }
            
            if (allValid) {
                maxAreaPart2 = area;
                break;
            }
        }
    }
    
    return [String(maxAreaPart1), String(maxAreaPart2)];
}

function main(): void {
    const inputPath = path.join(__dirname, '../data/input.txt');
    const data = fs.readFileSync(inputPath, 'utf-8');
    const [part1, part2] = solve(data);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

main();
