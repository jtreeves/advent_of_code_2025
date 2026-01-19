import * as fs from 'fs';
import * as path from 'path';

type Coord = [number, number, number];
type Pair = [number, number, number]; // [i, j, dist_sq]

class UnionFind {
    private parent: number[];
    private size: number[];
    public componentCount: number;

    constructor(n: number) {
        this.parent = Array.from({ length: n }, (_, i) => i);
        this.size = Array(n).fill(1);
        this.componentCount = n;
    }

    find(x: number): number {
        if (this.parent[x] !== x) {
            this.parent[x] = this.find(this.parent[x]);
        }
        return this.parent[x];
    }

    union(x: number, y: number): boolean {
        let rootX = this.find(x);
        let rootY = this.find(y);
        if (rootX === rootY) {
            return false;
        }
        if (this.size[rootX] < this.size[rootY]) {
            [rootX, rootY] = [rootY, rootX];
        }
        this.parent[rootY] = rootX;
        this.size[rootX] += this.size[rootY];
        this.componentCount--;
        return true;
    }
}

function parseCoordinates(lines: string[]): Coord[] {
    const coords: Coord[] = [];
    for (const line of lines) {
        const trimmed = line.trim();
        if (!trimmed) continue;
        const parts = trimmed.split(',');
        const x = parseInt(parts[0], 10);
        const y = parseInt(parts[1], 10);
        const z = parseInt(parts[2], 10);
        coords.push([x, y, z]);
    }
    return coords;
}

function squaredDistance(p1: Coord, p2: Coord): number {
    const dx = p2[0] - p1[0];
    const dy = p2[1] - p1[1];
    const dz = p2[2] - p1[2];
    return dx * dx + dy * dy + dz * dz;
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    const coords = parseCoordinates(lines);
    
    const n = coords.length;
    if (n === 0) {
        return ["0", "0"];
    }
    
    // Generate all pairs with squared distances
    const pairs: Pair[] = [];
    for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
            const distSq = squaredDistance(coords[i], coords[j]);
            pairs.push([i, j, distSq]);
        }
    }
    
    // Sort by distance
    pairs.sort((a, b) => a[2] - b[2]);
    
    // Part 1: Connect first 1000 pairs
    const uf1 = new UnionFind(n);
    let connectionsMade = 0;
    for (const [i, j] of pairs) {
        if (connectionsMade >= 1000) break;
        uf1.union(i, j);
        connectionsMade++;
    }
    
    // Get component sizes
    const componentSizes: Map<number, number> = new Map();
    for (let i = 0; i < n; i++) {
        const root = uf1.find(i);
        componentSizes.set(root, uf1['size'][root]);
    }
    
    const sizes = Array.from(componentSizes.values()).sort((a, b) => b - a);
    const part1 = sizes.length >= 3 ? sizes[0] * sizes[1] * sizes[2] : 0;
    
    // Part 2: Connect until all in one circuit
    const uf2 = new UnionFind(n);
    let finalPair: [number, number] | null = null;
    for (const [i, j] of pairs) {
        if (uf2.componentCount === 1) break;
        if (uf2.union(i, j)) {
            if (uf2.componentCount === 1) {
                finalPair = [i, j];
                break;
            }
        }
    }
    
    let part2 = 0;
    if (finalPair) {
        const [i, j] = finalPair;
        part2 = coords[i][0] * coords[j][0];
    }
    
    return [String(part1), String(part2)];
}

function main(): void {
    const inputPath = path.join(__dirname, '../data/input.txt');
    const data = fs.readFileSync(inputPath, 'utf-8');
    const [part1, part2] = solve(data);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

main();
