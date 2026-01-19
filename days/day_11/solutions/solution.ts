import * as path from 'path';
import * as fs from 'fs';

// Import utilities
const getInputPath = (day: number): string => "../data/input.txt";
const readInputRaw = (filePath: string): string => {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
};

interface MemoKey {
    node: string;
    visitedFFT: boolean;
    visitedDAC: boolean;
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n').filter(line => line.trim());
    
    // Build graph: device -> list of outputs
    const graph: Map<string, string[]> = new Map();
    for (const line of lines) {
        const parts = line.split(':');
        if (parts.length !== 2) {
            continue;
        }
        const device = parts[0].trim();
        const outputsStr = parts[1].trim();
        const outputs = outputsStr === '' ? [] : outputsStr.split(/\s+/);
        graph.set(device, outputs);
    }
    
    // Part 1: Count paths from "you" to "out"
    function countPathsPart1(node: string, memo: Map<string, number>): number {
        if (node === "out") {
            return 1;
        }
        if (memo.has(node)) {
            return memo.get(node)!;
        }
        
        let count = 0;
        const neighbors = graph.get(node) || [];
        for (const neighbor of neighbors) {
            count += countPathsPart1(neighbor, memo);
        }
        
        memo.set(node, count);
        return count;
    }
    
    let part1Count = 0;
    if (graph.has("you")) {
        const part1Memo = new Map<string, number>();
        part1Count = countPathsPart1("you", part1Memo);
    }
    
    // Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    function memoKeyToString(key: MemoKey): string {
        return `${key.node}|${key.visitedFFT}|${key.visitedDAC}`;
    }
    
    function countPathsPart2(
        node: string,
        visitedFFT: boolean,
        visitedDAC: boolean,
        memo: Map<string, number>
    ): number {
        if (node === "out") {
            return (visitedFFT && visitedDAC) ? 1 : 0;
        }
        
        const key: MemoKey = { node, visitedFFT, visitedDAC };
        const keyStr = memoKeyToString(key);
        if (memo.has(keyStr)) {
            return memo.get(keyStr)!;
        }
        
        // Update flags when visiting fft or dac
        const newVisitedFFT = visitedFFT || (node === "fft");
        const newVisitedDAC = visitedDAC || (node === "dac");
        
        let count = 0;
        const neighbors = graph.get(node) || [];
        for (const neighbor of neighbors) {
            count += countPathsPart2(neighbor, newVisitedFFT, newVisitedDAC, memo);
        }
        
        memo.set(keyStr, count);
        return count;
    }
    
    let part2Count = 0;
    if (graph.has("svr")) {
        const part2Memo = new Map<string, number>();
        part2Count = countPathsPart2("svr", false, false, part2Memo);
    }
    
    return [part1Count.toString(), part2Count.toString()];
}

const data = readInputRaw("../data/input.txt");
const [part1, part2] = solve(data);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
