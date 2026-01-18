import * as fs from 'fs';
import * as path from 'path';

// Find the largest N-digit number by selecting N digits in order from bank
function findLargestSubsequence(bank: string, n: number): number {
    if (bank.length < n) {
        return 0;
    }
    
    const result: string[] = [];
    let start = 0;
    const bankLen = bank.length;
    
    for (let i = 0; i < n; i++) {
        const remainingNeeded = n - i - 1;
        const end = bankLen - remainingNeeded;
        
        let maxDigit = bank[start];
        let maxPos = start;
        for (let j = start + 1; j < end; j++) {
            if (bank[j] > maxDigit) {
                maxDigit = bank[j];
                maxPos = j;
            }
        }
        
        result.push(maxDigit);
        start = maxPos + 1;
    }
    
    return parseInt(result.join(''), 10);
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    
    let part1Sum = 0;
    let part2Sum = 0;
    
    for (const line of lines) {
        const bank = line.trim();
        if (bank === '') {
            continue;
        }
        
        part1Sum += findLargestSubsequence(bank, 2);
        
        if (bank.length >= 12) {
            part2Sum += findLargestSubsequence(bank, 12);
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
