// Placeholder for Day 01 TypeScript solution
import { readInputRaw } from '../../../utilities/typescript/get_input';

function solve(inputData: string): [string, string] {
    console.log("Day 01 TypeScript placeholder");
    const lines = inputData.trim().split('\n');
    console.log("Lines:", lines);
    
    // Part 1
    const part1Result = "TODO";
    
    // Part 2
    const part2Result = "TODO";
    
    return [part1Result, part2Result];
}

function main(): void {
    // Use utility function to get input
    const data = readInputRaw("../data/input.txt");
    const [part1, part2] = solve(data);
    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

main();
