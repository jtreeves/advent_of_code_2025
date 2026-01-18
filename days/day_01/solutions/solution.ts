import * as path from 'path';
import * as fs from 'fs';

// Import utilities
const getInputPath = (day: number): string => "../data/input.txt";
const readInputRaw = (filePath: string): string => {
    const absPath = path.resolve(filePath);
    return fs.readFileSync(absPath, 'utf-8');
};

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n').filter(line => line.trim());
    
    // Part 1: Count times dial ends at 0 after a rotation
    let position = 50;
    let countPart1 = 0;
    
    for (const line of lines) {
        const direction = line[0];
        const distance = parseInt(line.slice(1));
        
        // Apply rotation
        if (direction === 'L') {
            position = ((position - distance) % 100 + 100) % 100;
        } else { // direction === 'R'
            position = (position + distance) % 100;
        }
        
        // Check if ended at 0
        if (position === 0) {
            countPart1++;
        }
    }
    
    // Part 2: Count times dial is at 0 during entire process
    position = 50;
    let countPart2 = 0;
    
    for (const line of lines) {
        const direction = line[0];
        const distance = parseInt(line.slice(1));
        
        const startPos = position;
        
        // Check each click position during rotation
        for (let click = 1; click <= distance; click++) {
            let clickPos: number;
            if (direction === 'L') {
                clickPos = ((startPos - click) % 100 + 100) % 100;
            } else { // direction === 'R'
                clickPos = (startPos + click) % 100;
            }
            
            if (clickPos === 0) {
                countPart2++;
            }
        }
        
        // Update position after rotation
        if (direction === 'L') {
            position = ((position - distance) % 100 + 100) % 100;
        } else {
            position = (position + distance) % 100;
        }
    }
    
    return [countPart1.toString(), countPart2.toString()];
}

const data = readInputRaw("../data/input.txt");
const [part1, part2] = solve(data);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
