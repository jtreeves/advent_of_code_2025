import * as fs from 'fs';

interface ParseResult {
    targetPattern: boolean[];
    buttons: number[][];
    joltages: number[];
}

function parseLine(line: string): ParseResult {
    const result: ParseResult = { targetPattern: [], buttons: [], joltages: [] };
    
    // Extract pattern: [.##.]
    const patternMatch = line.match(/\[([.#]+)\]/);
    if (!patternMatch) return result;
    const patternStr = patternMatch[1];
    result.targetPattern = patternStr.split('').map(c => c === '#');
    
    // Extract buttons: (1,3) (2) etc.
    const buttonMatches = line.matchAll(/\(([^)]*)\)/g);
    for (const match of buttonMatches) {
        const btnStr = match[1].trim();
        if (!btnStr) {
            result.buttons.push([]);
        } else {
            const lights = btnStr.split(',').map(x => parseInt(x.trim(), 10));
            result.buttons.push(lights);
        }
    }
    
    // Extract joltages: {3,5,4,7}
    const joltageMatch = line.match(/\{([^}]+)\}/);
    if (joltageMatch) {
        const joltageStr = joltageMatch[1];
        result.joltages = joltageStr.split(',').map(x => parseInt(x.trim(), 10));
    }
    
    return result;
}

function gaussianEliminationGF2(matrix: boolean[][], target: boolean[]): number | null {
    const numButtons = matrix.length;
    const numLights = target.length;
    
    // Create augmented matrix [A | b]
    const aug: boolean[][] = [];
    for (let i = 0; i < numLights; i++) {
        const row: boolean[] = [];
        for (let j = 0; j < numButtons; j++) {
            row.push(matrix[j] ? (matrix[j][i] || false) : false);
        }
        row.push(target[i]);
        aug.push(row);
    }
    
    // Gaussian elimination mod 2
    let pivotRow = 0;
    let pivotCol = 0;
    
    while (pivotRow < numLights && pivotCol < numButtons) {
        // Find pivot
        let pivotIdx: number | null = null;
        for (let i = pivotRow; i < numLights; i++) {
            if (aug[i][pivotCol]) {
                pivotIdx = i;
                break;
            }
        }
        
        if (pivotIdx === null) {
            pivotCol++;
            continue;
        }
        
        // Swap rows
        if (pivotIdx !== pivotRow) {
            [aug[pivotRow], aug[pivotIdx]] = [aug[pivotIdx], aug[pivotRow]];
        }
        
        // Eliminate
        for (let i = pivotRow + 1; i < numLights; i++) {
            if (aug[i][pivotCol]) {
                for (let j = 0; j < numButtons + 1; j++) {
                    aug[i][j] = aug[i][j] !== aug[pivotRow][j];
                }
            }
        }
        
        pivotRow++;
        pivotCol++;
    }
    
    // Check for inconsistency (0 = 1)
    for (let i = pivotRow; i < numLights; i++) {
        if (aug[i][numButtons] && !aug[i].slice(0, numButtons).some(x => x)) {
            return null; // No solution
        }
    }
    
    // Back substitution to find solution (set free variables to 0 to minimize)
    const solution: boolean[] = new Array(numButtons).fill(false);
    const usedRows = new Set<number>();
    
    // Process from bottom up
    for (let i = numLights - 1; i >= 0; i--) {
        // Find first non-zero column
        let pivotColIdx: number | null = null;
        for (let j = 0; j < numButtons; j++) {
            if (aug[i][j] && !usedRows.has(j)) {
                pivotColIdx = j;
                usedRows.add(j);
                break;
            }
        }
        
        if (pivotColIdx !== null) {
            // Calculate value
            let val = aug[i][numButtons];
            for (let j = pivotColIdx + 1; j < numButtons; j++) {
                if (aug[i][j] && solution[j]) {
                    val = !val;
                }
            }
            solution[pivotColIdx] = val;
        }
    }
    
    // Count number of presses
    return solution.filter(x => x).length;
}

function solvePart2ILP(buttons: number[][], joltages: number[], targetPattern: boolean[]): number | null {
    const numButtons = buttons.length;
    const numLights = joltages.length;
    
    // Base case: all joltages are 0
    if (joltages.every(j => j === 0)) {
        return 0;
    }
    
    // Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
    // that achieve the required parity, then divide by 2 and recurse
    const buttonMatrix: boolean[][] = Array(numButtons).fill(null).map(() => Array(numLights).fill(false));
    for (let i = 0; i < numButtons; i++) {
        for (const light of buttons[i]) {
            if (light >= 0 && light < numLights) {
                buttonMatrix[i][light] = true;
            }
        }
    }
    
    // Required parity: pattern ON = odd, pattern OFF = even
    const requiredParity = [...targetPattern];
    
    // Find minimum parity presses needed (Part 1 style)
    const parityToggles = [...requiredParity];
    const minParityPresses = gaussianEliminationGF2(buttonMatrix, parityToggles);
    
    if (minParityPresses !== null) {
        // Helper to generate combinations
        function* combinations(n: number, k: number): Generator<number[]> {
            if (k === 0) {
                yield [];
                return;
            }
            if (k > n) return;
            if (k === n) {
                yield Array.from({length: n}, (_, i) => i);
                return;
            }
            // Include first element
            for (const combo of combinations(n - 1, k - 1)) {
                yield [0, ...combo.map(x => x + 1)];
            }
            // Exclude first element
            for (const combo of combinations(n - 1, k)) {
                yield combo.map(x => x + 1);
            }
        }
        
        let bestResult: number | null = null;
        const maxParityToTry = numButtons;
        
        for (let k = minParityPresses; k <= maxParityToTry; k++) {
            // Pruning
            if (bestResult !== null && k > bestResult) {
                break;
            }
            
            for (const buttonCombo of combinations(numButtons, k)) {
                // Simulate pressing these buttons once
                const resultingJoltages = [...joltages];
                for (const btnIdx of buttonCombo) {
                    for (const light of buttons[btnIdx]) {
                        if (light >= 0 && light < numLights) {
                            resultingJoltages[light]++;
                        }
                    }
                }
                
                // Check if parity matches
                const resultingParity = resultingJoltages.map(j => j % 2 === 1);
                if (JSON.stringify(resultingParity) === JSON.stringify(requiredParity)) {
                    // Check if all remaining are even (can divide by 2)
                    if (resultingJoltages.every(j => j % 2 === 0)) {
                        // Divide by 2 and recurse
                        const halvedJoltages = resultingJoltages.map(j => Math.floor(j / 2));
                        const remainingResult = solvePart2ILP(buttons, halvedJoltages, targetPattern);
                        if (remainingResult !== null) {
                            const total = k + remainingResult * 2;
                            if (bestResult === null || total < bestResult) {
                                bestResult = total;
                            }
                        }
                    }
                }
            }
        }
        
        if (bestResult !== null) {
            return bestResult;
        }
    }
    
    // Fallback: divide-by-2 optimization
    if (joltages.every(j => j % 2 === 0) && joltages.some(j => j > 0)) {
        const halvedJoltages = joltages.map(j => Math.floor(j / 2));
        const result = solvePart2ILP(buttons, halvedJoltages, targetPattern);
        if (result !== null) {
            return result * 2;
        }
        return null;
    }
    
    // Final fallback: bounded DFS
    const maxJoltage = joltages.length > 0 ? Math.max(...joltages) : 0;
    
    function dfs(buttonIdx: number, currentJoltages: number[], pressesSoFar: number, best: number | null): number | null {
        if (buttonIdx >= numButtons) {
            if (currentJoltages.every((val, i) => val === joltages[i])) {
                return best === null ? pressesSoFar : Math.min(best, pressesSoFar);
            }
            return best;
        }
        
        if (best !== null && pressesSoFar >= best) {
            return best;
        }
        
        let bestResult = best;
        for (let presses = 0; presses <= maxJoltage; presses++) {
            if (best !== null && pressesSoFar + presses >= best) {
                break;
            }
            
            const newJoltages = [...currentJoltages];
            for (const light of buttons[buttonIdx]) {
                if (light >= 0 && light < numLights) {
                    newJoltages[light] += presses;
                }
            }
            
            if (newJoltages.some((val, i) => val > joltages[i])) {
                continue;
            }
            
            const result = dfs(buttonIdx + 1, newJoltages, pressesSoFar + presses, bestResult);
            if (result !== null) {
                bestResult = result;
            }
        }
        
        return bestResult;
    }
    
    const initialJoltages = Array(numLights).fill(0);
    return dfs(0, initialJoltages, 0, null);
}

function solve(inputData: string): [string, string] {
    const lines = inputData.trim().split('\n');
    if (lines.length === 0) {
        return ["0", "0"];
    }
    
    let part1Total = 0;
    let part2Total = 0;
    
    for (const line of lines) {
        if (!line.trim()) continue;
        
        const { targetPattern, buttons, joltages } = parseLine(line);
        if (targetPattern.length === 0) continue;
        
        const numLights = targetPattern.length;
        
        // Part 1: GF(2) linear system
        // Build incidence matrix: matrix[i][j] = true if button i toggles light j
        const buttonMatrix: boolean[][] = Array(buttons.length).fill(null).map(() => Array(numLights).fill(false));
        for (let i = 0; i < buttons.length; i++) {
            for (const light of buttons[i]) {
                if (light >= 0 && light < numLights) {
                    buttonMatrix[i][light] = true;
                }
            }
        }
        
        // Target: all start OFF, need to toggle to match pattern
        // Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        const requiredToggles = [...targetPattern];
        
        const result = gaussianEliminationGF2(buttonMatrix, requiredToggles);
        if (result !== null) {
            part1Total += result;
        }
        
        // Part 2: Integer Linear Programming
        if (joltages.length === numLights) {
            const result2 = solvePart2ILP(buttons, joltages, targetPattern);
            if (result2 !== null) {
                part2Total += result2;
            }
        }
    }
    
    return [part1Total.toString(), part2Total.toString()];
}

const content = fs.readFileSync('../data/input.txt', 'utf-8');
const [part1, part2] = solve(content);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
