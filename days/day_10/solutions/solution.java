import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

class Solution {
    static class ParseResult {
        List<Boolean> targetPattern = new ArrayList<>();
        List<List<Integer>> buttons = new ArrayList<>();
        List<Integer> joltages = new ArrayList<>();
    }
    
    static ParseResult parseLine(String line) {
        ParseResult result = new ParseResult();
        
        // Extract pattern: [.##.]
        Pattern patternRe = Pattern.compile("\\[([.#]+)\\]");
        Matcher patternMatch = patternRe.matcher(line);
        if (patternMatch.find()) {
            String patternStr = patternMatch.group(1);
            for (char c : patternStr.toCharArray()) {
                result.targetPattern.add(c == '#');
            }
        }
        
        // Extract buttons: (1,3) (2) etc.
        Pattern buttonRe = Pattern.compile("\\(([^)]*)\\)");
        Matcher buttonMatch = buttonRe.matcher(line);
        while (buttonMatch.find()) {
            String btnStr = buttonMatch.group(1).trim();
            if (btnStr.isEmpty()) {
                result.buttons.add(new ArrayList<>());
            } else {
                List<Integer> lights = new ArrayList<>();
                String[] parts = btnStr.split(",");
                for (String part : parts) {
                    try {
                        lights.add(Integer.parseInt(part.trim()));
                    } catch (NumberFormatException e) {
                        // Skip invalid numbers
                    }
                }
                result.buttons.add(lights);
            }
        }
        
        // Extract joltages: {3,5,4,7}
        Pattern joltageRe = Pattern.compile("\\{([^}]+)\\}");
        Matcher joltageMatch = joltageRe.matcher(line);
        if (joltageMatch.find()) {
            String joltageStr = joltageMatch.group(1);
            String[] parts = joltageStr.split(",");
            for (String part : parts) {
                try {
                    result.joltages.add(Integer.parseInt(part.trim()));
                } catch (NumberFormatException e) {
                    // Skip invalid numbers
                }
            }
        }
        
        return result;
    }
    
    static Integer gaussianEliminationGF2(List<List<Boolean>> matrix, List<Boolean> target) {
        int numButtons = matrix.size();
        int numLights = target.size();
        
        // Create augmented matrix [A | b]
        List<List<Boolean>> aug = new ArrayList<>();
        for (int i = 0; i < numLights; i++) {
            List<Boolean> row = new ArrayList<>();
            for (int j = 0; j < numButtons; j++) {
                if (j < matrix.size() && i < matrix.get(j).size()) {
                    row.add(matrix.get(j).get(i));
                } else {
                    row.add(false);
                }
            }
            row.add(target.get(i));
            aug.add(row);
        }
        
        // Gaussian elimination mod 2
        int pivotRow = 0;
        int pivotCol = 0;
        
        while (pivotRow < numLights && pivotCol < numButtons) {
            // Find pivot
            Integer pivotIdx = null;
            for (int i = pivotRow; i < numLights; i++) {
                if (aug.get(i).get(pivotCol)) {
                    pivotIdx = i;
                    break;
                }
            }
            
            if (pivotIdx == null) {
                pivotCol++;
                continue;
            }
            
            // Swap rows
            if (pivotIdx != pivotRow) {
                Collections.swap(aug, pivotRow, pivotIdx);
            }
            
            // Eliminate
            for (int i = pivotRow + 1; i < numLights; i++) {
                if (aug.get(i).get(pivotCol)) {
                    for (int j = 0; j <= numButtons; j++) {
                        boolean val = aug.get(i).get(j) ^ aug.get(pivotRow).get(j);
                        aug.get(i).set(j, val);
                    }
                }
            }
            
            pivotRow++;
            pivotCol++;
        }
        
        // Check for inconsistency (0 = 1)
        for (int i = pivotRow; i < numLights; i++) {
            if (aug.get(i).get(numButtons)) {
                boolean hasNonZero = false;
                for (int j = 0; j < numButtons; j++) {
                    if (aug.get(i).get(j)) {
                        hasNonZero = true;
                        break;
                    }
                }
                if (!hasNonZero) {
                    return null; // No solution
                }
            }
        }
        
        // Back substitution to find solution (set free variables to 0 to minimize)
        List<Boolean> solution = new ArrayList<>(Collections.nCopies(numButtons, false));
        Set<Integer> usedRows = new HashSet<>();
        
        // Process from bottom up
        for (int i = numLights - 1; i >= 0; i--) {
            // Find first non-zero column
            Integer pivotColIdx = null;
            for (int j = 0; j < numButtons; j++) {
                if (aug.get(i).get(j) && !usedRows.contains(j)) {
                    pivotColIdx = j;
                    usedRows.add(j);
                    break;
                }
            }
            
            if (pivotColIdx != null) {
                // Calculate value
                boolean val = aug.get(i).get(numButtons);
                for (int j = pivotColIdx + 1; j < numButtons; j++) {
                    if (aug.get(i).get(j) && solution.get(j)) {
                        val = !val;
                    }
                }
                solution.set(pivotColIdx, val);
            }
        }
        
        // Count number of presses
        int count = 0;
        for (boolean x : solution) {
            if (x) count++;
        }
        return count;
    }
    
    static Integer solvePart2ILP(List<List<Integer>> buttons, List<Integer> joltages) {
        int numButtons = buttons.size();
        int numLights = joltages.size();
        
        // Bounded search: max presses per button is max joltage
        int maxJoltage = joltages.stream().mapToInt(i -> i).max().orElse(0);
        
        return dfs(0, new ArrayList<>(Collections.nCopies(numLights, 0)), 0, null, buttons, joltages);
    }
    
    static Integer dfs(int buttonIdx, List<Integer> currentJoltages, int pressesSoFar, Integer best,
                       List<List<Integer>> buttons, List<Integer> joltages) {
        if (buttonIdx >= buttons.size()) {
            boolean allMatch = true;
            for (int i = 0; i < currentJoltages.size(); i++) {
                if (currentJoltages.get(i) != joltages.get(i)) {
                    allMatch = false;
                    break;
                }
            }
            if (allMatch) {
                return best == null ? pressesSoFar : Math.min(best, pressesSoFar);
            }
            return best;
        }
        
        // Pruning: if current presses already exceed best, skip
        if (best != null && pressesSoFar >= best) {
            return best;
        }
        
        // Calculate minimum additional presses needed (lower bound)
        int sumRemaining = 0;
        for (int i = 0; i < joltages.size(); i++) {
            int need = joltages.get(i) - currentJoltages.get(i);
            if (need > 0) sumRemaining += need;
        }
        if (sumRemaining > 0) {
            int maxLightsPerButton = 1;
            for (int i = buttonIdx; i < buttons.size(); i++) {
                if (buttons.get(i).size() > maxLightsPerButton) {
                    maxLightsPerButton = buttons.get(i).size();
                }
            }
            if (maxLightsPerButton > 0) {
                int minAdditional = (sumRemaining + maxLightsPerButton - 1) / maxLightsPerButton;
                if (best != null && pressesSoFar + minAdditional >= best) {
                    return best;
                }
            }
        }
        
        // Try 0 to maxJoltage presses of current button
        Integer bestResult = best;
        int maxJoltage = joltages.stream().mapToInt(i -> i).max().orElse(0);
        for (int presses = 0; presses <= maxJoltage; presses++) {
            if (best != null && pressesSoFar + presses >= best) {
                break;
            }
            
            List<Integer> newJoltages = new ArrayList<>(currentJoltages);
            for (int light : buttons.get(buttonIdx)) {
                if (light >= 0 && light < joltages.size()) {
                    newJoltages.set(light, newJoltages.get(light) + presses);
                }
            }
            
            // Check if any exceeds target (pruning)
            boolean exceeds = false;
            for (int i = 0; i < newJoltages.size(); i++) {
                if (newJoltages.get(i) > joltages.get(i)) {
                    exceeds = true;
                    break;
                }
            }
            if (exceeds) continue;
            
            Integer result = dfs(buttonIdx + 1, newJoltages, pressesSoFar + presses, bestResult, buttons, joltages);
            if (result != null) {
                bestResult = bestResult == null ? result : Math.min(bestResult, result);
            }
        }
        
        return bestResult;
    }
    
    public static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        if (lines.length == 0) {
            return new String[]{"0", "0"};
        }
        
        int part1Total = 0;
        int part2Total = 0;
        
        for (String line : lines) {
            if (line.trim().isEmpty()) continue;
            
            ParseResult parsed = parseLine(line);
            if (parsed.targetPattern.isEmpty()) continue;
            
            int numLights = parsed.targetPattern.size();
            
            // Part 1: GF(2) linear system
            // Build incidence matrix: matrix[i][j] = true if button i toggles light j
            List<List<Boolean>> buttonMatrix = new ArrayList<>();
            for (int i = 0; i < parsed.buttons.size(); i++) {
                List<Boolean> row = new ArrayList<>(Collections.nCopies(numLights, false));
                buttonMatrix.add(row);
            }
            for (int i = 0; i < parsed.buttons.size(); i++) {
                for (int light : parsed.buttons.get(i)) {
                    if (light >= 0 && light < numLights) {
                        buttonMatrix.get(i).set(light, true);
                    }
                }
            }
            
            // Target: all start OFF, need to toggle to match pattern
            List<Boolean> requiredToggles = new ArrayList<>(parsed.targetPattern);
            
            Integer result = gaussianEliminationGF2(buttonMatrix, requiredToggles);
            if (result != null) {
                part1Total += result;
            }
            
            // Part 2: Integer Linear Programming
            if (parsed.joltages.size() == numLights) {
                Integer result2 = solvePart2ILP(parsed.buttons, parsed.joltages);
                if (result2 != null) {
                    part2Total += result2;
                }
            }
        }
        
        return new String[]{String.valueOf(part1Total), String.valueOf(part2Total)};
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String data = Files.readString(inputPath);
        String[] results = solve(data);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
}
