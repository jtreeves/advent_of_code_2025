import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String content = new String(Files.readAllBytes(inputPath));
        long[] results = solve(content);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
    
    private static long[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        if (lines.length == 0) {
            return new long[]{0, 0};
        }
        
        char[][] grid = new char[lines.length][];
        for (int i = 0; i < lines.length; i++) {
            grid[i] = lines[i].toCharArray();
        }
        int rows = grid.length;
        int cols = rows > 0 ? grid[0].length : 0;
        
        // Find starting position S
        int startRow = -1, startCol = -1;
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 'S') {
                    startRow = r;
                    startCol = c;
                    break;
                }
            }
            if (startRow != -1) break;
        }
        
        if (startRow == -1) {
            return new long[]{0, 0};
        }
        
        // Part 1: Count total splits
        int splitCount = 0;
        Set<Integer> activeBeams = new HashSet<>();
        activeBeams.add(startCol);
        
        // Process each row starting from the row after S
        for (int r = startRow + 1; r < rows; r++) {
            Set<Integer> nextBeams = new HashSet<>();
            for (int col : activeBeams) {
                if (grid[r][col] == '.') {
                    // Beam continues down
                    nextBeams.add(col);
                } else if (grid[r][col] == '^') {
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
        long[][] beamCounts = new long[rows][cols];
        beamCounts[startRow][startCol] = 1; // Start with 1 beam at S
        
        // Process each row starting from the row after S
        for (int r = startRow + 1; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                long prevCount = beamCounts[r - 1][c];
                if (prevCount > 0) {
                    if (grid[r][c] == '.') {
                        // Beam continues down
                        beamCounts[r][c] += prevCount;
                    } else if (grid[r][c] == '^') {
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
        long bottomBeamCount = 0;
        for (int c = 0; c < cols; c++) {
            bottomBeamCount += beamCounts[rows - 1][c];
        }
        
        return new long[]{splitCount, bottomBeamCount};
    }
}
