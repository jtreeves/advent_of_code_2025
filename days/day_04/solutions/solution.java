import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    private static int countNeighbors(List<String> grid, int i, int j, int rows, int cols) {
        int count = 0;
        for (int di = -1; di <= 1; di++) {
            for (int dj = -1; dj <= 1; dj++) {
                if (di == 0 && dj == 0) continue;
                int ni = i + di;
                int nj = j + dj;
                if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid.get(ni).charAt(nj) == '@') {
                    count++;
                }
            }
        }
        return count;
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String content = Files.readString(inputPath);
        List<String> lines = new ArrayList<>();
        for (String line : content.trim().split("\n")) {
            line = line.trim();
            if (!line.isEmpty()) {
                lines.add(line);
            }
        }
        
        int[] results = solve(lines);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
    
    private static int[] solve(List<String> lines) {
        int rows = lines.size();
        int cols = rows > 0 ? lines.get(0).length() : 0;
        
        // Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
        int part1Count = 0;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (lines.get(i).charAt(j) == '@') {
                    int neighbors = countNeighbors(lines, i, j, rows, cols);
                    if (neighbors < 4) {
                        part1Count++;
                    }
                }
            }
        }
        
        // Part 2: Iteratively remove accessible rolls until none can be removed
        List<StringBuilder> grid = new ArrayList<>();
        for (String line : lines) {
            grid.add(new StringBuilder(line));
        }
        
        int part2Count = 0;
        while (true) {
            List<int[]> toRemove = new ArrayList<>();
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    if (grid.get(i).charAt(j) == '@') {
                        // Convert grid to string list for neighbor counting
                        List<String> gridStr = new ArrayList<>();
                        for (StringBuilder sb : grid) {
                            gridStr.add(sb.toString());
                        }
                        int neighbors = countNeighbors(gridStr, i, j, rows, cols);
                        if (neighbors < 4) {
                            toRemove.add(new int[]{i, j});
                        }
                    }
                }
            }
            
            if (toRemove.isEmpty()) {
                break;
            }
            
            // Remove all marked positions
            for (int[] pos : toRemove) {
                grid.get(pos[0]).setCharAt(pos[1], '.');
            }
            part2Count += toRemove.size();
        }
        
        return new int[]{part1Count, part2Count};
    }
}
