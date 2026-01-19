import java.io.*;
import java.nio.file.*;

class Solution {
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String content = new String(Files.readAllBytes(inputPath));
        String[] results = solve(content);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
    
    private static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        if (lines.length == 0) {
            return new String[]{"0", "Final star"};
        }
        
        // Parse shapes (first 6 shapes, numbered 0-5)
        int[] shapeAreas = new int[6];
        int i = 0;
        int shapeIdx = 0;
        
        while (i < lines.length && shapeIdx < 6) {
            String line = lines[i].trim();
            // Check if this is a shape header (format: "number:")
            if (line.length() > 0 && line.endsWith(":")) {
                try {
                    int shapeNum = Integer.parseInt(line.substring(0, line.length() - 1));
                    if (shapeNum == shapeIdx) {
                        // Read the next 3 lines for the shape grid
                        String[] shapeGrid = new String[3];
                        for (int j = 0; j < 3; j++) {
                            if (i + 1 + j < lines.length) {
                                shapeGrid[j] = lines[i + 1 + j].trim();
                            } else {
                                shapeGrid[j] = "";
                            }
                        }
                        
                        // Count '#' characters in the shape
                        int area = 0;
                        for (String row : shapeGrid) {
                            for (char c : row.toCharArray()) {
                                if (c == '#') {
                                    area++;
                                }
                            }
                        }
                        shapeAreas[shapeIdx] = area;
                        shapeIdx++;
                        i += 4; // Skip shape header + 3 grid lines + empty line (if present)
                        continue;
                    }
                } catch (NumberFormatException e) {
                    // Not a valid shape number
                }
            }
            i++;
        }
        
        // Find where queries start (skip empty lines after shapes)
        int queryStart = i;
        while (queryStart < lines.length && lines[queryStart].trim().isEmpty()) {
            queryStart++;
        }
        
        // Parse queries
        int possibleCount = 0;
        for (int lineIdx = queryStart; lineIdx < lines.length; lineIdx++) {
            String line = lines[lineIdx].trim();
            if (line.isEmpty()) {
                continue;
            }
            
            // Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
            if (!line.contains(":")) {
                continue;
            }
            
            String[] parts = line.split(":", 2);
            if (parts.length != 2) {
                continue;
            }
            
            // Parse dimensions
            String dims = parts[0].trim();
            if (!dims.contains("x")) {
                continue;
            }
            
            String[] dimParts = dims.split("x");
            if (dimParts.length != 2) {
                continue;
            }
            
            int width, height;
            try {
                width = Integer.parseInt(dimParts[0]);
                height = Integer.parseInt(dimParts[1]);
            } catch (NumberFormatException e) {
                continue;
            }
            
            // Parse counts
            String[] countParts = parts[1].trim().split("\\s+");
            if (countParts.length != 6) {
                continue;
            }
            
            int[] counts = new int[6];
            boolean validCounts = true;
            for (int j = 0; j < 6; j++) {
                try {
                    counts[j] = Integer.parseInt(countParts[j]);
                } catch (NumberFormatException e) {
                    validCounts = false;
                    break;
                }
            }
            
            if (!validCounts) {
                continue;
            }
            
            // Calculate area check
            long regionArea = (long)width * (long)height;
            long requiredArea = 0;
            for (int j = 0; j < 6; j++) {
                requiredArea += (long)shapeAreas[j] * (long)counts[j];
            }
            
            if (requiredArea <= regionArea) {
                possibleCount++;
            }
        }
        
        // Part 2: Final star (no computation needed)
        String part2 = "Final star";
        
        return new String[]{String.valueOf(possibleCount), part2};
    }
}
