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
        
        // Find blank line separator
        int blankIdx = -1;
        for (int i = 0; i < lines.length; i++) {
            if (lines[i].trim().isEmpty()) {
                blankIdx = i;
                break;
            }
        }
        
        // Parse ranges (first section)
        List<long[]> ranges = new ArrayList<>();
        for (int i = 0; i < blankIdx; i++) {
            String line = lines[i].trim();
            if (line.isEmpty()) continue;
            String[] parts = line.split("-");
            long start = Long.parseLong(parts[0]);
            long end = Long.parseLong(parts[1]);
            ranges.add(new long[]{start, end});
        }
        
        // Parse IDs to check (second section)
        List<Long> ids = new ArrayList<>();
        for (int i = blankIdx + 1; i < lines.length; i++) {
            String line = lines[i].trim();
            if (line.isEmpty()) continue;
            ids.add(Long.parseLong(line));
        }
        
        // Part 1: Count how many IDs fall into any range
        int part1Count = 0;
        for (long idVal : ids) {
            for (long[] range : ranges) {
                if (range[0] <= idVal && idVal <= range[1]) {
                    part1Count++;
                    break;
                }
            }
        }
        
        // Part 2: Merge ranges and count total unique IDs covered
        // Sort ranges by start value
        ranges.sort((a, b) -> Long.compare(a[0], b[0]));
        
        // Merge overlapping/adjacent ranges
        List<long[]> merged = new ArrayList<>();
        if (!ranges.isEmpty()) {
            merged.add(new long[]{ranges.get(0)[0], ranges.get(0)[1]});
            for (int i = 1; i < ranges.size(); i++) {
                long[] current = ranges.get(i);
                long[] last = merged.get(merged.size() - 1);
                // Check if overlaps or is adjacent (current[0] <= last[1] + 1)
                if (current[0] <= last[1] + 1) {
                    // Merge: update end to max of both ends
                    last[1] = Math.max(last[1], current[1]);
                } else {
                    // No overlap, add as new range
                    merged.add(new long[]{current[0], current[1]});
                }
            }
        }
        
        // Calculate total unique IDs covered
        long part2Total = 0;
        for (long[] range : merged) {
            part2Total += range[1] - range[0] + 1;
        }
        
        return new long[]{part1Count, part2Total};
    }
}
