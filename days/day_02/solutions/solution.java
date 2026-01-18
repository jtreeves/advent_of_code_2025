import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    // Check if ID is invalid for Part 1: exactly two identical sequences
    private static boolean isInvalidPart1(String idStr) {
        int n = idStr.length();
        if (n % 2 != 0) {
            return false;
        }
        int half = n / 2;
        return idStr.substring(0, half).equals(idStr.substring(half));
    }
    
    // Check if ID is invalid for Part 2: sequence repeated 2+ times
    private static boolean isInvalidPart2(String idStr) {
        int n = idStr.length();
        for (int k = 2; k <= n; k++) {
            if (n % k == 0) {
                int seqLen = n / k;
                String pattern = idStr.substring(0, seqLen);
                String repeated = pattern.repeat(k);
                if (idStr.equals(repeated)) {
                    return true;
                }
            }
        }
        return false;
    }
    
    // Parse a range string like "start-end"
    private static long[] parseRange(String rangeStr) {
        String[] parts = rangeStr.split("-");
        long start = Long.parseLong(parts[0].trim());
        long end = Long.parseLong(parts[1].trim());
        return new long[]{start, end};
    }
    
    // Parse a line of comma-separated ranges
    private static List<long[]> parseRanges(String line) {
        List<long[]> ranges = new ArrayList<>();
        String[] parts = line.split(",");
        for (String part : parts) {
            part = part.trim();
            if (!part.isEmpty()) {
                ranges.add(parseRange(part));
            }
        }
        return ranges;
    }
    
    public static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        
        long part1Sum = 0;
        long part2Sum = 0;
        
        for (String line : lines) {
            if (line.trim().isEmpty()) {
                continue;
            }
            List<long[]> ranges = parseRanges(line);
            
            for (long[] range : ranges) {
                long start = range[0];
                long end = range[1];
                for (long num = start; num <= end; num++) {
                    String idStr = String.valueOf(num);
                    
                    if (isInvalidPart1(idStr)) {
                        part1Sum += num;
                    }
                    
                    if (isInvalidPart2(idStr)) {
                        part2Sum += num;
                    }
                }
            }
        }
        
        return new String[]{String.valueOf(part1Sum), String.valueOf(part2Sum)};
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String data = Files.readString(inputPath);
        String[] results = solve(data);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
}
