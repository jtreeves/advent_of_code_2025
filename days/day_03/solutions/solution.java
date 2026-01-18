import java.io.*;
import java.nio.file.*;

class Solution {
    // Find the largest N-digit number by selecting N digits in order from bank
    private static long findLargestSubsequence(String bank, int n) {
        int bankLen = bank.length();
        if (bankLen < n) {
            return 0;
        }
        
        StringBuilder result = new StringBuilder();
        int start = 0;
        
        for (int i = 0; i < n; i++) {
            int remainingNeeded = n - i - 1;
            int end = bankLen - remainingNeeded;
            
            char maxDigit = bank.charAt(start);
            int maxPos = start;
            for (int j = start + 1; j < end; j++) {
                if (bank.charAt(j) > maxDigit) {
                    maxDigit = bank.charAt(j);
                    maxPos = j;
                }
            }
            
            result.append(maxDigit);
            start = maxPos + 1;
        }
        
        return Long.parseLong(result.toString());
    }
    
    public static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        
        long part1Sum = 0;
        long part2Sum = 0;
        
        for (String line : lines) {
            String bank = line.trim();
            if (bank.isEmpty()) {
                continue;
            }
            
            part1Sum += findLargestSubsequence(bank, 2);
            
            if (bank.length() >= 12) {
                part2Sum += findLargestSubsequence(bank, 12);
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
