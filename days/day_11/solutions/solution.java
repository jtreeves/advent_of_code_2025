import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    // Helper class for memoization key
    static class MemoKey {
        String node;
        boolean visitedFFT;
        boolean visitedDAC;
        
        MemoKey(String node, boolean visitedFFT, boolean visitedDAC) {
            this.node = node;
            this.visitedFFT = visitedFFT;
            this.visitedDAC = visitedDAC;
        }
        
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            MemoKey memoKey = (MemoKey) o;
            return visitedFFT == memoKey.visitedFFT &&
                   visitedDAC == memoKey.visitedDAC &&
                   Objects.equals(node, memoKey.node);
        }
        
        @Override
        public int hashCode() {
            return Objects.hash(node, visitedFFT, visitedDAC);
        }
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String content = new String(Files.readAllBytes(inputPath));
        String[] results = solve(content);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
    
    private static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        
        // Build graph: device -> list of outputs
        Map<String, List<String>> graph = new HashMap<>();
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) {
                continue;
            }
            String[] parts = line.split(":");
            if (parts.length != 2) {
                continue;
            }
            String device = parts[0].trim();
            String outputsStr = parts[1].trim();
            List<String> outputs = new ArrayList<>();
            if (!outputsStr.isEmpty()) {
                outputs = Arrays.asList(outputsStr.split("\\s+"));
            }
            graph.put(device, outputs);
        }
        
        // Part 1: Count paths from "you" to "out"
        Map<String, Long> part1Memo = new HashMap<>();
        long part1Count = 0;
        if (graph.containsKey("you")) {
            part1Count = countPathsPart1("you", graph, part1Memo);
        }
        
        // Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
        
        Map<MemoKey, Long> part2Memo = new HashMap<>();
        long part2Count = 0;
        if (graph.containsKey("svr")) {
            part2Count = countPathsPart2("svr", false, false, graph, part2Memo);
        }
        
        return new String[]{String.valueOf(part1Count), String.valueOf(part2Count)};
    }
    
    private static long countPathsPart1(String node, Map<String, List<String>> graph, Map<String, Long> memo) {
        if ("out".equals(node)) {
            return 1;
        }
        if (memo.containsKey(node)) {
            return memo.get(node);
        }
        
        long count = 0;
        List<String> neighbors = graph.getOrDefault(node, Collections.emptyList());
        for (String neighbor : neighbors) {
            count += countPathsPart1(neighbor, graph, memo);
        }
        
        memo.put(node, count);
        return count;
    }
    
    private static long countPathsPart2(String node, boolean visitedFFT, boolean visitedDAC,
                                        Map<String, List<String>> graph, Map<MemoKey, Long> memo) {
        if ("out".equals(node)) {
            return (visitedFFT && visitedDAC) ? 1 : 0;
        }
        
        MemoKey key = new MemoKey(node, visitedFFT, visitedDAC);
        if (memo.containsKey(key)) {
            return memo.get(key);
        }
        
        // Update flags when visiting fft or dac
        boolean newVisitedFFT = visitedFFT || "fft".equals(node);
        boolean newVisitedDAC = visitedDAC || "dac".equals(node);
        
        long count = 0;
        List<String> neighbors = graph.getOrDefault(node, Collections.emptyList());
        for (String neighbor : neighbors) {
            count += countPathsPart2(neighbor, newVisitedFFT, newVisitedDAC, graph, memo);
        }
        
        memo.put(key, count);
        return count;
    }
}
