import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    static class Problem {
        int startCol;
        int endCol;
        char op;
        
        Problem(int startCol, int endCol, char op) {
            this.startCol = startCol;
            this.endCol = endCol;
            this.op = op;
        }
    }
    
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
        
        // Find maximum line length and pad all lines
        int maxLen = Arrays.stream(lines).mapToInt(String::length).max().orElse(0);
        String[] paddedLines = Arrays.stream(lines)
            .map(line -> String.format("%-" + maxLen + "s", line))
            .toArray(String[]::new);
        
        // Operator row is the last row
        int opRowIdx = paddedLines.length - 1;
        String opRow = paddedLines[opRowIdx];
        String[] numRows = Arrays.copyOf(paddedLines, opRowIdx);
        
        // Part 1: Parse horizontally
        long part1Total = 0;
        
        // Find problem boundaries (columns that are all spaces)
        boolean[] isSpaceCol = new boolean[maxLen];
        for (int col = 0; col < maxLen; col++) {
            final int c = col;
            isSpaceCol[col] = Arrays.stream(paddedLines)
                .allMatch(line -> c >= line.length() || line.charAt(c) == ' ');
        }
        
        // Group columns into problems
        List<Problem> problems = new ArrayList<>();
        int i = 0;
        while (i < maxLen) {
            if (!isSpaceCol[i]) {
                int startCol = i;
                while (i < maxLen && !isSpaceCol[i]) {
                    i++;
                }
                int endCol = i;
                // Extract operator for this problem
                char op = 0;
                for (int j = startCol; j < endCol; j++) {
                    if (j < opRow.length() && (opRow.charAt(j) == '+' || opRow.charAt(j) == '*')) {
                        op = opRow.charAt(j);
                        break;
                    }
                }
                if (op != 0) {
                    problems.add(new Problem(startCol, endCol, op));
                }
            } else {
                i++;
            }
        }
        
        // Solve Part 1: Extract numbers horizontally
        for (Problem prob : problems) {
            List<Long> numbers = new ArrayList<>();
            for (String row : numRows) {
                if (prob.endCol > row.length()) continue;
                String problemStr = row.substring(prob.startCol, prob.endCol).trim();
                String[] parts = problemStr.split("\\s+");
                for (String part : parts) {
                    try {
                        numbers.add(Long.parseLong(part));
                    } catch (NumberFormatException e) {
                        // Skip invalid numbers
                    }
                }
            }
            
            // Apply operator
            if (!numbers.isEmpty()) {
                long result;
                if (prob.op == '+') {
                    result = numbers.stream().mapToLong(Long::longValue).sum();
                } else {
                    result = numbers.stream().reduce(1L, (a, b) -> a * b);
                }
                part1Total += result;
            }
        }
        
        // Part 2: Parse vertically (columns, right-to-left)
        // Approach: Use Java streams to transpose and extract numbers
        long part2Total = 0;
        
        for (Problem prob : problems) {
            // Extract column strings (transpose)
            List<String> colStrings = new ArrayList<>();
            for (int col = prob.startCol; col < prob.endCol; col++) {
                if (col < isSpaceCol.length && isSpaceCol[col]) continue;
                
                StringBuilder colStr = new StringBuilder();
                for (String row : numRows) {
                    if (col < row.length()) {
                        char ch = row.charAt(col);
                        if (Character.isDigit(ch) || ch == ' ') {
                            colStr.append(ch);
                        }
                    }
                }
                String trimmed = colStr.toString().trim();
                if (!trimmed.isEmpty()) {
                    colStrings.add(trimmed);
                }
            }
            
            // Parse numbers from columns (reading right-to-left means reverse)
            // Reverse for right-to-left reading, then parse
            Collections.reverse(colStrings);
            List<Long> numbers = colStrings.stream()
                .map(s -> s.replaceAll(" ", ""))
                .filter(s -> !s.isEmpty())
                .map(Long::parseLong)
                .collect(java.util.stream.Collectors.toList());
            
            // Apply operator
            if (!numbers.isEmpty()) {
                long result;
                if (prob.op == '+') {
                    result = numbers.stream().mapToLong(Long::longValue).sum();
                } else {
                    result = numbers.stream().reduce(1L, (a, b) -> a * b);
                }
                part2Total += result;
            }
        }
        
        return new long[]{part1Total, part2Total};
    }
}
