import java.io.IOException;
import java.util.List;

class Solution {
    public static void main(String[] args) {
        try {
            List<String> lines = GetInput.getInput(1);
            int[] results = solve(lines);
            System.out.println("Part 1: " + results[0]);
            System.out.println("Part 2: " + results[1]);
        } catch (IOException e) {
            System.err.println("Error reading input: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static int[] solve(List<String> lines) {
        // Part 1: Count times dial ends at 0 after a rotation
        int position = 50;
        int countPart1 = 0;
        
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            
            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));
            
            // Apply rotation
            if (direction == 'L') {
                position = ((position - distance) % 100 + 100) % 100;
            } else { // direction == 'R'
                position = (position + distance) % 100;
            }
            
            // Check if ended at 0
            if (position == 0) {
                countPart1++;
            }
        }
        
        // Part 2: Count times dial is at 0 during entire process
        position = 50;
        int countPart2 = 0;
        
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            
            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));
            
            int startPos = position;
            
            // Check each click position during rotation
            for (int click = 1; click <= distance; click++) {
                int clickPos;
                if (direction == 'L') {
                    clickPos = ((startPos - click) % 100 + 100) % 100;
                } else { // direction == 'R'
                    clickPos = (startPos + click) % 100;
                }
                
                if (clickPos == 0) {
                    countPart2++;
                }
            }
            
            // Update position after rotation
            if (direction == 'L') {
                position = ((position - distance) % 100 + 100) % 100;
            } else {
                position = (position + distance) % 100;
            }
        }
        
        return new int[]{countPart1, countPart2};
    }
}
