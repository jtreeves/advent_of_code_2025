import java.io.*;
import java.nio.file.*;
import java.util.*;

class Point {
    long x, y;
    Point(long x, long y) {
        this.x = x;
        this.y = y;
    }
}

class Candidate {
    long minX, maxX, minY, maxY, area;
    int cx1, cx2, cy1, cy2;
    Candidate(long minX, long maxX, long minY, long maxY, long area, int cx1, int cx2, int cy1, int cy2) {
        this.minX = minX; this.maxX = maxX; this.minY = minY; this.maxY = maxY;
        this.area = area; this.cx1 = cx1; this.cx2 = cx2; this.cy1 = cy1; this.cy2 = cy2;
    }
}

class Solution {
    public static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        List<Point> redTiles = new ArrayList<>();
        
        // Parse coordinates
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            if (line.contains(",")) {
                String[] parts = line.split(",");
                if (parts.length == 2) {
                    try {
                        long x = Long.parseLong(parts[0].trim());
                        long y = Long.parseLong(parts[1].trim());
                        redTiles.add(new Point(x, y));
                    } catch (NumberFormatException e) {
                        // Skip invalid lines
                    }
                }
            }
        }
        
        if (redTiles.size() < 2) {
            return new String[]{"0", "0"};
        }
        
        // Part 1: Find largest rectangle area using any two red tiles as corners
        long maxAreaPart1 = 0;
        for (int i = 0; i < redTiles.size(); i++) {
            for (int j = i + 1; j < redTiles.size(); j++) {
                long x1 = redTiles.get(i).x;
                long y1 = redTiles.get(i).y;
                long x2 = redTiles.get(j).x;
                long y2 = redTiles.get(j).y;
                long width = Math.abs(x1 - x2) + 1;
                long height = Math.abs(y1 - y2) + 1;
                long area = width * height;
                maxAreaPart1 = Math.max(maxAreaPart1, area);
            }
        }
        
        // Part 2: Coordinate compression + flood-fill + prefix sums
        Set<Long> allXSet = new HashSet<>();
        Set<Long> allYSet = new HashSet<>();
        for (Point pt : redTiles) {
            allXSet.add(pt.x);
            allXSet.add(pt.x + 1);
            allYSet.add(pt.y);
            allYSet.add(pt.y + 1);
        }
        
        List<Long> allX = new ArrayList<>(allXSet);
        List<Long> allY = new ArrayList<>(allYSet);
        Collections.sort(allX);
        Collections.sort(allY);
        
        Map<Long, Integer> xToCx = new HashMap<>();
        Map<Long, Integer> yToCy = new HashMap<>();
        for (int i = 0; i < allX.size(); i++) {
            xToCx.put(allX.get(i), i);
        }
        for (int i = 0; i < allY.size(); i++) {
            yToCy.put(allY.get(i), i);
        }
        
        int width = allX.size();
        int height = allY.size();
        
        // Build grid
        boolean[][] grid = new boolean[width][height];
        
        // Mark boundary
        for (Point pt : redTiles) {
            Integer cx = xToCx.get(pt.x);
            Integer cy = yToCy.get(pt.y);
            if (cx != null && cy != null) {
                grid[cx][cy] = true;
            }
        }
        
        // Connect consecutive red tiles
        for (int i = 0; i < redTiles.size(); i++) {
            Point p1 = redTiles.get(i);
            Point p2 = redTiles.get((i + 1) % redTiles.size());
            
            if (p1.x == p2.x) {
                long startY = Math.min(p1.y, p2.y);
                long endY = Math.max(p1.y, p2.y);
                for (long y = startY; y <= endY; y++) {
                    Integer cx = xToCx.get(p1.x);
                    Integer cy = yToCy.get(y);
                    if (cx != null && cy != null) {
                        grid[cx][cy] = true;
                    }
                }
            } else if (p1.y == p2.y) {
                long startX = Math.min(p1.x, p2.x);
                long endX = Math.max(p1.x, p2.x);
                for (long x = startX; x <= endX; x++) {
                    Integer cx = xToCx.get(x);
                    Integer cy = yToCy.get(p1.y);
                    if (cx != null && cy != null) {
                        grid[cx][cy] = true;
                    }
                }
            }
        }
        
        // Point-in-polygon
        java.util.function.BiFunction<Long, Long, Boolean> pointInPolygon = (px, py) -> {
            boolean inside = false;
            for (int i = 0; i < redTiles.size(); i++) {
                Point p1 = redTiles.get(i);
                Point p2 = redTiles.get((i + 1) % redTiles.size());
                if ((p1.y > py) != (p2.y > py)) {
                    double intersectX = p2.y != p1.y
                        ? (py - p1.y) * (p2.x - p1.x) / (double)(p2.y - p1.y) + p1.x
                        : px;
                    if (px < intersectX) {
                        inside = !inside;
                    }
                }
            }
            return inside;
        };
        
        // Flood fill interior
        boolean foundInterior = false;
        for (int cx = 0; cx < width && !foundInterior; cx++) {
            for (int cy = 0; cy < height && !foundInterior; cy++) {
                if (!grid[cx][cy]) {
                    long origX = allX.get(cx);
                    long origY = allY.get(cy);
                    if (pointInPolygon.apply(origX, origY)) {
                        Stack<int[]> stack = new Stack<>();
                        stack.push(new int[]{cx, cy});
                        while (!stack.isEmpty()) {
                            int[] pos = stack.pop();
                            int x = pos[0], y = pos[1];
                            if (x >= width || y >= height || grid[x][y]) continue;
                            long origX2 = allX.get(x);
                            long origY2 = allY.get(y);
                            if (pointInPolygon.apply(origX2, origY2)) {
                                grid[x][y] = true;
                                if (x > 0) stack.push(new int[]{x - 1, y});
                                if (x + 1 < width) stack.push(new int[]{x + 1, y});
                                if (y > 0) stack.push(new int[]{x, y - 1});
                                if (y + 1 < height) stack.push(new int[]{x, y + 1});
                            }
                        }
                        foundInterior = true;
                    }
                }
            }
        }
        
        // Build 2D prefix sum
        long[][] prefix = new long[width + 1][height + 1];
        for (int cx = 0; cx < width; cx++) {
            for (int cy = 0; cy < height; cy++) {
                prefix[cx + 1][cy + 1] = prefix[cx][cy + 1] + prefix[cx + 1][cy] 
                    - prefix[cx][cy] + (grid[cx][cy] ? 1 : 0);
            }
        }
        
        java.util.function.Function<int[], Long> rectSum = (coords) -> {
            int cx1 = coords[0], cx2 = coords[1], cy1 = coords[2], cy2 = coords[3];
            return prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] 
                - prefix[cx2 + 1][cy1] + prefix[cx1][cy1];
        };
        
        // Generate candidates sorted by area descending
        List<Candidate> candidates = new ArrayList<>();
        for (int i = 0; i < redTiles.size(); i++) {
            for (int j = i + 1; j < redTiles.size(); j++) {
                Point p1 = redTiles.get(i);
                Point p2 = redTiles.get(j);
                long minX = Math.min(p1.x, p2.x);
                long maxX = Math.max(p1.x, p2.x);
                long minY = Math.min(p1.y, p2.y);
                long maxY = Math.max(p1.y, p2.y);
                long area = (maxX - minX + 1) * (maxY - minY + 1);
                
                Integer cx1 = xToCx.get(minX);
                Integer cx2 = xToCx.get(maxX);
                Integer cy1 = yToCy.get(minY);
                Integer cy2 = yToCy.get(maxY);
                if (cx1 != null && cx2 != null && cy1 != null && cy2 != null) {
                    candidates.add(new Candidate(minX, maxX, minY, maxY, area, cx1, cx2, cy1, cy2));
                }
            }
        }
        
        candidates.sort((a, b) -> Long.compare(b.area, a.area));
        
        // Check candidates
        long maxAreaPart2 = 0;
        for (Candidate cand : candidates) {
            if (cand.area <= maxAreaPart2) break;
            
            long validCount = rectSum.apply(new int[]{cand.cx1, cand.cx2, cand.cy1, cand.cy2});
            long expectedCells = (long)(cand.cx2 - cand.cx1 + 1) * (cand.cy2 - cand.cy1 + 1);
            
            if (validCount == expectedCells) {
                boolean allValid = true;
                long[][] corners = {{cand.minX, cand.minY}, {cand.minX, cand.maxY}, 
                                   {cand.maxX, cand.minY}, {cand.maxX, cand.maxY}};
                for (long[] corner : corners) {
                    long x = corner[0], y = corner[1];
                    Integer cx = xToCx.get(x);
                    Integer cy = yToCy.get(y);
                    if (cx != null && cy != null) {
                        if (!grid[cx][cy]) {
                            allValid = false;
                            break;
                        }
                    } else {
                        if (!pointInPolygon.apply(x, y)) {
                            allValid = false;
                            break;
                        }
                    }
                }
                
                if (allValid) {
                    maxAreaPart2 = cand.area;
                    break;
                }
            }
        }
        
        return new String[]{String.valueOf(maxAreaPart1), String.valueOf(maxAreaPart2)};
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String data = Files.readString(inputPath);
        String[] results = solve(data);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
}
