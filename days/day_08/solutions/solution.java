import java.io.*;
import java.nio.file.*;
import java.util.*;

class Solution {
    static class Coord {
        long x, y, z;
        Coord(long x, long y, long z) {
            this.x = x; this.y = y; this.z = z;
        }
    }
    
    static class Pair {
        int i, j;
        long distSq;
        Pair(int i, int j, long distSq) {
            this.i = i; this.j = j; this.distSq = distSq;
        }
    }
    
    static class UnionFind {
        int[] parent;
        long[] size;
        int componentCount;
        
        UnionFind(int n) {
            parent = new int[n];
            size = new long[n];
            for (int i = 0; i < n; i++) {
                parent[i] = i;
                size[i] = 1;
            }
            componentCount = n;
        }
        
        int find(int x) {
            if (parent[x] != x) {
                parent[x] = find(parent[x]);
            }
            return parent[x];
        }
        
        boolean union(int x, int y) {
            int rootX = find(x);
            int rootY = find(y);
            if (rootX == rootY) {
                return false;
            }
            if (size[rootX] < size[rootY]) {
                int temp = rootX;
                rootX = rootY;
                rootY = temp;
            }
            parent[rootY] = rootX;
            size[rootX] += size[rootY];
            componentCount--;
            return true;
        }
    }
    
    static List<Coord> parseCoordinates(String[] lines) {
        List<Coord> coords = new ArrayList<>();
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            String[] parts = line.split(",");
            long x = Long.parseLong(parts[0]);
            long y = Long.parseLong(parts[1]);
            long z = Long.parseLong(parts[2]);
            coords.add(new Coord(x, y, z));
        }
        return coords;
    }
    
    static long squaredDistance(Coord p1, Coord p2) {
        long dx = p2.x - p1.x;
        long dy = p2.y - p1.y;
        long dz = p2.z - p1.z;
        return dx * dx + dy * dy + dz * dz;
    }
    
    public static String[] solve(String inputData) {
        String[] lines = inputData.trim().split("\n");
        List<Coord> coords = parseCoordinates(lines);
        
        int n = coords.size();
        if (n == 0) {
            return new String[]{"0", "0"};
        }
        
        // Generate all pairs with squared distances
        List<Pair> pairs = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                long distSq = squaredDistance(coords.get(i), coords.get(j));
                pairs.add(new Pair(i, j, distSq));
            }
        }
        
        // Sort by distance
        pairs.sort((a, b) -> Long.compare(a.distSq, b.distSq));
        
        // Part 1: Connect first 1000 pairs
        UnionFind uf1 = new UnionFind(n);
        int connectionsMade = 0;
        for (Pair pair : pairs) {
            if (connectionsMade >= 1000) break;
            uf1.union(pair.i, pair.j);
            connectionsMade++;
        }
        
        // Get component sizes
        Map<Integer, Long> componentSizes = new HashMap<>();
        for (int i = 0; i < n; i++) {
            int root = uf1.find(i);
            componentSizes.put(root, uf1.size[root]);
        }
        
        List<Long> sizes = new ArrayList<>(componentSizes.values());
        sizes.sort((a, b) -> Long.compare(b, a));
        long part1 = sizes.size() >= 3 ? sizes.get(0) * sizes.get(1) * sizes.get(2) : 0;
        
        // Part 2: Connect until all in one circuit
        UnionFind uf2 = new UnionFind(n);
        Pair finalPair = null;
        for (Pair pair : pairs) {
            if (uf2.componentCount == 1) break;
            if (uf2.union(pair.i, pair.j)) {
                if (uf2.componentCount == 1) {
                    finalPair = pair;
                    break;
                }
            }
        }
        
        long part2 = 0;
        if (finalPair != null) {
            part2 = coords.get(finalPair.i).x * coords.get(finalPair.j).x;
        }
        
        return new String[]{String.valueOf(part1), String.valueOf(part2)};
    }
    
    public static void main(String[] args) throws IOException {
        Path inputPath = Paths.get("../data/input.txt");
        String data = Files.readString(inputPath);
        String[] results = solve(data);
        System.out.println("Part 1: " + results[0]);
        System.out.println("Part 2: " + results[1]);
    }
}
