import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Common utility functions for Java solutions.
 */
public class Utils {
    public static List<String> readInput(String filePath) throws IOException {
        Path path = Paths.get(filePath);
        return Files.readAllLines(path);
    }
    
    public static String readInputRaw(String filePath) throws IOException {
        Path path = Paths.get(filePath);
        return Files.readString(path);
    }
    
    public static List<Integer> parseInts(String line) {
        Pattern pattern = Pattern.compile("-?\\d+");
        return pattern.matcher(line)
            .results()
            .map(m -> Integer.parseInt(m.group()))
            .collect(Collectors.toList());
    }
    
    // Placeholder for additional common utilities
}
