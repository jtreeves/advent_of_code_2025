import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Input reading utilities for Java solutions.
 */
public class GetInput {
    /**
     * Read input from input.txt for the given day.
     * @param day Day number (1, 2, 3, etc.)
     * @return List of lines as strings
     */
    public static List<String> getInput(int day) throws IOException {
        String path = getInputPath(day);
        return readInput(path);
    }
    
    /**
     * Read input from test_N.txt for the given day and test number.
     * @param day Day number (1, 2, 3, etc.)
     * @param testNum Test number (1, 2, 3, etc.)
     * @return List of lines as strings
     */
    public static List<String> getTestInput(int day, int testNum) throws IOException {
        String path = getTestInputPath(day, testNum);
        return readInput(path);
    }
    
    /**
     * Return the path to input.txt for the given day.
     */
    public static String getInputPath(int day) {
        return "../data/input.txt";
    }
    
    /**
     * Return the path to test_N.txt for the given day and test number.
     */
    public static String getTestInputPath(int day, int testNum) {
        return String.format("../data/test_%d.txt", testNum);
    }
    
    /**
     * Read input file and return lines as a list.
     */
    public static List<String> readInput(String filePath) throws IOException {
        Path path = Paths.get(filePath);
        return Files.readAllLines(path);
    }
    
    /**
     * Read input file and return raw content.
     */
    public static String readInputRaw(String filePath) throws IOException {
        Path path = Paths.get(filePath);
        return Files.readString(path);
    }
}
