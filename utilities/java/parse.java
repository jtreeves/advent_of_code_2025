import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Parsing utilities for Java solutions.
 */
public class Parse {
    /**
     * Parse integers from a line of text.
     * @param line Line of text containing integers
     * @return List of integers found in the line
     */
    public static List<Integer> parseInts(String line) {
        Pattern pattern = Pattern.compile("-?\\d+");
        return pattern.matcher(line)
            .results()
            .map(m -> Integer.parseInt(m.group()))
            .collect(Collectors.toList());
    }
}
