#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

void solve(char* content, char** part1_result, char** part2_result) {
    // Split content into lines
    char** lines = NULL;
    int line_count = 0;
    int capacity = 10000;
    lines = (char**)malloc(capacity * sizeof(char*));
    
    char* pos = content;
    while (*pos != '\0') {
        char* line_start = pos;
        // Find next newline or end of string
        while (*pos != '\n' && *pos != '\0') {
            pos++;
        }
        
        if (line_count >= capacity) {
            capacity *= 2;
            lines = (char**)realloc(lines, capacity * sizeof(char*));
        }
        
        if (*pos == '\n') {
            *pos = '\0';
            pos++;
        }
        
        lines[line_count++] = line_start;
    }
    
    if (line_count == 0) {
        *part1_result = strdup("0");
        *part2_result = strdup("Final star");
        free(lines);
        return;
    }
    
    // Parse shapes (first 6 shapes, numbered 0-5)
    int shape_areas[6] = {0, 0, 0, 0, 0, 0};
    int i = 0;
    int shape_idx = 0;
    
    while (i < line_count && shape_idx < 6) {
        // Trim line
        char* line = lines[i];
        while (*line == ' ' || *line == '\t') line++;
        int line_len = strlen(line);
        while (line_len > 0 && (line[line_len-1] == ' ' || line[line_len-1] == '\t' || line[line_len-1] == '\r')) {
            line[line_len-1] = '\0';
            line_len--;
        }
        
        // Check if this is a shape header (format: "number:")
        if (line_len > 0 && line[line_len-1] == ':') {
            line[line_len-1] = '\0';
            int shape_num = -1;
            if (sscanf(line, "%d", &shape_num) == 1 && shape_num == shape_idx) {
                // Read the next 3 lines for the shape grid
                int area = 0;
                for (int j = 0; j < 3; j++) {
                    if (i + 1 + j < line_count) {
                        char* row = lines[i + 1 + j];
                        // Count '#' characters
                        for (int k = 0; row[k] != '\0'; k++) {
                            if (row[k] == '#') {
                                area++;
                            }
                        }
                    }
                }
                shape_areas[shape_idx] = area;
                shape_idx++;
                i += 4; // Skip shape header + 3 grid lines + empty line (if present)
                continue;
            }
        }
        i++;
    }
    
    // Find where queries start (skip empty lines after shapes)
    int query_start = i;
    while (query_start < line_count) {
        char* line = lines[query_start];
        while (*line == ' ' || *line == '\t') line++;
        if (*line != '\0') break;
        query_start++;
    }
    
    // Parse queries
    int possible_count = 0;
    for (int line_idx = query_start; line_idx < line_count; line_idx++) {
        char* line = lines[line_idx];
        // Trim
        while (*line == ' ' || *line == '\t') line++;
        int line_len = strlen(line);
        while (line_len > 0 && (line[line_len-1] == ' ' || line[line_len-1] == '\t' || line[line_len-1] == '\r')) {
            line[line_len-1] = '\0';
            line_len--;
        }
        if (line_len == 0) continue;
        
        // Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        char* colon = strchr(line, ':');
        if (!colon) continue;
        
        *colon = '\0';
        char* dims = line;
        char* counts_str = colon + 1;
        
        // Parse dimensions
        char* x = strchr(dims, 'x');
        if (!x) {
            *colon = ':';
            continue;
        }
        
        *x = '\0';
        int width = atoi(dims);
        int height = atoi(x + 1);
        
        // Parse counts
        int counts[6];
        int count_idx = 0;
        char* token = strtok(counts_str, " \t");
        int valid = 1;
        while (token && count_idx < 6) {
            counts[count_idx++] = atoi(token);
            token = strtok(NULL, " \t");
        }
        
        if (count_idx != 6) {
            *x = 'x';
            *colon = ':';
            continue;
        }
        
        // Calculate area check
        long long region_area = (long long)width * (long long)height;
        long long required_area = 0;
        for (int j = 0; j < 6; j++) {
            required_area += (long long)shape_areas[j] * (long long)counts[j];
        }
        
        if (required_area <= region_area) {
            possible_count++;
        }
        
        *x = 'x';
        *colon = ':';
    }
    
    // Part 2: Final star (no computation needed)
    *part1_result = (char*)malloc(32);
    snprintf(*part1_result, 32, "%d", possible_count);
    *part2_result = strdup("Final star");
    
    free(lines);
}

int main() {
    FILE* file = fopen("../data/input.txt", "r");
    if (!file) {
        fprintf(stderr, "Error opening file\n");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* content = (char*)malloc(size + 1);
    fread(content, 1, size, file);
    content[size] = '\0';
    fclose(file);
    
    char* part1 = NULL;
    char* part2 = NULL;
    solve(content, &part1, &part2);
    
    printf("Part 1: %s\n", part1);
    printf("Part 2: %s\n", part2);
    
    free(content);
    free(part1);
    free(part2);
    
    return 0;
}
