#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void solve(char* content, long long* part1_result, long long* part2_result) {
    // Split content into lines
    char** lines = NULL;
    int line_count = 0;
    int capacity = 1000;
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
        *part1_result = 0;
        *part2_result = 0;
        free(lines);
        return;
    }
    
    // Build grid
    int rows = line_count;
    int cols = strlen(lines[0]);
    char** grid = lines;
    
    // Find starting position S
    int start_row = -1, start_col = -1;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 'S') {
                start_row = r;
                start_col = c;
                break;
            }
        }
        if (start_row != -1) break;
    }
    
    if (start_row == -1) {
        *part1_result = 0;
        *part2_result = 0;
        free(lines);
        return;
    }
    
    // Part 1: Count total splits using sets (represented as arrays)
    int split_count = 0;
    int* active_beams = (int*)calloc(cols, sizeof(int));
    active_beams[start_col] = 1;
    
    for (int r = start_row + 1; r < rows; r++) {
        int* next_beams = (int*)calloc(cols, sizeof(int));
        for (int c = 0; c < cols; c++) {
            if (active_beams[c]) {
                if (grid[r][c] == '.') {
                    next_beams[c] = 1;
                } else if (grid[r][c] == '^') {
                    split_count++;
                    if (c - 1 >= 0) next_beams[c - 1] = 1;
                    if (c + 1 < cols) next_beams[c + 1] = 1;
                }
            }
        }
        free(active_beams);
        active_beams = next_beams;
    }
    free(active_beams);
    
    // Part 2: Count beams reaching bottom row using 2D array
    long long** beam_counts = (long long**)malloc(rows * sizeof(long long*));
    for (int r = 0; r < rows; r++) {
        beam_counts[r] = (long long*)calloc(cols, sizeof(long long));
    }
    beam_counts[start_row][start_col] = 1;
    
    for (int r = start_row + 1; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            long long prev_count = beam_counts[r - 1][c];
            if (prev_count > 0) {
                if (grid[r][c] == '.') {
                    beam_counts[r][c] += prev_count;
                } else if (grid[r][c] == '^') {
                    if (c - 1 >= 0) beam_counts[r][c - 1] += prev_count;
                    if (c + 1 < cols) beam_counts[r][c + 1] += prev_count;
                }
            }
        }
    }
    
    // Sum all beams in bottom row
    long long bottom_beam_count = 0;
    for (int c = 0; c < cols; c++) {
        bottom_beam_count += beam_counts[rows - 1][c];
    }
    
    // Clean up
    for (int r = 0; r < rows; r++) {
        free(beam_counts[r]);
    }
    free(beam_counts);
    
    *part1_result = split_count;
    *part2_result = bottom_beam_count;
    // Note: lines are not freed here as they point to content
}

int main() {
    FILE* file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "Error opening file\n");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* content = (char*)malloc(file_size + 1);
    fread(content, 1, file_size, file);
    content[file_size] = '\0';
    fclose(file);
    
    long long part1, part2;
    solve(content, &part1, &part2);
    
    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);
    
    free(content);
    return 0;
}
