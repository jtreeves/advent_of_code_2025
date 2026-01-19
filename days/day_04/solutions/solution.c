#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Maximum line length
#define MAX_LINE_LENGTH 1024

// Structure to hold input lines
typedef struct {
    char **lines;
    int count;
    int capacity;
} InputLines;

#define INITIAL_CAPACITY 100


void free_input_lines(InputLines* input) {
    if (input == NULL) {
        return;
    }

    for (int i = 0; i < input->count; i++) {
        free(input->lines[i]);
    }
    free(input->lines);
    free(input);
}

int count_neighbors(char** grid, int i, int j, int rows, int cols) {
    int count = 0;
    for (int di = -1; di <= 1; di++) {
        for (int dj = -1; dj <= 1; dj++) {
            if (di == 0 && dj == 0) continue;
            int ni = i + di;
            int nj = j + dj;
            if (ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@') {
                count++;
            }
        }
    }
    return count;
}

void solve(InputLines* lines, int* part1_result, int* part2_result) {
    int rows = lines->count;
    int cols = rows > 0 ? strlen(lines->lines[0]) : 0;
    
    // Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    int part1_count = 0;
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (lines->lines[i][j] == '@') {
                int neighbors = count_neighbors(lines->lines, i, j, rows, cols);
                if (neighbors < 4) {
                    part1_count++;
                }
            }
        }
    }
    
    // Part 2: Iteratively remove accessible rolls until none can be removed
    // Create mutable copy
    char** grid = (char**)malloc(rows * sizeof(char*));
    for (int i = 0; i < rows; i++) {
        grid[i] = (char*)malloc((cols + 1) * sizeof(char));
        strcpy(grid[i], lines->lines[i]);
    }
    
    int part2_count = 0;
    while (1) {
        // Find positions to remove
        int* to_remove_i = (int*)malloc(rows * cols * sizeof(int));
        int* to_remove_j = (int*)malloc(rows * cols * sizeof(int));
        int to_remove_count = 0;
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (grid[i][j] == '@') {
                    int neighbors = count_neighbors(grid, i, j, rows, cols);
                    if (neighbors < 4) {
                        to_remove_i[to_remove_count] = i;
                        to_remove_j[to_remove_count] = j;
                        to_remove_count++;
                    }
                }
            }
        }
        
        if (to_remove_count == 0) {
            free(to_remove_i);
            free(to_remove_j);
            break;
        }
        
        // Remove all marked positions
        for (int k = 0; k < to_remove_count; k++) {
            grid[to_remove_i[k]][to_remove_j[k]] = '.';
        }
        part2_count += to_remove_count;
        
        free(to_remove_i);
        free(to_remove_j);
    }
    
    // Free grid memory
    for (int i = 0; i < rows; i++) {
        free(grid[i]);
    }
    free(grid);
    
    *part1_result = part1_count;
    *part2_result = part2_count;
}

int main() {
    FILE* file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        fprintf(stderr, "Error reading input\n");
        return 1;
    }

    InputLines* lines = (InputLines*)malloc(sizeof(InputLines));
    lines->count = 0;
    lines->capacity = INITIAL_CAPACITY;
    lines->lines = (char**)malloc(lines->capacity * sizeof(char*));

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }
        if (lines->count >= lines->capacity) {
            lines->capacity *= 2;
            lines->lines = (char**)realloc(lines->lines, lines->capacity * sizeof(char*));
        }
        lines->lines[lines->count] = (char*)malloc((strlen(line) + 1) * sizeof(char));
        strcpy(lines->lines[lines->count], line);
        lines->count++;
    }

    fclose(file);
    
    int part1, part2;
    solve(lines, &part1, &part2);
    
    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);
    
    free_input_lines(lines);
    return 0;
}
