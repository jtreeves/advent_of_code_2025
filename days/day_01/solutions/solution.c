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

void solve(InputLines* lines, int* part1_result, int* part2_result) {
    // Part 1: Count times dial ends at 0 after a rotation
    int position = 50;
    int count_part1 = 0;
    
    for (int i = 0; i < lines->count; i++) {
        char* line = lines->lines[i];
        if (strlen(line) == 0) continue;
        
        char direction = line[0];
        int distance = atoi(&line[1]);
        
        // Apply rotation
        if (direction == 'L') {
            position = ((position - distance) % 100 + 100) % 100;
        } else { // direction == 'R'
            position = (position + distance) % 100;
        }
        
        // Check if ended at 0
        if (position == 0) {
            count_part1++;
        }
    }
    
    // Part 2: Count times dial is at 0 during entire process
    position = 50;
    int count_part2 = 0;
    
    for (int i = 0; i < lines->count; i++) {
        char* line = lines->lines[i];
        if (strlen(line) == 0) continue;
        
        char direction = line[0];
        int distance = atoi(&line[1]);
        
        int start_pos = position;
        
        // Check each click position during rotation
        for (int click = 1; click <= distance; click++) {
            int click_pos;
            if (direction == 'L') {
                click_pos = ((start_pos - click) % 100 + 100) % 100;
            } else { // direction == 'R'
                click_pos = (start_pos + click) % 100;
            }
            
            if (click_pos == 0) {
                count_part2++;
            }
        }
        
        // Update position after rotation
        if (direction == 'L') {
            position = ((position - distance) % 100 + 100) % 100;
        } else {
            position = (position + distance) % 100;
        }
    }
    
    *part1_result = count_part1;
    *part2_result = count_part2;
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
