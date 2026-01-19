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

// Check if ID is invalid for Part 1: exactly two identical sequences
int is_invalid_part1(const char* id_str) {
    int n = strlen(id_str);
    if (n % 2 != 0) {
        return 0;
    }
    int half = n / 2;
    return strncmp(id_str, id_str + half, half) == 0;
}

// Check if ID is invalid for Part 2: sequence repeated 2+ times
int is_invalid_part2(const char* id_str) {
    int n = strlen(id_str);
    for (int k = 2; k <= n; k++) {
        if (n % k == 0) {
            int seq_len = n / k;
            // Check if pattern repeats k times
            int valid = 1;
            for (int i = 1; i < k; i++) {
                if (strncmp(id_str, id_str + i * seq_len, seq_len) != 0) {
                    valid = 0;
                    break;
                }
            }
            if (valid) {
                return 1;
            }
        }
    }
    return 0;
}

// Convert number to string
void num_to_str(long long num, char* str, int max_len) {
    snprintf(str, max_len, "%lld", num);
}

// Parse a range string like "start-end"
void parse_range(const char* range_str, long long* start, long long* end) {
    char* copy = strdup(range_str);
    char* token = strtok(copy, "-");
    *start = atoll(token);
    token = strtok(NULL, "-");
    *end = atoll(token);
    free(copy);
}

// Parse a line of comma-separated ranges
void parse_ranges(const char* line, long long* starts, long long* ends, int* count) {
    char* copy = strdup(line);
    *count = 0;
    
    char* token = strtok(copy, ",");
    while (token != NULL) {
        // Skip whitespace
        while (*token == ' ') token++;
        parse_range(token, &starts[*count], &ends[*count]);
        (*count)++;
        token = strtok(NULL, ",");
    }
    
    free(copy);
}

void solve(InputLines* lines, long long* part1_result, long long* part2_result) {
    long long part1_sum = 0;
    long long part2_sum = 0;
    
    char id_str[64];
    long long starts[100];
    long long ends[100];
    int range_count;
    
    for (int i = 0; i < lines->count; i++) {
        char* line = lines->lines[i];
        if (strlen(line) == 0) continue;
        
        parse_ranges(line, starts, ends, &range_count);
        
        for (int j = 0; j < range_count; j++) {
            long long start = starts[j];
            long long end = ends[j];
            
            for (long long num = start; num <= end; num++) {
                num_to_str(num, id_str, sizeof(id_str));
                
                if (is_invalid_part1(id_str)) {
                    part1_sum += num;
                }
                
                if (is_invalid_part2(id_str)) {
                    part2_sum += num;
                }
            }
        }
    }
    
    *part1_result = part1_sum;
    *part2_result = part2_sum;
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
    
    long long part1, part2;
    solve(lines, &part1, &part2);
    
    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);
    
    free_input_lines(lines);
    return 0;
}
