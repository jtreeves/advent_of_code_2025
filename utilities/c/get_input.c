#include "get_input.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INITIAL_CAPACITY 100

InputLines* get_input(int day) {
    char* path = get_input_path(day);
    InputLines* result = read_input(path);
    free(path);
    return result;
}

InputLines* get_test_input(int day, int test_num) {
    char* path = get_test_input_path(day, test_num);
    InputLines* result = read_input(path);
    free(path);
    return result;
}

char* get_input_path(int day) {
    // Solutions are in days/day_NN/solutions/
    // Input is in days/day_NN/data/input.txt
    char* path = (char*)malloc(256 * sizeof(char));
    snprintf(path, 256, "../data/input.txt");
    return path;
}

char* get_test_input_path(int day, int test_num) {
    char* path = (char*)malloc(256 * sizeof(char));
    snprintf(path, 256, "../data/test_%d.txt", test_num);
    return path;
}

InputLines* read_input(const char* file_path) {
    FILE* file = fopen(file_path, "r");
    if (file == NULL) {
        return NULL;
    }

    InputLines* input = (InputLines*)malloc(sizeof(InputLines));
    input->count = 0;
    input->capacity = INITIAL_CAPACITY;
    input->lines = (char**)malloc(input->capacity * sizeof(char*));

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        // Trim newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }

        // Expand capacity if needed
        if (input->count >= input->capacity) {
            input->capacity *= 2;
            input->lines = (char**)realloc(input->lines, input->capacity * sizeof(char*));
        }

        // Allocate and copy line
        input->lines[input->count] = (char*)malloc((strlen(line) + 1) * sizeof(char));
        strcpy(input->lines[input->count], line);
        input->count++;
    }

    fclose(file);
    return input;
}

char* read_input_raw(const char* file_path) {
    FILE* file = fopen(file_path, "r");
    if (file == NULL) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* content = (char*)malloc((size + 1) * sizeof(char));
    fread(content, 1, size, file);
    content[size] = '\0';

    fclose(file);
    return content;
}

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

void free_input_raw(char* content) {
    if (content != NULL) {
        free(content);
    }
}
