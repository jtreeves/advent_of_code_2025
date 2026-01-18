#ifndef GET_INPUT_H
#define GET_INPUT_H

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

// Read input from input.txt for the given day
// Returns array of lines, caller must free with free_input_lines
InputLines* get_input(int day);

// Read input from test_N.txt for the given day and test number
// Returns array of lines, caller must free with free_input_lines
InputLines* get_test_input(int day, int test_num);

// Get path to input.txt for the given day
// Solutions are in days/day_NN/solutions/
// Input is in days/day_NN/data/input.txt
char* get_input_path(int day);

// Get path to test_N.txt for the given day and test number
char* get_test_input_path(int day, int test_num);

// Read input file and return lines
InputLines* read_input(const char* file_path);

// Read input file and return raw content (caller must free)
char* read_input_raw(const char* file_path);

// Free memory allocated by get_input, get_test_input, or read_input
void free_input_lines(InputLines* input);

// Free memory allocated by read_input_raw
void free_input_raw(char* content);

#endif // GET_INPUT_H
