#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    int start_col;
    int end_col;
    char op;
} Problem;

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
    
    // Find maximum line length
    int max_len = 0;
    for (int i = 0; i < line_count; i++) {
        int len = strlen(lines[i]);
        if (len > max_len) max_len = len;
    }
    
    // Pad all lines to max_len
    for (int i = 0; i < line_count; i++) {
        int len = strlen(lines[i]);
        if (len < max_len) {
            lines[i] = (char*)realloc(lines[i], max_len + 1);
            memset(lines[i] + len, ' ', max_len - len);
            lines[i][max_len] = '\0';
        }
    }
    
    // Operator row is the last row
    int op_row_idx = line_count - 1;
    char* op_row = lines[op_row_idx];
    
    // Find problem boundaries (columns that are all spaces)
    int* is_space_col = (int*)calloc(max_len, sizeof(int));
    for (int col = 0; col < max_len; col++) {
        int all_spaces = 1;
        for (int i = 0; i < line_count; i++) {
            if (col < strlen(lines[i]) && lines[i][col] != ' ') {
                all_spaces = 0;
                break;
            }
        }
        is_space_col[col] = all_spaces;
    }
    
    // Group columns into problems
    Problem* problems = NULL;
    int problem_count = 0;
    int problem_capacity = 100;
    problems = (Problem*)malloc(problem_capacity * sizeof(Problem));
    
    int i = 0;
    while (i < max_len) {
        if (!is_space_col[i]) {
            int start_col = i;
            while (i < max_len && !is_space_col[i]) {
                i++;
            }
            int end_col = i;
            
            // Extract operator for this problem
            char op = 0;
            for (int j = start_col; j < end_col; j++) {
                if (j < strlen(op_row) && (op_row[j] == '+' || op_row[j] == '*')) {
                    op = op_row[j];
                    break;
                }
            }
            
            if (op != 0) {
                if (problem_count >= problem_capacity) {
                    problem_capacity *= 2;
                    problems = (Problem*)realloc(problems, problem_capacity * sizeof(Problem));
                }
                problems[problem_count].start_col = start_col;
                problems[problem_count].end_col = end_col;
                problems[problem_count].op = op;
                problem_count++;
            }
        } else {
            i++;
        }
    }
    
    // Part 1: Extract numbers horizontally
    long long part1_total = 0;
    
    for (int p = 0; p < problem_count; p++) {
        Problem* prob = &problems[p];
        long long* numbers = NULL;
        int num_count = 0;
        int num_capacity = 100;
        numbers = (long long*)malloc(num_capacity * sizeof(long long));
        
        // Extract numbers from each row in this problem area
        for (int row_idx = 0; row_idx < op_row_idx; row_idx++) {
            char* row = lines[row_idx];
            if (prob->end_col > strlen(row)) continue;
            
            // Extract substring for this problem
            char* substr = (char*)malloc((prob->end_col - prob->start_col + 1) * sizeof(char));
            strncpy(substr, row + prob->start_col, prob->end_col - prob->start_col);
            substr[prob->end_col - prob->start_col] = '\0';
            
            // Trim and split by spaces
            // Skip leading/trailing spaces
            char* start = substr;
            while (*start == ' ') start++;
            char* end = substr + strlen(substr) - 1;
            while (end > start && *end == ' ') *end-- = '\0';
            
            // Split by spaces
            char* token = strtok(start, " \t");
            while (token != NULL) {
                long long num = strtoll(token, NULL, 10);
                if (num_count >= num_capacity) {
                    num_capacity *= 2;
                    numbers = (long long*)realloc(numbers, num_capacity * sizeof(long long));
                }
                numbers[num_count++] = num;
                token = strtok(NULL, " \t");
            }
            
            free(substr);
        }
        
        // Apply operator
        if (num_count > 0) {
            long long result;
            if (prob->op == '+') {
                result = 0;
                for (int n = 0; n < num_count; n++) {
                    result += numbers[n];
                }
            } else {
                result = 1;
                for (int n = 0; n < num_count; n++) {
                    result *= numbers[n];
                }
            }
            part1_total += result;
        }
        
        free(numbers);
    }
    
    // Part 2: Parse vertically (columns, right-to-left)
    // Approach: Manual memory management with column extraction
    long long part2_total = 0;
    
    for (int p = 0; p < problem_count; p++) {
        Problem* prob = &problems[p];
        
        // Extract column strings (transpose)
        char** col_strings = NULL;
        int col_str_count = 0;
        int col_str_capacity = 100;
        col_strings = (char**)malloc(col_str_capacity * sizeof(char*));
        
        for (int col = prob->start_col; col < prob->end_col; col++) {
            if (col < max_len && is_space_col[col]) continue;
            
            // Build column string
            int col_str_len = op_row_idx + 1;
            char* col_str = (char*)malloc(col_str_len * sizeof(char));
            int idx = 0;
            for (int row_idx = 0; row_idx < op_row_idx; row_idx++) {
                if (col < strlen(lines[row_idx])) {
                    char ch = lines[row_idx][col];
                    if (isdigit(ch) || ch == ' ') {
                        col_str[idx++] = ch;
                    }
                }
            }
            col_str[idx] = '\0';
            
            // Trim spaces
            while (idx > 0 && col_str[idx-1] == ' ') {
                col_str[--idx] = '\0';
            }
            
            if (idx > 0) {
                if (col_str_count >= col_str_capacity) {
                    col_str_capacity *= 2;
                    col_strings = (char**)realloc(col_strings, col_str_capacity * sizeof(char*));
                }
                col_strings[col_str_count++] = col_str;
            } else {
                free(col_str);
            }
        }
        
        // Parse numbers from columns (right-to-left means reverse order)
        long long* numbers = NULL;
        int num_count = 0;
        int num_capacity = 100;
        numbers = (long long*)malloc(num_capacity * sizeof(long long));
        
        for (int i = col_str_count - 1; i >= 0; i--) {
            // Remove spaces and parse
            char* digits = (char*)malloc(strlen(col_strings[i]) + 1);
            int d_idx = 0;
            for (int j = 0; col_strings[i][j] != '\0'; j++) {
                if (isdigit(col_strings[i][j])) {
                    digits[d_idx++] = col_strings[i][j];
                }
            }
            digits[d_idx] = '\0';
            
            if (d_idx > 0) {
                if (num_count >= num_capacity) {
                    num_capacity *= 2;
                    numbers = (long long*)realloc(numbers, num_capacity * sizeof(long long));
                }
                numbers[num_count++] = strtoll(digits, NULL, 10);
            }
            free(digits);
        }
        
        // Clean up column strings
        for (int i = 0; i < col_str_count; i++) {
            free(col_strings[i]);
        }
        free(col_strings);
        
        // Apply operator
        if (num_count > 0) {
            long long result;
            if (prob->op == '+') {
                result = 0;
                for (int n = 0; n < num_count; n++) {
                    result += numbers[n];
                }
            } else {
                result = 1;
                for (int n = 0; n < num_count; n++) {
                    result *= numbers[n];
                }
            }
            part2_total += result;
        }
        
        free(numbers);
    }
    
    *part1_result = part1_total;
    *part2_result = part2_total;
    
    free(lines);
    free(is_space_col);
    free(problems);
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
