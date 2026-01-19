#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    int* target_pattern;
    int target_pattern_size;
    int** buttons;
    int* button_sizes;
    int num_buttons;
    int* joltages;
    int joltages_size;
} ParseResult;

void free_parse_result(ParseResult* pr) {
    if (pr->target_pattern) free(pr->target_pattern);
    if (pr->buttons) {
        for (int i = 0; i < pr->num_buttons; i++) {
            if (pr->buttons[i]) free(pr->buttons[i]);
        }
        free(pr->buttons);
    }
    if (pr->button_sizes) free(pr->button_sizes);
    if (pr->joltages) free(pr->joltages);
}

// Simple regex-like parsing without regex library
ParseResult parse_line(const char* line) {
    ParseResult result = {0};
    
    // Find pattern: [.##.]
    const char* pattern_start = strchr(line, '[');
    if (pattern_start) {
        const char* pattern_end = strchr(pattern_start + 1, ']');
        if (pattern_end) {
            int len = pattern_end - pattern_start - 1;
            result.target_pattern = (int*)malloc(len * sizeof(int));
            result.target_pattern_size = len;
            for (int i = 0; i < len; i++) {
                result.target_pattern[i] = (pattern_start[1 + i] == '#') ? 1 : 0;
            }
        }
    }
    
    // Extract buttons: (1,3) (2) etc.
    result.buttons = (int**)malloc(100 * sizeof(int*));
    result.button_sizes = (int*)malloc(100 * sizeof(int));
    result.num_buttons = 0;
    
    const char* pos = line;
    while ((pos = strchr(pos, '(')) != NULL) {
        const char* btn_start = pos + 1;
        const char* btn_end = strchr(btn_start, ')');
        if (!btn_end) break;
        
        int btn_len = btn_end - btn_start;
        if (btn_len > 0) {
            char* btn_str = (char*)malloc((btn_len + 1) * sizeof(char));
            strncpy(btn_str, btn_start, btn_len);
            btn_str[btn_len] = '\0';
            
            // Parse comma-separated numbers
            int* lights = (int*)malloc(100 * sizeof(int));
            int light_count = 0;
            char* token = strtok(btn_str, ",");
            while (token) {
                // Skip whitespace
                while (*token == ' ') token++;
                lights[light_count++] = atoi(token);
                token = strtok(NULL, ",");
            }
            
            result.buttons[result.num_buttons] = (int*)realloc(lights, light_count * sizeof(int));
            result.button_sizes[result.num_buttons] = light_count;
            result.num_buttons++;
            
            free(btn_str);
        } else {
            result.buttons[result.num_buttons] = NULL;
            result.button_sizes[result.num_buttons] = 0;
            result.num_buttons++;
        }
        pos = btn_end + 1;
    }
    
    // Extract joltages: {3,5,4,7}
    const char* joltage_start = strchr(line, '{');
    if (joltage_start) {
        const char* joltage_end = strchr(joltage_start + 1, '}');
        if (joltage_end) {
            int len = joltage_end - joltage_start - 1;
            char* joltage_str = (char*)malloc((len + 1) * sizeof(char));
            strncpy(joltage_str, joltage_start + 1, len);
            joltage_str[len] = '\0';
            
            result.joltages = (int*)malloc(100 * sizeof(int));
            result.joltages_size = 0;
            
            char* token = strtok(joltage_str, ",");
            while (token) {
                while (*token == ' ') token++;
                result.joltages[result.joltages_size++] = atoi(token);
                token = strtok(NULL, ",");
            }
            
            result.joltages = (int*)realloc(result.joltages, result.joltages_size * sizeof(int));
            free(joltage_str);
        }
    }
    
    return result;
}

int gaussian_elimination_gf2(int** matrix, int num_buttons, int num_lights, int* target, int* result_count) {
    // Create augmented matrix [A | b]
    int** aug = (int**)malloc(num_lights * sizeof(int*));
    for (int i = 0; i < num_lights; i++) {
        aug[i] = (int*)malloc((num_buttons + 1) * sizeof(int));
        for (int j = 0; j < num_buttons; j++) {
            if (j < num_buttons && i < num_lights && matrix[j]) {
                aug[i][j] = matrix[j][i];
            } else {
                aug[i][j] = 0;
            }
        }
        aug[i][num_buttons] = target[i];
    }
    
    // Gaussian elimination mod 2
    int pivot_row = 0;
    int pivot_col = 0;
    
    while (pivot_row < num_lights && pivot_col < num_buttons) {
        // Find pivot
        int pivot_idx = -1;
        for (int i = pivot_row; i < num_lights; i++) {
            if (aug[i][pivot_col]) {
                pivot_idx = i;
                break;
            }
        }
        
        if (pivot_idx == -1) {
            pivot_col++;
            continue;
        }
        
        // Swap rows
        if (pivot_idx != pivot_row) {
            int* temp = aug[pivot_row];
            aug[pivot_row] = aug[pivot_idx];
            aug[pivot_idx] = temp;
        }
        
        // Eliminate
        for (int i = pivot_row + 1; i < num_lights; i++) {
            if (aug[i][pivot_col]) {
                for (int j = 0; j <= num_buttons; j++) {
                    aug[i][j] ^= aug[pivot_row][j];
                }
            }
        }
        
        pivot_row++;
        pivot_col++;
    }
    
    // Check for inconsistency (0 = 1)
    for (int i = pivot_row; i < num_lights; i++) {
        if (aug[i][num_buttons]) {
            int has_non_zero = 0;
            for (int j = 0; j < num_buttons; j++) {
                if (aug[i][j]) {
                    has_non_zero = 1;
                    break;
                }
            }
            if (!has_non_zero) {
                // Free memory
                for (int i = 0; i < num_lights; i++) free(aug[i]);
                free(aug);
                return 0; // No solution
            }
        }
    }
    
    // Back substitution to find solution (set free variables to 0 to minimize)
    int* solution = (int*)calloc(num_buttons, sizeof(int));
    int* used_rows = (int*)calloc(num_buttons, sizeof(int));
    
    // Process from bottom up
    for (int i = num_lights - 1; i >= 0; i--) {
        // Find first non-zero column
        int pivot_col_idx = -1;
        for (int j = 0; j < num_buttons; j++) {
            if (aug[i][j] && !used_rows[j]) {
                pivot_col_idx = j;
                used_rows[j] = 1;
                break;
            }
        }
        
        if (pivot_col_idx != -1) {
            // Calculate value
            int val = aug[i][num_buttons];
            for (int j = pivot_col_idx + 1; j < num_buttons; j++) {
                if (aug[i][j] && solution[j]) {
                    val ^= 1;
                }
            }
            solution[pivot_col_idx] = val;
        }
    }
    
    // Count number of presses
    int count = 0;
    for (int i = 0; i < num_buttons; i++) {
        if (solution[i]) count++;
    }
    
    // Free memory
    free(solution);
    free(used_rows);
    for (int i = 0; i < num_lights; i++) free(aug[i]);
    free(aug);
    
    *result_count = count;
    return 1; // Solution found
}

int solve_part2_ilp(int** buttons, int* button_sizes, int num_buttons, int* joltages, int num_lights, int button_idx, int* current_joltages, int presses_so_far, int* best) {
    if (button_idx >= num_buttons) {
        int all_match = 1;
        for (int i = 0; i < num_lights; i++) {
            if (current_joltages[i] != joltages[i]) {
                all_match = 0;
                break;
            }
        }
        if (all_match) {
            if (*best == -1 || presses_so_far < *best) {
                *best = presses_so_far;
            }
            return *best;
        }
        return *best;
    }
    
    // Pruning: if current presses already exceed best, skip
    if (*best != -1 && presses_so_far >= *best) {
        return *best;
    }
    
    // Calculate minimum additional presses needed (lower bound)
    int sum_remaining = 0;
    for (int i = 0; i < num_lights; i++) {
        int need = joltages[i] - current_joltages[i];
        if (need > 0) sum_remaining += need;
    }
    if (sum_remaining > 0) {
        int max_lights_per_button = 1;
        for (int i = button_idx; i < num_buttons; i++) {
            if (button_sizes[i] > max_lights_per_button) {
                max_lights_per_button = button_sizes[i];
            }
        }
        if (max_lights_per_button > 0) {
            int min_additional = (sum_remaining + max_lights_per_button - 1) / max_lights_per_button;
            if (*best != -1 && presses_so_far + min_additional >= *best) {
                return *best;
            }
        }
    }
    
    // Find max joltage
    int max_joltage = 0;
    for (int i = 0; i < num_lights; i++) {
        if (joltages[i] > max_joltage) max_joltage = joltages[i];
    }
    
    // Try 0 to max_joltage presses of current button
    int best_result = *best;
    for (int presses = 0; presses <= max_joltage; presses++) {
        if (*best != -1 && presses_so_far + presses >= *best) {
            break;
        }
        
        int* new_joltages = (int*)malloc(num_lights * sizeof(int));
        memcpy(new_joltages, current_joltages, num_lights * sizeof(int));
        
        for (int k = 0; k < button_sizes[button_idx]; k++) {
            int light = buttons[button_idx][k];
            if (light >= 0 && light < num_lights) {
                new_joltages[light] += presses;
            }
        }
        
        // Check if any exceeds target (pruning)
        int exceeds = 0;
        for (int i = 0; i < num_lights; i++) {
            if (new_joltages[i] > joltages[i]) {
                exceeds = 1;
                break;
            }
        }
        
        if (!exceeds) {
            int result = solve_part2_ilp(buttons, button_sizes, num_buttons, joltages, num_lights, button_idx + 1, new_joltages, presses_so_far + presses, best);
            if (result != -1 && (best_result == -1 || result < best_result)) {
                best_result = result;
                *best = result;
            }
        }
        
        free(new_joltages);
    }
    
    return best_result;
}

void solve(const char* input_data, long long* part1_result, long long* part2_result) {
    // Split into lines
    char** lines = NULL;
    int line_count = 0;
    int capacity = 1000;
    lines = (char**)malloc(capacity * sizeof(char*));
    
    const char* pos = input_data;
    while (*pos != '\0') {
        const char* line_start = pos;
        while (*pos != '\n' && *pos != '\0') pos++;
        
        if (line_count >= capacity) {
            capacity *= 2;
            lines = (char**)realloc(lines, capacity * sizeof(char*));
        }
        
        int len = pos - line_start;
        if (len > 0) {
            lines[line_count] = (char*)malloc((len + 1) * sizeof(char));
            strncpy(lines[line_count], line_start, len);
            lines[line_count][len] = '\0';
        } else {
            lines[line_count] = NULL;
        }
        line_count++;
        
        if (*pos == '\n') pos++;
    }
    
    *part1_result = 0;
    *part2_result = 0;
    
    for (int i = 0; i < line_count; i++) {
        if (!lines[i] || strlen(lines[i]) == 0) continue;
        
        ParseResult parsed = parse_line(lines[i]);
        if (parsed.target_pattern_size == 0) {
            free_parse_result(&parsed);
            continue;
        }
        
        int num_lights = parsed.target_pattern_size;
        
        // Part 1: GF(2) linear system
        // Build incidence matrix: matrix[i][j] = 1 if button i toggles light j
        int** button_matrix = (int**)malloc(parsed.num_buttons * sizeof(int*));
        for (int j = 0; j < parsed.num_buttons; j++) {
            button_matrix[j] = (int*)calloc(num_lights, sizeof(int));
            for (int k = 0; k < parsed.button_sizes[j]; k++) {
                int light = parsed.buttons[j][k];
                if (light >= 0 && light < num_lights) {
                    button_matrix[j][light] = 1;
                }
            }
        }
        
        // Required toggles: target_pattern
        int result_count = 0;
        if (gaussian_elimination_gf2(button_matrix, parsed.num_buttons, num_lights, parsed.target_pattern, &result_count)) {
            *part1_result += result_count;
        }
        
        // Free button matrix
        for (int j = 0; j < parsed.num_buttons; j++) {
            free(button_matrix[j]);
        }
        free(button_matrix);
        
        // Part 2: Integer Linear Programming
        if (parsed.joltages_size == num_lights) {
            int* initial_joltages = (int*)calloc(num_lights, sizeof(int));
            int best = -1;
            int result = solve_part2_ilp(parsed.buttons, parsed.button_sizes, parsed.num_buttons, parsed.joltages, num_lights, 0, initial_joltages, 0, &best);
            if (result != -1) {
                *part2_result += result;
            }
            free(initial_joltages);
        }
        
        free_parse_result(&parsed);
    }
    
    // Free lines
    for (int i = 0; i < line_count; i++) {
        if (lines[i]) free(lines[i]);
    }
    free(lines);
}

int main() {
    FILE* file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* content = (char*)malloc((size + 1) * sizeof(char));
    fread(content, 1, size, file);
    content[size] = '\0';
    fclose(file);
    
    long long part1, part2;
    solve(content, &part1, &part2);
    
    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);
    
    free(content);
    return 0;
}
