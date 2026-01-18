#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Find the largest N-digit number by selecting N digits in order from bank
long long find_largest_subsequence(const char* bank, int n) {
    int bank_len = strlen(bank);
    if (bank_len < n) {
        return 0;
    }
    
    char result[16] = {0};  // Max 12 digits + null terminator
    int start = 0;
    int result_idx = 0;
    
    for (int i = 0; i < n; i++) {
        int remaining_needed = n - i - 1;
        int end = bank_len - remaining_needed;
        
        char max_digit = bank[start];
        int max_pos = start;
        for (int j = start + 1; j < end; j++) {
            if (bank[j] > max_digit) {
                max_digit = bank[j];
                max_pos = j;
            }
        }
        
        result[result_idx++] = max_digit;
        start = max_pos + 1;
    }
    
    result[result_idx] = '\0';
    return atoll(result);
}

int main() {
    FILE *file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }
    
    char line[256];
    long long part1_sum = 0;
    long long part2_sum = 0;
    
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;
        
        if (line[0] == '\0') {
            continue;
        }
        
        part1_sum += find_largest_subsequence(line, 2);
        
        if (strlen(line) >= 12) {
            part2_sum += find_largest_subsequence(line, 12);
        }
    }
    
    fclose(file);
    
    printf("Part 1: %lld\n", part1_sum);
    printf("Part 2: %lld\n", part2_sum);
    
    return 0;
}
