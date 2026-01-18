#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef struct {
    long long start;
    long long end;
} Range;

int compare_ranges(const void* a, const void* b) {
    Range* r1 = (Range*)a;
    Range* r2 = (Range*)b;
    if (r1->start < r2->start) return -1;
    if (r1->start > r2->start) return 1;
    return 0;
}

void solve(char* content, long long* part1_result, long long* part2_result) {
    // Split content into lines manually (preserving blank lines)
    char** lines = NULL;
    int line_count = 0;
    int capacity = 1000;
    lines = (char**)malloc(capacity * sizeof(char*));
    
    int blank_idx = -1;
    char* start = content;
    char* pos = content;
    
    // Manually split on newlines
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
        
        // Replace newline with null terminator
        if (*pos == '\n') {
            *pos = '\0';
            lines[line_count] = line_start;
            pos++; // Move past newline
        } else {
            lines[line_count] = line_start;
        }
        
        // Check if this is a blank line
        if (strlen(lines[line_count]) == 0 && blank_idx == -1) {
            blank_idx = line_count;
        }
        
        line_count++;
    }
    
    if (blank_idx == -1) blank_idx = line_count;
    
    // Parse ranges (first section)
    Range* ranges = NULL;
    int range_count = 0;
    int range_capacity = 1000;
    ranges = (Range*)malloc(range_capacity * sizeof(Range));
    
    for (int i = 0; i < blank_idx; i++) {
        if (lines[i] == NULL || strlen(lines[i]) == 0) continue;
        char* dash = strchr(lines[i], '-');
        if (dash == NULL) continue;
        
        long long start = strtoll(lines[i], NULL, 10);
        long long end = strtoll(dash + 1, NULL, 10);
        
        if (range_count >= range_capacity) {
            range_capacity *= 2;
            ranges = (Range*)realloc(ranges, range_capacity * sizeof(Range));
        }
        ranges[range_count].start = start;
        ranges[range_count].end = end;
        range_count++;
    }
    
    // Parse IDs (second section)
    long long* ids = NULL;
    int id_count = 0;
    int id_capacity = 1000;
    ids = (long long*)malloc(id_capacity * sizeof(long long));
    
    for (int i = blank_idx + 1; i < line_count; i++) {
        if (lines[i] == NULL || strlen(lines[i]) == 0) continue;
        long long id = strtoll(lines[i], NULL, 10);
        
        if (id_count >= id_capacity) {
            id_capacity *= 2;
            ids = (long long*)realloc(ids, id_capacity * sizeof(long long));
        }
        ids[id_count] = id;
        id_count++;
    }
    
    // Part 1: Count how many IDs fall into any range
    long long part1_count = 0;
    for (int i = 0; i < id_count; i++) {
        for (int j = 0; j < range_count; j++) {
            if (ranges[j].start <= ids[i] && ids[i] <= ranges[j].end) {
                part1_count++;
                break;
            }
        }
    }
    
    // Part 2: Sort and merge ranges
    qsort(ranges, range_count, sizeof(Range), compare_ranges);
    
    // Merge overlapping/adjacent ranges
    Range* merged = NULL;
    int merged_count = 0;
    int merged_capacity = range_count;
    merged = (Range*)malloc(merged_capacity * sizeof(Range));
    
    if (range_count > 0) {
        merged[0] = ranges[0];
        merged_count = 1;
        for (int i = 1; i < range_count; i++) {
            Range* last = &merged[merged_count - 1];
            if (ranges[i].start <= last->end + 1) {
                // Merge
                if (ranges[i].end > last->end) {
                    last->end = ranges[i].end;
                }
            } else {
                // New range
                if (merged_count >= merged_capacity) {
                    merged_capacity *= 2;
                    merged = (Range*)realloc(merged, merged_capacity * sizeof(Range));
                }
                merged[merged_count] = ranges[i];
                merged_count++;
            }
        }
    }
    
    // Calculate total unique IDs covered
    long long part2_total = 0;
    for (int i = 0; i < merged_count; i++) {
        part2_total += merged[i].end - merged[i].start + 1;
    }
    
    *part1_result = part1_count;
    *part2_result = part2_total;
    
    free(lines);
    free(ranges);
    free(ids);
    free(merged);
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
