#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    long long x, y;
} Point;

typedef struct {
    Point* points;
    int count;
    int capacity;
} PointArray;

typedef struct {
    long long min_x, max_x, min_y, max_y, area;
} Candidate;

int point_in_polygon(long long px, long long py, Point* polygon, int n) {
    int inside = 0;
    for (int i = 0; i < n; i++) {
        long long x1 = polygon[i].x;
        long long y1 = polygon[i].y;
        long long x2 = polygon[(i + 1) % n].x;
        long long y2 = polygon[(i + 1) % n].y;
        if ((y1 > py) != (y2 > py)) {
            double intersect_x = (y2 != y1) 
                ? (double)(py - y1) * (x2 - x1) / (double)(y2 - y1) + x1
                : px;
            if (px < intersect_x) {
                inside = !inside;
            }
        }
    }
    return inside;
}

int is_valid_tile(long long x, long long y, Point* red_tiles, int n, 
                  Point* boundary_set, int boundary_count) {
    // Check if in boundary set
    for (int i = 0; i < boundary_count; i++) {
        if (boundary_set[i].x == x && boundary_set[i].y == y) {
            return 1;
        }
    }
    // Check if inside polygon
    return point_in_polygon(x, y, red_tiles, n);
}

int compare_candidates(const void* a, const void* b) {
    Candidate* ca = (Candidate*)a;
    Candidate* cb = (Candidate*)b;
    if (ca->area > cb->area) return -1;
    if (ca->area < cb->area) return 1;
    return 0;
}

void solve(char* content, char* part1_result, char* part2_result) {
    // Parse coordinates
    PointArray red_tiles = {NULL, 0, 100};
    red_tiles.points = (Point*)malloc(red_tiles.capacity * sizeof(Point));
    
    char* line = strtok(content, "\n");
    while (line != NULL) {
        while (*line == ' ') line++;
        char* end = line + strlen(line) - 1;
        while (end > line && *end == ' ') *end-- = '\0';
        
        if (strlen(line) > 0 && strchr(line, ',') != NULL) {
            char* comma = strchr(line, ',');
            *comma = '\0';
            long long x = atoll(line);
            long long y = atoll(comma + 1);
            
            if (red_tiles.count >= red_tiles.capacity) {
                red_tiles.capacity *= 2;
                red_tiles.points = (Point*)realloc(red_tiles.points, red_tiles.capacity * sizeof(Point));
            }
            red_tiles.points[red_tiles.count].x = x;
            red_tiles.points[red_tiles.count].y = y;
            red_tiles.count++;
        }
        line = strtok(NULL, "\n");
    }
    
    if (red_tiles.count < 2) {
        strcpy(part1_result, "0");
        strcpy(part2_result, "0");
        free(red_tiles.points);
        return;
    }
    
    // Part 1: Find largest rectangle area
    long long max_area_part1 = 0;
    for (int i = 0; i < red_tiles.count; i++) {
        for (int j = i + 1; j < red_tiles.count; j++) {
            long long x1 = red_tiles.points[i].x;
            long long y1 = red_tiles.points[i].y;
            long long x2 = red_tiles.points[j].x;
            long long y2 = red_tiles.points[j].y;
            long long width = llabs(x1 - x2) + 1;
            long long height = llabs(y1 - y2) + 1;
            long long area = width * height;
            if (area > max_area_part1) {
                max_area_part1 = area;
            }
        }
    }
    
    sprintf(part1_result, "%lld", max_area_part1);
    
    // Part 2: Build boundary set
    PointArray boundary = {NULL, 0, 1000};
    boundary.points = (Point*)malloc(boundary.capacity * sizeof(Point));
    
    // Add red tiles
    for (int i = 0; i < red_tiles.count; i++) {
        if (boundary.count >= boundary.capacity) {
            boundary.capacity *= 2;
            boundary.points = (Point*)realloc(boundary.points, boundary.capacity * sizeof(Point));
        }
        boundary.points[boundary.count++] = red_tiles.points[i];
    }
    
    // Connect consecutive red tiles
    for (int i = 0; i < red_tiles.count; i++) {
        Point p1 = red_tiles.points[i];
        Point p2 = red_tiles.points[(i + 1) % red_tiles.count];
        
        if (p1.x == p2.x) {
            long long start_y = (p1.y < p2.y) ? p1.y : p2.y;
            long long end_y = (p1.y > p2.y) ? p1.y : p2.y;
            for (long long y = start_y; y <= end_y; y++) {
                if (boundary.count >= boundary.capacity) {
                    boundary.capacity *= 2;
                    boundary.points = (Point*)realloc(boundary.points, boundary.capacity * sizeof(Point));
                }
                boundary.points[boundary.count].x = p1.x;
                boundary.points[boundary.count].y = y;
                boundary.count++;
            }
        } else if (p1.y == p2.y) {
            long long start_x = (p1.x < p2.x) ? p1.x : p2.x;
            long long end_x = (p1.x > p2.x) ? p1.x : p2.x;
            for (long long x = start_x; x <= end_x; x++) {
                if (boundary.count >= boundary.capacity) {
                    boundary.capacity *= 2;
                    boundary.points = (Point*)realloc(boundary.points, boundary.capacity * sizeof(Point));
                }
                boundary.points[boundary.count].x = x;
                boundary.points[boundary.count].y = p1.y;
                boundary.count++;
            }
        }
    }
    
    // Generate candidates sorted by area descending
    Candidate* candidates = (Candidate*)malloc(red_tiles.count * red_tiles.count * sizeof(Candidate));
    int candidate_count = 0;
    
    for (int i = 0; i < red_tiles.count; i++) {
        for (int j = i + 1; j < red_tiles.count; j++) {
            long long x1 = red_tiles.points[i].x;
            long long y1 = red_tiles.points[i].y;
            long long x2 = red_tiles.points[j].x;
            long long y2 = red_tiles.points[j].y;
            long long min_x = (x1 < x2) ? x1 : x2;
            long long max_x = (x1 > x2) ? x1 : x2;
            long long min_y = (y1 < y2) ? y1 : y2;
            long long max_y = (y1 > y2) ? y1 : y2;
            long long area = (max_x - min_x + 1) * (max_y - min_y + 1);
            
            candidates[candidate_count].min_x = min_x;
            candidates[candidate_count].max_x = max_x;
            candidates[candidate_count].min_y = min_y;
            candidates[candidate_count].max_y = max_y;
            candidates[candidate_count].area = area;
            candidate_count++;
        }
    }
    
    qsort(candidates, candidate_count, sizeof(Candidate), compare_candidates);
    
    // Check candidates - simplified: check if all corners and sample points are valid
    long long max_area_part2 = 0;
    for (int i = 0; i < candidate_count; i++) {
        if (candidates[i].area <= max_area_part2) {
            break;
        }
        
        long long min_x = candidates[i].min_x;
        long long max_x = candidates[i].max_x;
        long long min_y = candidates[i].min_y;
        long long max_y = candidates[i].max_y;
        
        // Check corners and a sample of interior points
        int all_valid = 1;
        // Check corners
        Point corners[] = {{min_x, min_y}, {min_x, max_y}, {max_x, min_y}, {max_x, max_y}};
        for (int c = 0; c < 4; c++) {
            if (!is_valid_tile(corners[c].x, corners[c].y, red_tiles.points, red_tiles.count,
                              boundary.points, boundary.count)) {
                all_valid = 0;
                break;
            }
        }
        
        // Check all points in rectangle
        if (all_valid) {
            for (long long rx = min_x; rx <= max_x; rx++) {
                for (long long ry = min_y; ry <= max_y; ry++) {
                    if (!is_valid_tile(rx, ry, red_tiles.points, red_tiles.count,
                                      boundary.points, boundary.count)) {
                        all_valid = 0;
                        break;
                    }
                }
                if (!all_valid) break;
            }
        }
        
        if (all_valid) {
            max_area_part2 = candidates[i].area;
            break;
        }
    }
    
    sprintf(part2_result, "%lld", max_area_part2);
    
    free(red_tiles.points);
    free(boundary.points);
    free(candidates);
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
    
    char* content = (char*)malloc(size + 1);
    fread(content, 1, size, file);
    content[size] = '\0';
    fclose(file);
    
    char part1_result[100];
    char part2_result[100];
    
    solve(content, part1_result, part2_result);
    
    printf("Part 1: %s\n", part1_result);
    printf("Part 2: %s\n", part2_result);
    
    free(content);
    return 0;
}
