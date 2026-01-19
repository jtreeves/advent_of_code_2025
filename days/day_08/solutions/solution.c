#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    long long x, y, z;
} Coord;

typedef struct {
    int i, j;
    long long dist_sq;
} Pair;

typedef struct {
    int* parent;
    long long* size;
    int component_count;
    int n;
} UnionFind;

UnionFind* uf_create(int n) {
    UnionFind* uf = (UnionFind*)malloc(sizeof(UnionFind));
    uf->parent = (int*)malloc(n * sizeof(int));
    uf->size = (long long*)malloc(n * sizeof(long long));
    uf->n = n;
    uf->component_count = n;
    for (int i = 0; i < n; i++) {
        uf->parent[i] = i;
        uf->size[i] = 1;
    }
    return uf;
}

void uf_free(UnionFind* uf) {
    free(uf->parent);
    free(uf->size);
    free(uf);
}

int uf_find(UnionFind* uf, int x) {
    if (uf->parent[x] != x) {
        uf->parent[x] = uf_find(uf, uf->parent[x]);
    }
    return uf->parent[x];
}

int uf_union(UnionFind* uf, int x, int y) {
    int root_x = uf_find(uf, x);
    int root_y = uf_find(uf, y);
    if (root_x == root_y) {
        return 0;
    }
    if (uf->size[root_x] < uf->size[root_y]) {
        int temp = root_x;
        root_x = root_y;
        root_y = temp;
    }
    uf->parent[root_y] = root_x;
    uf->size[root_x] += uf->size[root_y];
    uf->component_count--;
    return 1;
}

long long squared_distance(Coord p1, Coord p2) {
    long long dx = p2.x - p1.x;
    long long dy = p2.y - p1.y;
    long long dz = p2.z - p1.z;
    return dx * dx + dy * dy + dz * dz;
}

int compare_pairs(const void* a, const void* b) {
    Pair* pa = (Pair*)a;
    Pair* pb = (Pair*)b;
    if (pa->dist_sq < pb->dist_sq) return -1;
    if (pa->dist_sq > pb->dist_sq) return 1;
    return 0;
}

int compare_long_long(const void* a, const void* b) {
    long long* la = (long long*)a;
    long long* lb = (long long*)b;
    if (*la > *lb) return -1;
    if (*la < *lb) return 1;
    return 0;
}

int main() {
    FILE* file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[256];
    Coord* coords = NULL;
    int capacity = 1000;
    int n = 0;
    coords = (Coord*)malloc(capacity * sizeof(Coord));

    while (fgets(line, sizeof(line), file)) {
        if (strlen(line) == 0 || line[0] == '\n') continue;
        if (n >= capacity) {
            capacity *= 2;
            coords = (Coord*)realloc(coords, capacity * sizeof(Coord));
        }
        long long x, y, z;
        if (sscanf(line, "%lld,%lld,%lld", &x, &y, &z) == 3) {
            coords[n].x = x;
            coords[n].y = y;
            coords[n].z = z;
            n++;
        }
    }
    fclose(file);

    if (n == 0) {
        printf("Part 1: 0\n");
        printf("Part 2: 0\n");
        free(coords);
        return 0;
    }

    // Generate all pairs
    Pair* pairs = NULL;
    int pair_count = 0;
    int pair_capacity = n * n;
    pairs = (Pair*)malloc(pair_capacity * sizeof(Pair));
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            long long dist_sq = squared_distance(coords[i], coords[j]);
            pairs[pair_count].i = i;
            pairs[pair_count].j = j;
            pairs[pair_count].dist_sq = dist_sq;
            pair_count++;
        }
    }

    // Sort pairs by distance
    qsort(pairs, pair_count, sizeof(Pair), compare_pairs);

    // Part 1: Connect first 1000 pairs
    UnionFind* uf1 = uf_create(n);
    int connections_made = 0;
    for (int i = 0; i < pair_count && connections_made < 1000; i++) {
        uf_union(uf1, pairs[i].i, pairs[i].j);
        connections_made++;
    }

    // Get component sizes
    long long* component_sizes = (long long*)calloc(n, sizeof(long long));
    for (int i = 0; i < n; i++) {
        int root = uf_find(uf1, i);
        component_sizes[root] = uf1->size[root];
    }

    // Count unique component sizes
    int unique_count = 0;
    for (int i = 0; i < n; i++) {
        if (component_sizes[i] > 0) unique_count++;
    }

    long long* sizes = (long long*)malloc(unique_count * sizeof(long long));
    int idx = 0;
    for (int i = 0; i < n; i++) {
        if (component_sizes[i] > 0) {
            sizes[idx++] = component_sizes[i];
        }
    }
    qsort(sizes, unique_count, sizeof(long long), compare_long_long);

    long long part1 = 0;
    if (unique_count >= 3) {
        part1 = sizes[0] * sizes[1] * sizes[2];
    }

    // Part 2: Connect until all in one circuit
    UnionFind* uf2 = uf_create(n);
    Pair* final_pair = NULL;
    for (int i = 0; i < pair_count; i++) {
        if (uf2->component_count == 1) break;
        if (uf_union(uf2, pairs[i].i, pairs[i].j)) {
            if (uf2->component_count == 1) {
                final_pair = &pairs[i];
                break;
            }
        }
    }

    long long part2 = 0;
    if (final_pair != NULL) {
        part2 = coords[final_pair->i].x * coords[final_pair->j].x;
    }

    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);

    uf_free(uf1);
    uf_free(uf2);
    free(coords);
    free(pairs);
    free(component_sizes);
    free(sizes);

    return 0;
}
