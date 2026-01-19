#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DEVICE_NAME 64
#define HASH_SIZE 1000
#define MAX_NEIGHBORS 100

// Graph structure
typedef struct Node {
    char name[MAX_DEVICE_NAME];
    char **neighbors;
    int neighbor_count;
    struct Node *next;
} Node;

typedef struct {
    Node **buckets;
    int bucket_count;
} Graph;

// Memo entry for Part 1
typedef struct MemoEntry1 {
    char key[MAX_DEVICE_NAME];
    long long value;
    struct MemoEntry1 *next;
} MemoEntry1;

typedef struct {
    MemoEntry1 **buckets;
    int bucket_count;
} Memo1;

// Memo entry for Part 2
typedef struct MemoKey2 {
    char node[MAX_DEVICE_NAME];
    int visited_fft;
    int visited_dac;
} MemoKey2;

typedef struct MemoEntry2 {
    MemoKey2 key;
    long long value;
    struct MemoEntry2 *next;
} MemoEntry2;

typedef struct {
    MemoEntry2 **buckets;
    int bucket_count;
} Memo2;

// Hash function for strings
int hash_string(const char *str, int size) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
    }
    return hash % size;
}

// Graph operations
Graph* create_graph(int bucket_count) {
    Graph *g = (Graph*)malloc(sizeof(Graph));
    g->buckets = (Node**)calloc(bucket_count, sizeof(Node*));
    g->bucket_count = bucket_count;
    return g;
}

Node* find_node(Graph *g, const char *name) {
    int idx = hash_string(name, g->bucket_count);
    Node *node = g->buckets[idx];
    while (node) {
        if (strcmp(node->name, name) == 0) {
            return node;
        }
        node = node->next;
    }
    return NULL;
}

void add_node(Graph *g, const char *name) {
    if (find_node(g, name)) return;
    int idx = hash_string(name, g->bucket_count);
    Node *node = (Node*)malloc(sizeof(Node));
    strncpy(node->name, name, MAX_DEVICE_NAME - 1);
    node->name[MAX_DEVICE_NAME - 1] = '\0';
    node->neighbors = NULL;
    node->neighbor_count = 0;
    node->next = g->buckets[idx];
    g->buckets[idx] = node;
}

void add_edge(Graph *g, const char *from, const char *to) {
    Node *node = find_node(g, from);
    if (!node) {
        add_node(g, from);
        node = find_node(g, from);
    }
    node->neighbors = (char**)realloc(node->neighbors, (node->neighbor_count + 1) * sizeof(char*));
    node->neighbors[node->neighbor_count] = (char*)malloc(strlen(to) + 1);
    strcpy(node->neighbors[node->neighbor_count], to);
    node->neighbor_count++;
    // Ensure 'to' node exists
    add_node(g, to);
}

// Memo1 operations
Memo1* create_memo1(int bucket_count) {
    Memo1 *m = (Memo1*)malloc(sizeof(Memo1));
    m->buckets = (MemoEntry1**)calloc(bucket_count, sizeof(MemoEntry1*));
    m->bucket_count = bucket_count;
    return m;
}

long long get_memo1(Memo1 *m, const char *key) {
    int idx = hash_string(key, m->bucket_count);
    MemoEntry1 *entry = m->buckets[idx];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            return entry->value;
        }
        entry = entry->next;
    }
    return -1; // Not found
}

void set_memo1(Memo1 *m, const char *key, long long value) {
    int idx = hash_string(key, m->bucket_count);
    MemoEntry1 *entry = m->buckets[idx];
    while (entry) {
        if (strcmp(entry->key, key) == 0) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }
    // Create new entry
    entry = (MemoEntry1*)malloc(sizeof(MemoEntry1));
    strncpy(entry->key, key, MAX_DEVICE_NAME - 1);
    entry->key[MAX_DEVICE_NAME - 1] = '\0';
    entry->value = value;
    entry->next = m->buckets[idx];
    m->buckets[idx] = entry;
}

// Memo2 operations
int hash_key2(MemoKey2 *key, int size) {
    return (hash_string(key->node, size) + key->visited_fft * 31 + key->visited_dac * 31 * 31) % size;
}

int key2_equal(MemoKey2 *k1, MemoKey2 *k2) {
    return strcmp(k1->node, k2->node) == 0 && k1->visited_fft == k2->visited_fft && k1->visited_dac == k2->visited_dac;
}

Memo2* create_memo2(int bucket_count) {
    Memo2 *m = (Memo2*)malloc(sizeof(Memo2));
    m->buckets = (MemoEntry2**)calloc(bucket_count, sizeof(MemoEntry2*));
    m->bucket_count = bucket_count;
    return m;
}

long long get_memo2(Memo2 *m, MemoKey2 *key) {
    int idx = hash_key2(key, m->bucket_count);
    MemoEntry2 *entry = m->buckets[idx];
    while (entry) {
        if (key2_equal(&entry->key, key)) {
            return entry->value;
        }
        entry = entry->next;
    }
    return -1; // Not found
}

void set_memo2(Memo2 *m, MemoKey2 *key, long long value) {
    int idx = hash_key2(key, m->bucket_count);
    MemoEntry2 *entry = m->buckets[idx];
    while (entry) {
        if (key2_equal(&entry->key, key)) {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }
    // Create new entry
    entry = (MemoEntry2*)malloc(sizeof(MemoEntry2));
    entry->key = *key;
    entry->value = value;
    entry->next = m->buckets[idx];
    m->buckets[idx] = entry;
}

// Part 1: Count paths from "you" to "out"
long long count_paths_part1(const char *node, Graph *g, Memo1 *memo) {
    if (strcmp(node, "out") == 0) {
        return 1;
    }
    
    long long memo_val = get_memo1(memo, node);
    if (memo_val >= 0) {
        return memo_val;
    }
    
    long long count = 0;
    Node *n = find_node(g, node);
    if (n) {
        for (int i = 0; i < n->neighbor_count; i++) {
            count += count_paths_part1(n->neighbors[i], g, memo);
        }
    }
    
    set_memo1(memo, node, count);
    return count;
}

// Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
long long count_paths_part2(const char *node, int visited_fft, int visited_dac, Graph *g, Memo2 *memo) {
    if (strcmp(node, "out") == 0) {
        return (visited_fft && visited_dac) ? 1 : 0;
    }
    
    MemoKey2 key;
    strncpy(key.node, node, MAX_DEVICE_NAME - 1);
    key.node[MAX_DEVICE_NAME - 1] = '\0';
    key.visited_fft = visited_fft;
    key.visited_dac = visited_dac;
    
    long long memo_val = get_memo2(memo, &key);
    if (memo_val >= 0) {
        return memo_val;
    }
    
    int new_visited_fft = visited_fft || (strcmp(node, "fft") == 0);
    int new_visited_dac = visited_dac || (strcmp(node, "dac") == 0);
    
    long long count = 0;
    Node *n = find_node(g, node);
    if (n) {
        for (int i = 0; i < n->neighbor_count; i++) {
            count += count_paths_part2(n->neighbors[i], new_visited_fft, new_visited_dac, g, memo);
        }
    }
    
    set_memo2(memo, &key, count);
    return count;
}

void parse_graph(const char *input_data, Graph *g) {
    char *data = strdup(input_data);
    char *line = strtok(data, "\n");
    while (line) {
        char *colon = strchr(line, ':');
        if (colon) {
            *colon = '\0';
            char *device = line;
            char *outputs_str = colon + 1;
            
            // Trim whitespace
            while (*device == ' ' || *device == '\t') device++;
            while (*outputs_str == ' ' || *outputs_str == '\t') outputs_str++;
            
            // Parse outputs manually (can't nest strtok)
            char *start = outputs_str;
            while (*start) {
                // Skip spaces
                while (*start == ' ' || *start == '\t') start++;
                if (!*start) break;
                
                // Find end of token
                char *end = start;
                while (*end && *end != ' ' && *end != '\t') end++;
                
                // Extract token
                int len = end - start;
                if (len > 0) {
                    char token[MAX_DEVICE_NAME];
                    strncpy(token, start, len);
                    token[len] = '\0';
                    add_edge(g, device, token);
                }
                start = end;
            }
        }
        line = strtok(NULL, "\n");
    }
    free(data);
}

int main() {
    FILE *file = fopen("../data/input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *input_data = (char*)malloc(size + 1);
    fread(input_data, 1, size, file);
    input_data[size] = '\0';
    fclose(file);
    
    Graph *graph = create_graph(HASH_SIZE);
    parse_graph(input_data, graph);
    free(input_data);
    
    // Part 1
    long long part1_count = 0;
    if (find_node(graph, "you")) {
        Memo1 *part1_memo = create_memo1(HASH_SIZE);
        part1_count = count_paths_part1("you", graph, part1_memo);
        // Clean up memo would go here if needed
    }
    
    // Part 2
    long long part2_count = 0;
    if (find_node(graph, "svr")) {
        Memo2 *part2_memo = create_memo2(HASH_SIZE);
        part2_count = count_paths_part2("svr", 0, 0, graph, part2_memo);
        // Clean up memo would go here if needed
    }
    
    printf("Part 1: %lld\n", part1_count);
    printf("Part 2: %lld\n", part2_count);
    
    return 0;
}
