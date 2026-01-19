class UnionFind:
    """Union-Find (Disjoint Set Union) data structure."""

    def __init__(self, n: int):
        self.parent = list(range(n))
        self.size = [1] * n
        self.component_count = n

    def find(self, x: int) -> int:
        """Find root with path compression."""
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x: int, y: int) -> bool:
        """Union two components. Returns True if they were separate."""
        root_x = self.find(x)
        root_y = self.find(y)
        if root_x == root_y:
            return False

        # Union by size
        if self.size[root_x] < self.size[root_y]:
            root_x, root_y = root_y, root_x

        self.parent[root_y] = root_x
        self.size[root_x] += self.size[root_y]
        self.component_count -= 1
        return True


def parse_coordinates(lines: list[str]) -> list[tuple[int, int, int]]:
    """Parse x,y,z coordinates from input lines."""
    coords: list[tuple[int, int, int]] = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        parts = line.split(',')
        x, y, z = int(parts[0]), int(parts[1]), int(parts[2])
        coords.append((x, y, z))
    return coords


def squared_distance(p1: tuple[int, int, int], p2: tuple[int, int, int]) -> int:
    """Compute squared Euclidean distance between two 3D points."""
    dx = p2[0] - p1[0]
    dy = p2[1] - p1[1]
    dz = p2[2] - p1[2]
    return dx * dx + dy * dy + dz * dz


def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 8 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    coords = parse_coordinates(lines)

    n = len(coords)
    if n == 0:
        return "0", "0"

    # Generate all pairs with squared distances
    pairs: list[tuple[int, int, int]] = []
    for i in range(n):
        for j in range(i + 1, n):
            dist_sq = squared_distance(coords[i], coords[j])
            pairs.append((i, j, dist_sq))

    # Sort by distance
    pairs.sort(key=lambda x: x[2])

    # Part 1: Connect first 1000 pairs
    uf1 = UnionFind(n)
    connections_made = 0
    for i, j, _ in pairs:
        if connections_made >= 1000:
            break
        uf1.union(i, j)  # Counts even if already connected
        connections_made += 1

    # Get component sizes
    component_sizes: dict[int, int] = {}
    for i in range(n):
        root = uf1.find(i)
        component_sizes[root] = uf1.size[root]

    sizes = sorted(component_sizes.values(), reverse=True)
    part1 = sizes[0] * sizes[1] * sizes[2] if len(sizes) >= 3 else 0

    # Part 2: Connect until all in one circuit
    uf2 = UnionFind(n)
    final_pair: tuple[int, int] | None = None
    for i, j, _ in pairs:
        if uf2.component_count == 1:
            break
        if uf2.union(i, j):
            if uf2.component_count == 1:
                final_pair = (i, j)
                break

    part2: int = 0
    if final_pair:
        i, j = final_pair
        part2 = coords[i][0] * coords[j][0]

    return str(part1), str(part2)


if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        input_data = f.read()
    part1, part2 = solve(input_data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
