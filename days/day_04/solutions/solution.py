from get_input import read_input_raw  # type: ignore
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve(
).parent.parent.parent.parent / 'utilities' / 'python'))


def count_neighbors(grid: list[str], i: int, j: int, rows: int, cols: int) -> int:
    """Count neighbors that are '@'."""
    neighbor_count = 0
    for di in [-1, 0, 1]:
        for dj in [-1, 0, 1]:
            if di == 0 and dj == 0:
                continue
            ni, nj = i + di, j + dj
            if 0 <= ni < rows and 0 <= nj < cols and grid[ni][nj] == '@':
                neighbor_count += 1
    return neighbor_count


def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 04 parts 1 and 2."""
    lines = [line for line in input_data.strip().split('\n') if line.strip()]
    rows = len(lines)
    cols = len(lines[0]) if rows > 0 else 0

    # Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    part1_count = 0
    for i in range(rows):
        for j in range(cols):
            if lines[i][j] == '@':
                neighbors = count_neighbors(lines, i, j, rows, cols)
                if neighbors < 4:
                    part1_count += 1

    # Part 2: Iteratively remove accessible rolls until none can be removed
    # Create mutable copy
    grid = [list(line) for line in lines]
    part2_count = 0

    while True:
        to_remove: list[tuple[int, int]] = []
        for i in range(rows):
            for j in range(cols):
                if grid[i][j] == '@':
                    neighbors = count_neighbors(
                        [''.join(row) for row in grid], i, j, rows, cols)
                    if neighbors < 4:
                        to_remove.append((i, j))

        if not to_remove:
            break

        # Remove all marked positions
        for i, j in to_remove:
            grid[i][j] = '.'
        part2_count += len(to_remove)

    return str(part1_count), str(part2_count)


if __name__ == "__main__":
    data: str = read_input_raw("../data/input.txt")  # type: ignore
    part1, part2 = solve(data)  # type: ignore
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
