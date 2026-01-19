def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 7 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    if not lines:
        return "0", "0"

    grid = [list(line) for line in lines]
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0

    # Find starting position S
    start_row, start_col = -1, -1
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 'S':
                start_row, start_col = r, c
                break
        if start_row != -1:
            break

    if start_row == -1:
        return "0", "0"

    # Part 1: Count total splits
    split_count = 0
    # Track active beam columns per row (using a set for each row)
    active_beams = set([start_col])  # Start with beam at S column

    # Process each row starting from the row after S
    for r in range(start_row + 1, rows):
        next_beams: set[int] = set()
        for col in active_beams:
            if grid[r][col] == '.':
                # Beam continues down
                next_beams.add(col)
            elif grid[r][col] == '^':
                # Beam splits
                split_count += 1
                # Add beams to left and right
                if col - 1 >= 0:
                    next_beams.add(col - 1)
                if col + 1 < cols:
                    next_beams.add(col + 1)
        active_beams = next_beams

    # Part 2: Count beams reaching bottom row
    # Use a 2D count matrix to track beam counts per cell
    beam_counts = [[0] * cols for _ in range(rows)]
    beam_counts[start_row][start_col] = 1  # Start with 1 beam at S

    # Process each row starting from the row after S
    for r in range(start_row + 1, rows):
        for c in range(cols):
            prev_count = beam_counts[r - 1][c]
            if prev_count > 0:
                if grid[r][c] == '.':
                    # Beam continues down
                    beam_counts[r][c] += prev_count
                elif grid[r][c] == '^':
                    # Beam splits into left and right
                    if c - 1 >= 0:
                        beam_counts[r][c - 1] += prev_count
                    if c + 1 < cols:
                        beam_counts[r][c + 1] += prev_count

    # Sum all beams in bottom row
    bottom_beam_count = sum(beam_counts[rows - 1])

    return str(split_count), str(bottom_beam_count)


if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
