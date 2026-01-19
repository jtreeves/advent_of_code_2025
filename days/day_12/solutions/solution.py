from typing import Tuple


def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 12 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    if not lines:
        return "0", "N/A"

    # Parse shapes (first 6 shapes, numbered 0-5)
    shape_areas = [0] * 6
    i = 0
    shape_idx = 0

    while i < len(lines) and shape_idx < 6:
        line = lines[i].strip()
        # Check if this is a shape header (format: "number:")
        if line and line.endswith(':') and line[:-1].isdigit():
            shape_num = int(line[:-1])
            if shape_num == shape_idx:
                # Read the next 3 lines for the shape grid
                shape_grid: list[str] = []
                for j in range(3):
                    if i + 1 + j < len(lines):
                        shape_grid.append(lines[i + 1 + j].strip())
                    else:
                        shape_grid.append("")

                # Count '#' characters in the shape
                area = sum(row.count('#') for row in shape_grid)
                shape_areas[shape_idx] = area
                shape_idx += 1
                # Skip shape header + 3 grid lines + empty line (if present)
                i += 4
                continue
        i += 1

    # Find where queries start (skip empty lines after shapes)
    query_start = i
    while query_start < len(lines) and not lines[query_start].strip():
        query_start += 1

    # Parse queries
    possible_count = 0
    for line_idx in range(query_start, len(lines)):
        line = lines[line_idx].strip()
        if not line:
            continue

        # Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        if ':' not in line:
            continue

        parts = line.split(':', 1)
        if len(parts) != 2:
            continue

        # Parse dimensions
        dims = parts[0].strip()
        if 'x' not in dims:
            continue

        dim_parts = dims.split('x')
        if len(dim_parts) != 2:
            continue

        try:
            width = int(dim_parts[0])
            height = int(dim_parts[1])
        except ValueError:
            continue

        # Parse counts
        count_parts = parts[1].strip().split()
        if len(count_parts) != 6:
            continue

        try:
            counts = [int(c) for c in count_parts]
        except ValueError:
            continue

        # Calculate area check
        region_area = width * height
        required_area = sum(shape_areas[j] * counts[j] for j in range(6))

        if required_area <= region_area:
            possible_count += 1

    # Part 2: Final star (no computation needed)
    part2 = "Final star"

    return str(possible_count), part2


if __name__ == "__main__":
    with open("../data/input.txt", "r") as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
