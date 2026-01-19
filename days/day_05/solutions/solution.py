def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 5 parts 1 and 2."""
    lines = input_data.strip().split('\n')

    # Find blank line separator
    blank_idx = -1
    for i, line in enumerate(lines):
        if not line.strip():
            blank_idx = i
            break

    # Parse ranges (first section)
    ranges: list[tuple[int, int]] = []
    for i in range(blank_idx):
        if lines[i].strip():
            start, end = map(int, lines[i].split('-'))
            ranges.append((start, end))

    # Parse IDs to check (second section)
    ids: list[int] = []
    for i in range(blank_idx + 1, len(lines)):
        if lines[i].strip():
            ids.append(int(lines[i].strip()))

    # Part 1: Count how many IDs fall into any range
    part1_count = 0
    for id_val in ids:
        for start, end in ranges:
            if start <= id_val <= end:
                part1_count += 1
                break

    # Part 2: Merge ranges and count total unique IDs covered
    # Sort ranges by start value
    ranges_sorted: list[tuple[int, int]] = sorted(ranges, key=lambda x: x[0])

    # Merge overlapping/adjacent ranges
    merged: list[list[int]] = []
    if ranges_sorted:
        merged.append(list(ranges_sorted[0]))
        for start, end in ranges_sorted[1:]:
            _, last_end = merged[-1]
            # Check if overlaps or is adjacent (start <= last_end + 1)
            if start <= last_end + 1:
                # Merge: update end to max of both ends
                merged[-1][1] = max(last_end, end)
            else:
                # No overlap, add as new range
                merged.append([start, end])

    # Calculate total unique IDs covered
    part2_total = sum(end - start + 1 for start, end in merged)

    return str(part1_count), str(part2_total)


if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
