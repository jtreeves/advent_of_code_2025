from typing import List, Tuple


def is_invalid_part1(id_str: str) -> bool:
    """Check if ID is invalid for Part 1: exactly two identical sequences."""
    n = len(id_str)
    # Must be even length to have exactly two sequences
    if n % 2 != 0:
        return False
    # Split in half and check if halves are identical
    half = n // 2
    return id_str[:half] == id_str[half:]


def is_invalid_part2(id_str: str) -> bool:
    """Check if ID is invalid for Part 2: sequence repeated 2+ times."""
    n = len(id_str)
    # Check all possible divisors k >= 2
    for k in range(2, n + 1):
        if n % k == 0:
            seq_len = n // k
            pattern = id_str[:seq_len]
            # Check if the pattern repeats k times
            if id_str == pattern * k:
                return True
    return False


def parse_ranges(line: str) -> List[Tuple[int, int]]:
    """Parse a line of comma-separated ranges like 'start-end, start-end'."""
    ranges: List[Tuple[int, int]] = []
    for range_str in line.split(','):
        range_str = range_str.strip()
        if '-' in range_str:
            start, end = map(int, range_str.split('-'))
            ranges.append((start, end))
    return ranges


def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 2 parts 1 and 2."""
    lines = input_data.strip().split('\n')

    part1_sum = 0
    part2_sum = 0

    for line in lines:
        if not line.strip():
            continue
        ranges_list = parse_ranges(line)

        for start, end in ranges_list:
            # Iterate through all IDs in the range (inclusive)
            for num in range(start, end + 1):
                id_str = str(num)

                # Part 1: exactly two identical sequences
                if is_invalid_part1(id_str):
                    part1_sum += num

                # Part 2: sequence repeated 2+ times (includes Part 1 cases)
                if is_invalid_part2(id_str):
                    part2_sum += num

    return str(part1_sum), str(part2_sum)


if __name__ == "__main__":
    with open("../data/input.txt", "r") as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
