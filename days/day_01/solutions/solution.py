from get_input import get_input, read_input_raw
from typing import List, Tuple
import sys
sys.path.append('../../../utilities/python')

# Placeholder for Day 01 Python solution


def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 01 parts 1 and 2."""
    print("Day 01 Python placeholder")
    lines = input_data.strip().split('\n')
    print(f"Lines: {lines}")

    # Part 1
    part1_result = "TODO"

    # Part 2
    part2_result = "TODO"

    return part1_result, part2_result


if __name__ == "__main__":
    # Use utility function to get input
    data = read_input_raw("../data/input.txt")
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
