from get_input import read_input_raw  # type: ignore
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve(
).parent.parent.parent.parent / 'utilities' / 'python'))


def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 01 parts 1 and 2."""
    lines = [line.strip()
             for line in input_data.strip().split('\n') if line.strip()]

    # Part 1: Count times dial ends at 0 after a rotation
    position = 50
    count_part1 = 0

    for line in lines:
        direction = line[0]
        distance = int(line[1:])

        # Apply rotation
        if direction == 'L':
            position = ((position - distance) % 100 + 100) % 100
        else:  # direction == 'R'
            position = (position + distance) % 100

        # Check if ended at 0
        if position == 0:
            count_part1 += 1

    # Part 2: Count times dial is at 0 during entire process
    position = 50
    count_part2 = 0

    for line in lines:
        direction = line[0]
        distance = int(line[1:])

        start_pos = position

        # Check each click position during rotation
        for click in range(1, distance + 1):
            if direction == 'L':
                click_pos = ((start_pos - click) % 100 + 100) % 100
            else:  # direction == 'R'
                click_pos = (start_pos + click) % 100

            if click_pos == 0:
                count_part2 += 1

        # Update position after rotation
        if direction == 'L':
            position = ((position - distance) % 100 + 100) % 100
        else:
            position = (position + distance) % 100

    return str(count_part1), str(count_part2)


if __name__ == "__main__":
    data: str = read_input_raw("../data/input.txt")  # type: ignore
    part1, part2 = solve(data)  # type: ignore
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
