def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 6 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    if not lines:
        return "0", "0"

    # Find maximum line length and pad all lines
    max_len = max(len(line) for line in lines)
    padded_lines = [line.ljust(max_len) for line in lines]

    # Operator row is the last row
    op_row_idx = len(padded_lines) - 1
    op_row = padded_lines[op_row_idx]
    num_rows = padded_lines[:op_row_idx]

    # Part 1: Parse horizontally
    part1_total = 0

    # Find problem boundaries (columns that are all spaces)
    is_space_col: list[bool] = []
    for col in range(max_len):
        all_spaces = all(line[col] == ' ' for line in padded_lines)
        is_space_col.append(all_spaces)

    # Group columns into problems
    problems: list[tuple[int, int, str]] = []
    i = 0
    while i < max_len:
        if not is_space_col[i]:
            # Start of a problem
            start_col = i
            while i < max_len and not is_space_col[i]:
                i += 1
            end_col = i
            # Extract operator for this problem
            # Find the operator in this column range in the op row
            op: str | None = None
            for j in range(start_col, end_col):
                if op_row[j] in ['+', '*']:
                    op = op_row[j]
                    break
            if op:
                problems.append((start_col, end_col, op))
        else:
            i += 1

    # Solve Part 1: Extract numbers horizontally
    for start_col, end_col, op in problems:
        numbers: list[int] = []
        # Extract numbers from each row in this problem area
        for row in num_rows:
            # Extract the substring for this problem
            problem_str = row[start_col:end_col].strip()
            # Split by spaces and parse numbers
            parts = problem_str.split()
            for part in parts:
                try:
                    numbers.append(int(part))
                except ValueError:
                    pass

        # Apply operator
        if numbers:
            if op == '+':
                result = sum(numbers)
            else:  # op == '*'
                result = 1
                for num in numbers:
                    result *= num
            part1_total += result

    # Part 2: Parse vertically (columns, right-to-left)
    # Approach: Use Python's list comprehensions and string operations
    part2_total = 0

    for start_col, end_col, op in problems:
        # Extract column strings (transpose) using list comprehensions
        col_strings: list[str] = []
        for col in range(start_col, end_col):
            if is_space_col[col]:
                continue
            col_str = ''.join(row[col] if col < len(
                row) else ' ' for row in num_rows)
            col_str = col_str.strip()
            if col_str:
                col_strings.append(col_str)

        # Reverse for right-to-left reading
        col_strings = col_strings[::-1]

        # Parse numbers by removing spaces and converting
        numbers_vert: list[int] = []
        for col_str in col_strings:
            digits_only = ''.join(c for c in col_str if c.isdigit())
            if digits_only:
                try:
                    numbers_vert.append(int(digits_only))
                except ValueError:
                    pass

        # Apply operator
        if numbers_vert:
            if op == '+':
                result = sum(numbers_vert)
            else:  # op == '*'
                result = 1
                for num in numbers_vert:
                    result *= num
            part2_total += result

    return str(part1_total), str(part2_total)


if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
