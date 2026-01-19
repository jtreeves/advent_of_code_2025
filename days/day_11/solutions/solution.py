def solve(input_data: str) -> tuple[str, str]:
    """Solve Day 11 parts 1 and 2."""
    lines = input_data.strip().split('\n')

    # Build graph: device -> list of outputs
    graph: dict[str, list[str]] = {}
    for line in lines:
        if not line.strip():
            continue
        parts = line.split(':')
        if len(parts) != 2:
            continue
        device = parts[0].strip()
        outputs_str = parts[1].strip()
        outputs = outputs_str.split() if outputs_str else []
        graph[device] = outputs

    # Part 1: Count paths from "you" to "out"
    def count_paths_part1(node: str, memo: dict[str, int]) -> int:
        if node == "out":
            return 1
        if node in memo:
            return memo[node]

        count = 0
        for neighbor in graph.get(node, []):
            count += count_paths_part1(neighbor, memo)

        memo[node] = count
        return count

    part1_count = 0
    if "you" in graph:
        part1_memo: dict[str, int] = {}
        part1_count = count_paths_part1("you", part1_memo)

    # Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    def count_paths_part2(node: str, visited_fft: bool, visited_dac: bool, memo: dict[tuple[str, bool, bool], int]) -> int:
        if node == "out":
            return 1 if (visited_fft and visited_dac) else 0

        key = (node, visited_fft, visited_dac)
        if key in memo:
            return memo[key]

        # Update flags when visiting fft or dac
        new_visited_fft = visited_fft or (node == "fft")
        new_visited_dac = visited_dac or (node == "dac")

        count = 0
        for neighbor in graph.get(node, []):
            count += count_paths_part2(neighbor,
                                       new_visited_fft, new_visited_dac, memo)

        memo[key] = count
        return count

    part2_count = 0
    if "svr" in graph:
        part2_memo: dict[tuple[str, bool, bool], int] = {}
        part2_count = count_paths_part2("svr", False, False, part2_memo)

    return str(part1_count), str(part2_count)


if __name__ == "__main__":
    with open("../data/input.txt", 'r') as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
