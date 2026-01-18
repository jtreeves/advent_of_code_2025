from typing import List, Tuple


def find_largest_subsequence(bank: str, n: int) -> int:
    """Find the largest N-digit number by selecting N digits in order from bank.
    
    Uses greedy algorithm: at each position, pick the largest digit available
    such that there are enough digits remaining to complete the sequence.
    """
    if len(bank) < n:
        return 0
    
    result = []
    start = 0
    bank_len = len(bank)
    
    for i in range(n):
        # How many digits we still need after this one
        remaining_needed = n - i - 1
        # The last index we can choose from
        end = bank_len - remaining_needed
        
        # Find the maximum digit in the range [start, end]
        max_digit = bank[start]
        max_pos = start
        for j in range(start + 1, end):
            if bank[j] > max_digit:
                max_digit = bank[j]
                max_pos = j
        
        result.append(max_digit)
        start = max_pos + 1
    
    return int(''.join(result))


def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 3 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    
    part1_sum = 0
    part2_sum = 0
    
    for line in lines:
        bank = line.strip()
        if not bank:
            continue
        
        # Part 1: largest 2-digit number
        part1_sum += find_largest_subsequence(bank, 2)
        
        # Part 2: largest 12-digit number
        if len(bank) >= 12:
            part2_sum += find_largest_subsequence(bank, 12)
    
    return str(part1_sum), str(part2_sum)


if __name__ == "__main__":
    with open("../data/input.txt", "r") as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
