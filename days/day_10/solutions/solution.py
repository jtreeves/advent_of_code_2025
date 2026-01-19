from get_input import read_input_raw  # type: ignore
import sys
from pathlib import Path
import re
from typing import List, Tuple, Optional
sys.path.insert(0, str(Path(__file__).resolve(
).parent.parent.parent.parent / 'utilities' / 'python'))


def parse_line(line: str) -> Tuple[List[bool], List[List[int]], List[int]]:
    """Parse a machine line: [pattern] (buttons) {joltages}"""
    # Extract pattern: [.##.]
    pattern_match = re.search(r'\[([.#]+)\]', line)
    if not pattern_match:
        return [], [], []
    pattern_str = pattern_match.group(1)
    target_pattern: list[bool] = [c == '#' for c in pattern_str]

    # Extract buttons: (1,3) (2) etc.
    button_matches = re.findall(r'\(([^)]+)\)', line)
    buttons: list[list[int]] = []
    for btn_str in button_matches:
        if not btn_str.strip():
            buttons.append([])
        else:
            lights = [int(x) for x in btn_str.split(',')]
            buttons.append(lights)

    # Extract joltages: {3,5,4,7}
    joltage_match = re.search(r'\{([^}]+)\}', line)
    joltages: list[int] = []
    if joltage_match:
        joltage_str = joltage_match.group(1)
        joltages = [int(x.strip()) for x in joltage_str.split(',')]

    return target_pattern, buttons, joltages


def gaussian_elimination_gf2(matrix: List[List[bool]], target: List[bool]) -> Optional[int]:
    """
    Solve Ax = b over GF(2) to find minimum weight solution.
    Uses brute force with early termination for practical performance.
    """
    num_buttons = len(matrix)
    num_lights = len(target)

    # Brute force: try all combinations in order of increasing weight
    from itertools import combinations

    # Try all combinations of k buttons pressed, for k from 0 to num_buttons
    for k in range(num_buttons + 1):
        for button_combo in combinations(range(num_buttons), k):
            # Simulate pressing these buttons
            lights = [False] * num_lights  # All start OFF
            for btn_idx in button_combo:
                for light_idx in range(num_lights):
                    if matrix[btn_idx][light_idx]:
                        lights[light_idx] ^= True  # Toggle

            # Check if matches target
            if lights == target:
                return k  # k buttons pressed = minimum weight

    return None  # No solution found


def gcd(a: int, b: int) -> int:
    """Compute GCD using Euclidean algorithm."""
    while b:
        a, b = b, a % b
    return abs(a)


def integer_gaussian_elimination(matrix: List[List[int]], target: List[int]) -> Tuple[List[List[int]], List[int], List[int], List[int]]:
    """
    Perform integer Gaussian elimination on Ax = b using GCD-based elimination.
    Returns: (reduced_matrix, reduced_target, pivot_cols, free_cols)
    where pivot_cols are columns with pivots, free_cols are free variables.
    """
    num_rows = len(matrix)
    num_cols = len(matrix[0]) if matrix else 0

    # Copy matrix and target
    A = [row[:] for row in matrix]
    b = target[:]

    pivot_cols: list[int] = []  # Columns that have pivots
    free_cols: list[int] = []   # Free variables (columns without pivots)
    pivot_row = 0

    # Forward elimination
    for col in range(num_cols):
        # Find a row with non-zero entry in this column, starting from pivot_row
        pivot_idx = None
        for row in range(pivot_row, num_rows):
            if A[row][col] != 0:
                pivot_idx = row
                break

        if pivot_idx is None:
            # No pivot in this column - it's a free variable
            free_cols.append(col)
            continue

        # Swap rows
        if pivot_idx != pivot_row:
            A[pivot_row], A[pivot_idx] = A[pivot_idx], A[pivot_row]
            b[pivot_row], b[pivot_idx] = b[pivot_idx], b[pivot_row]

        pivot_cols.append(col)
        pivot_val = A[pivot_row][col]

        # Eliminate this column in other rows using GCD-based elimination
        for row in range(num_rows):
            if row == pivot_row:
                continue
            if A[row][col] != 0:
                # Use extended Euclidean algorithm approach
                # We want: new_row = A[row] - (A[row][col] / pivot_val) * A[pivot_row]
                # For integers, we compute: new_row = (pivot_val * A[row] - A[row][col] * A[pivot_row]) / gcd
                # Actually, simpler: if pivot_val divides A[row][col], we can subtract directly
                if A[row][col] % pivot_val == 0:
                    factor = A[row][col] // pivot_val
                    A[row] = [A[row][c] - factor * A[pivot_row][c]
                              for c in range(num_cols)]
                    b[row] = b[row] - factor * b[pivot_row]
                else:
                    # Use GCD approach: combine rows to eliminate
                    g = gcd(pivot_val, A[row][col])
                    if g > 0:
                        # new_row = (A[row][col]//g) * A[pivot_row] - (pivot_val//g) * A[row]
                        factor1 = A[row][col] // g
                        factor2 = pivot_val // g
                        new_row = [factor1 * A[pivot_row][c] -
                                   factor2 * A[row][c] for c in range(num_cols)]
                        new_b = factor1 * b[pivot_row] - factor2 * b[row]
                        A[row] = new_row
                        b[row] = new_b

        pivot_row += 1
        if pivot_row >= num_rows:
            break

    # Mark remaining columns as free
    for col in range(num_cols):
        if col not in pivot_cols:
            free_cols.append(col)

    return A, b, pivot_cols, free_cols


def solve_part2_ilp(buttons: List[List[int]], joltages: List[int], target_pattern: List[bool]) -> Optional[int]:
    """
    Solve integer linear programming: Ax = b, minimize sum(x)
    Where A is button incidence matrix, b is joltages, x is button presses.
    Uses "Bifurcate" approach: try all parity solutions, then reduce and recurse.
    Uses Z3 SMT solver as fallback for cases where bifurcate doesn't apply.
    """
    try:
        import z3  # type: ignore
    except ImportError:
        # Fallback to matrix elimination if Z3 not available
        return solve_part2_ilp_fallback(buttons, joltages, target_pattern)

    num_buttons = len(buttons)
    num_lights = len(joltages)

    # Base case: all joltages are 0
    if all(j == 0 for j in joltages):
        return 0

    # Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
    # that achieve the required parity, then divide by 2 and recurse
    # This is more efficient than pure Z3 for many cases
    button_matrix = [[False] * num_lights for _ in range(num_buttons)]
    for btn_idx, btn in enumerate(buttons):
        for light in btn:
            if 0 <= light < num_lights:
                button_matrix[btn_idx][light] = True

    # Required parity: pattern ON = odd, pattern OFF = even
    required_parity = [target_pattern[i] for i in range(num_lights)]

    # Find minimum parity presses needed (Part 1 style)
    parity_toggles = required_parity.copy()
    min_parity_presses = gaussian_elimination_gf2(
        button_matrix, parity_toggles)

    if min_parity_presses is not None:
        # Try all combinations starting from minimum
        from itertools import combinations

        best_result = None

        # Try all combinations - Reddit emphasizes trying ALL parity solutions
        # But limit to reasonable size to avoid exponential explosion
        # Try from minimum up to all buttons, but prune if we have a good solution
        max_parity_to_try = num_buttons

        for k in range(min_parity_presses, max_parity_to_try + 1):
            # Pruning: if we already have a solution and k is getting large, skip
            if best_result is not None and k > best_result:
                break

            for button_combo in combinations(range(num_buttons), k):
                # Simulate pressing these buttons once
                resulting_joltages = list(joltages)
                for btn_idx in button_combo:
                    for light in buttons[btn_idx]:
                        if 0 <= light < num_lights:
                            resulting_joltages[light] += 1

                # Check if parity matches
                resulting_parity = [j % 2 == 1 for j in resulting_joltages]
                if resulting_parity == required_parity:
                    # Check if all remaining are even (can divide by 2)
                    if all(j % 2 == 0 for j in resulting_joltages):
                        # Divide by 2 and recurse
                        halved_joltages = [j // 2 for j in resulting_joltages]
                        remaining_result = solve_part2_ilp(
                            buttons, halved_joltages, target_pattern)
                        if remaining_result is not None:
                            total = k + remaining_result * 2
                            if best_result is None or total < best_result:
                                best_result = total

        if best_result is not None:
            return best_result

    # If no bifurcate solution found, fall back to Z3

    # Create Z3 solver
    solver = z3.Optimize()  # type: ignore

    # Create integer variables for button presses (non-negative)
    # Reddit suggests not using overly restrictive upper bounds
    # Use a reasonable bound: sum of all presses can't exceed sum of joltages
    # But per-button, we can use a looser bound
    total_joltage = sum(joltages) if joltages else 0
    presses = [z3.Int(f'press_{i}')  # type: ignore
               for i in range(num_buttons)]
    for p in presses:
        solver.add(p >= 0)  # type: ignore
        # Upper bound: no single button needs more than total joltage
        solver.add(p <= total_joltage)  # type: ignore

    # Add constraints: for each light, sum of presses of buttons affecting it = joltage
    for light_idx in range(num_lights):
        joltage_sum = z3.Sum([presses[btn_idx] for btn_idx in range(num_buttons)  # type: ignore
                              if light_idx in buttons[btn_idx]])
        solver.add(joltage_sum == joltages[light_idx])  # type: ignore

    # Minimize total presses
    total_presses = z3.Sum(presses)  # type: ignore
    solver.minimize(total_presses)  # type: ignore

    # Solve and verify we got the minimum
    if solver.check() == z3.sat:  # type: ignore
        model = solver.model()  # type: ignore
        result = sum(model[p].as_long() for p in presses)  # type: ignore

        # Verify the solution is correct
        test_joltages = [0] * num_lights
        for btn_idx, btn in enumerate(buttons):
            presses_val = model[presses[btn_idx]].as_long()  # type: ignore
            for light in btn:
                if 0 <= light < num_lights:
                    test_joltages[light] += presses_val

        if test_joltages != joltages:
            # Solution doesn't match - this shouldn't happen but let's be safe
            return None

        return result
    else:
        return None


def solve_part2_ilp_fallback(buttons: List[List[int]], joltages: List[int], target_pattern: List[bool]) -> Optional[int]:
    """
    Fallback: Solve using integer Gaussian elimination when Z3 is not available.
    """
    num_buttons = len(buttons)
    num_lights = len(joltages)

    # Divide-by-2 optimization: if all joltages are even, halve and recurse
    if all(j % 2 == 0 for j in joltages) and any(j > 0 for j in joltages):
        halved_joltages = [j // 2 for j in joltages]
        result = solve_part2_ilp_fallback(
            buttons, halved_joltages, target_pattern)
        if result is not None:
            return result * 2
        return None

    # Build incidence matrix: A[i][j] = 1 if button j affects light i, else 0
    matrix = [[0] * num_buttons for _ in range(num_lights)]
    for btn_idx, btn in enumerate(buttons):
        for light in btn:
            if 0 <= light < num_lights:
                matrix[light][btn_idx] = 1

    # Perform integer Gaussian elimination
    reduced_A, reduced_b, pivot_cols, free_cols = integer_gaussian_elimination(
        matrix, joltages)

    # If no free variables, solve directly by back-substitution
    if not free_cols:
        solution = [0] * num_buttons
        for i in range(len(pivot_cols) - 1, -1, -1):
            if i >= len(reduced_A):
                continue
            col = pivot_cols[i]
            pivot_val = reduced_A[i][col] if i < len(
                reduced_A) and col < len(reduced_A[i]) else 0
            if pivot_val == 0:
                continue

            rhs = reduced_b[i]
            for j in range(i + 1, len(pivot_cols)):
                if j < len(reduced_A[i]):
                    rhs -= reduced_A[i][pivot_cols[j]] * \
                        solution[pivot_cols[j]]

            if rhs % pivot_val != 0:
                return None
            solution[col] = rhs // pivot_val

            if solution[col] < 0:
                return None

        # Verify solution
        test_joltages = [0] * num_lights
        for btn_idx, presses in enumerate(solution):
            for light in buttons[btn_idx]:
                if 0 <= light < num_lights:
                    test_joltages[light] += presses

        if test_joltages == joltages:
            return sum(solution)
        return None

    # There are free variables - search over them
    max_joltage = max(joltages) if joltages else 0
    best = None

    def solve_with_free_vars(free_assignments: List[Tuple[int, int]]) -> Optional[int]:
        solution = [0] * num_buttons
        free_dict = dict(free_assignments)

        for col, val in free_dict.items():
            solution[col] = val

        for i in range(len(pivot_cols) - 1, -1, -1):
            if i >= len(reduced_A):
                continue
            col = pivot_cols[i]
            pivot_val = reduced_A[i][col] if i < len(
                reduced_A) and col < len(reduced_A[i]) else 0
            if pivot_val == 0:
                continue

            rhs = reduced_b[i]
            for j in range(num_buttons):
                if j != col and j < len(reduced_A[i]):
                    rhs -= reduced_A[i][j] * solution[j]

            if rhs % pivot_val != 0:
                return None
            solution[col] = rhs // pivot_val

            if solution[col] < 0:
                return None

        test_joltages = [0] * num_lights
        for btn_idx, presses in enumerate(solution):
            for light in buttons[btn_idx]:
                if 0 <= light < num_lights:
                    test_joltages[light] += presses

        if test_joltages == joltages:
            return sum(solution)
        return None

    def search_free_vars(free_idx: int, assignments: List[Tuple[int, int]], current_sum: int):
        nonlocal best
        if free_idx >= len(free_cols):
            result = solve_with_free_vars(assignments)
            if result is not None:
                if best is None or result < best:
                    best = result
            return

        if best is not None and current_sum >= best:
            return

        col = free_cols[free_idx]
        max_val = max_joltage
        if best is not None:
            max_val = min(max_val, best - current_sum)

        for val in range(max_val + 1):
            new_assignments = assignments + [(col, val)]
            search_free_vars(free_idx + 1, new_assignments, current_sum + val)
            if best is not None and current_sum + val >= best:
                break

    search_free_vars(0, [], 0)
    return best


def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 10 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    if not lines:
        return "0", "0"

    part1_total = 0
    part2_total = 0

    for line in lines:
        if not line.strip():
            continue

        target_pattern, buttons, joltages = parse_line(line)
        if not target_pattern:
            continue

        num_lights = len(target_pattern)

        # Part 1: GF(2) linear system
        # Build incidence matrix: matrix[i][j] = True if button i toggles light j
        button_matrix = [[False] * num_lights for _ in range(len(buttons))]
        for i, btn in enumerate(buttons):
            for light in btn:
                if 0 <= light < num_lights:
                    button_matrix[i][light] = True

        # Target: all start OFF, need to toggle to match pattern
        # Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        required_toggles = target_pattern.copy()

        result = gaussian_elimination_gf2(button_matrix, required_toggles)
        if result is not None:
            part1_total += result

        # Part 2: Integer Linear Programming
        if joltages and len(joltages) == num_lights:
            result2 = solve_part2_ilp(buttons, joltages, target_pattern)
            if result2 is not None:
                part2_total += result2

    return str(part1_total), str(part2_total)


if __name__ == "__main__":
    data: str = read_input_raw("../data/input.txt")  # type: ignore
    part1, part2 = solve(data)  # type: ignore
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
