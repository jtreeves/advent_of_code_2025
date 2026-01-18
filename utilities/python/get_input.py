"""Input reading utilities for Python solutions."""

from typing import List
from pathlib import Path


def get_input(day: int) -> List[str]:
    """Read input from input.txt for the given day.

    Args:
        day: Day number (1, 2, 3, etc.)

    Returns:
        List of lines as strings
    """
    path = get_input_path(day)
    return read_input(path)


def get_test_input(day: int, test_num: int) -> List[str]:
    """Read input from test_N.txt for the given day and test number.

    Args:
        day: Day number (1, 2, 3, etc.)
        test_num: Test number (1, 2, 3, etc.)

    Returns:
        List of lines as strings
    """
    path = get_test_input_path(day, test_num)
    return read_input(path)


def get_input_path(day: int) -> str:
    """Return the path to input.txt for the given day.

    Solutions are in days/day_NN/solutions/
    Input is in days/day_NN/data/input.txt
    """
    return "../data/input.txt"


def get_test_input_path(day: int, test_num: int) -> str:
    """Return the path to test_N.txt for the given day and test number."""
    return f"../data/test_{test_num}.txt"


def read_input(file_path: str) -> List[str]:
    """Read input file and return lines as a list."""
    path = Path(file_path).resolve()
    with open(path, 'r') as f:
        return [line.strip() for line in f.readlines()]


def read_input_raw(file_path: str) -> str:
    """Read input file and return raw content."""
    path = Path(file_path).resolve()
    with open(path, 'r') as f:
        return f.read()
