from typing import List, Optional

"""Common utility functions for Python solutions."""

def read_input(file_path: str) -> List[str]:
    """Read input file and return lines as a list."""
    with open(file_path, 'r') as f:
        return [line.strip() for line in f.readlines()]

def read_input_raw(file_path: str) -> str:
    """Read input file and return raw content."""
    with open(file_path, 'r') as f:
        return f.read()

def parse_ints(line: str) -> List[int]:
    """Parse integers from a line of text."""
    import re
    return [int(x) for x in re.findall(r'-?\d+', line)]

# Placeholder for additional common utilities
