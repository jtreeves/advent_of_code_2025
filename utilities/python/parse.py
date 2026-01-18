"""Parsing utilities for Python solutions."""

from typing import List
import re


def parse_ints(line: str) -> List[int]:
    """Parse integers from a line of text.
    
    Args:
        line: Line of text containing integers
        
    Returns:
        List of integers found in the line
    """
    return [int(x) for x in re.findall(r'-?\d+', line)]
