from typing import List, Tuple, Set

def solve(input_data: str) -> Tuple[str, str]:
    """Solve Day 9 parts 1 and 2."""
    lines = input_data.strip().split('\n')
    lines = [line.strip() for line in lines if line.strip()]
    
    # Parse coordinates
    red_tiles = []
    for line in lines:
        if ',' in line:
            parts = line.split(',')
            x = int(parts[0])
            y = int(parts[1])
            red_tiles.append((x, y))
    
    if len(red_tiles) < 2:
        return "0", "0"
    
    # Part 1: Find largest rectangle area using any two red tiles as corners
    max_area_part1 = 0
    for i in range(len(red_tiles)):
        for j in range(i + 1, len(red_tiles)):
            x1, y1 = red_tiles[i]
            x2, y2 = red_tiles[j]
            width = abs(x1 - x2) + 1
            height = abs(y1 - y2) + 1
            area = width * height
            max_area_part1 = max(max_area_part1, area)
    
    # Part 2: Coordinate compression + flood-fill + prefix sums
    # Collect all x and y coordinates for compression
    all_x_set = set()
    all_y_set = set()
    for x, y in red_tiles:
        all_x_set.add(x)
        all_x_set.add(x + 1)
        all_y_set.add(y)
        all_y_set.add(y + 1)
    
    all_x = sorted(all_x_set)
    all_y = sorted(all_y_set)
    
    # Create compression maps
    x_to_cx = {x: i for i, x in enumerate(all_x)}
    y_to_cy = {y: i for i, y in enumerate(all_y)}
    
    width = len(all_x)
    height = len(all_y)
    
    # Build grid: False = outside/invalid, True = inside/valid
    grid = [[False] * height for _ in range(width)]
    
    # Mark boundary (red + green tiles)
    for x, y in red_tiles:
        if x in x_to_cx and y in y_to_cy:
            grid[x_to_cx[x]][y_to_cy[y]] = True
    
    # Connect consecutive red tiles with green tiles
    for i in range(len(red_tiles)):
        x1, y1 = red_tiles[i]
        x2, y2 = red_tiles[(i + 1) % len(red_tiles)]
        
        if x1 == x2:
            start_y = min(y1, y2)
            end_y = max(y1, y2)
            for y in range(start_y, end_y + 1):
                if x1 in x_to_cx and y in y_to_cy:
                    grid[x_to_cx[x1]][y_to_cy[y]] = True
        elif y1 == y2:
            start_x = min(x1, x2)
            end_x = max(x1, x2)
            for x in range(start_x, end_x + 1):
                if x in x_to_cx and y1 in y_to_cy:
                    grid[x_to_cx[x]][y_to_cy[y1]] = True
    
    # Point-in-polygon check
    def point_in_polygon(px: int, py: int) -> bool:
        inside = False
        for i in range(len(red_tiles)):
            x1, y1 = red_tiles[i]
            x2, y2 = red_tiles[(i + 1) % len(red_tiles)]
            if (y1 > py) != (y2 > py):
                intersect_x = (py - y1) * (x2 - x1) / (y2 - y1) + x1 if y2 != y1 else px
                if px < intersect_x:
                    inside = not inside
        return inside
    
    # Flood fill interior
    found_interior = False
    for cx in range(width):
        for cy in range(height):
            if not grid[cx][cy]:
                orig_x = all_x[cx]
                orig_y = all_y[cy]
                if point_in_polygon(orig_x, orig_y):
                    # Flood fill from this point
                    stack = [(cx, cy)]
                    while stack:
                        x, y = stack.pop()
                        if x >= width or y >= height or grid[x][y]:
                            continue
                        orig_x = all_x[x]
                        orig_y = all_y[y]
                        if point_in_polygon(orig_x, orig_y):
                            grid[x][y] = True
                            if x > 0:
                                stack.append((x - 1, y))
                            if x + 1 < width:
                                stack.append((x + 1, y))
                            if y > 0:
                                stack.append((x, y - 1))
                            if y + 1 < height:
                                stack.append((x, y + 1))
                    found_interior = True
                    break
        if found_interior:
            break
    
    # Build 2D prefix sum for O(1) rectangle queries
    prefix = [[0] * (height + 1) for _ in range(width + 1)]
    for cx in range(width):
        for cy in range(height):
            prefix[cx + 1][cy + 1] = (prefix[cx][cy + 1] + prefix[cx + 1][cy] 
                                      - prefix[cx][cy] + (1 if grid[cx][cy] else 0))
    
    # Helper: get sum in rectangle [cx1, cx2] x [cy1, cy2] (inclusive)
    def rect_sum(cx1: int, cx2: int, cy1: int, cy2: int) -> int:
        return (prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] 
                - prefix[cx2 + 1][cy1] + prefix[cx1][cy1])
    
    # Generate candidates sorted by area descending
    candidates = []
    for i in range(len(red_tiles)):
        for j in range(i + 1, len(red_tiles)):
            x1, y1 = red_tiles[i]
            x2, y2 = red_tiles[j]
            min_x = min(x1, x2)
            max_x = max(x1, x2)
            min_y = min(y1, y2)
            max_y = max(y1, y2)
            area = (max_x - min_x + 1) * (max_y - min_y + 1)
            
            if (min_x in x_to_cx and max_x in x_to_cx and 
                min_y in y_to_cy and max_y in y_to_cy):
                cx1 = x_to_cx[min_x]
                cx2 = x_to_cx[max_x]
                cy1 = y_to_cy[min_y]
                cy2 = y_to_cy[max_y]
                candidates.append((min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2))
    
    candidates.sort(key=lambda x: x[4], reverse=True)
    
    # Check candidates in descending area order
    max_area_part2 = 0
    for min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2 in candidates:
        if area <= max_area_part2:
            break
        
        # Check if rectangle is fully contained using prefix sum
        valid_count = rect_sum(cx1, cx2, cy1, cy2)
        expected_cells = (cx2 - cx1 + 1) * (cy2 - cy1 + 1)
        
        if valid_count == expected_cells:
            # Check corners to ensure rectangle doesn't extend beyond valid regions
            all_valid = True
            for x, y in [(min_x, min_y), (min_x, max_y), (max_x, min_y), (max_x, max_y)]:
                if x in x_to_cx and y in y_to_cy:
                    cx, cy = x_to_cx[x], y_to_cy[y]
                    if not grid[cx][cy]:
                        all_valid = False
                        break
                else:
                    if not point_in_polygon(x, y):
                        all_valid = False
                        break
            
            if all_valid:
                max_area_part2 = area
                break
    
    return str(max_area_part1), str(max_area_part2)


if __name__ == "__main__":
    with open("../data/input.txt", "r") as f:
        data = f.read()
    part1, part2 = solve(data)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
