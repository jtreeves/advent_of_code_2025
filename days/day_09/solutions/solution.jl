# Day 9 Julia solution
function solve(input_data::String)::Tuple{String, String}
    lines = split(strip(input_data), '\n')
    lines = filter(!isempty, map(strip, lines))
    
    # Parse coordinates
    red_tiles = Tuple{Int, Int}[]
    for line in lines
        if ',' in line
            parts = split(line, ',')
            if length(parts) == 2
                try
                    x = parse(Int, strip(parts[1]))
                    y = parse(Int, strip(parts[2]))
                    push!(red_tiles, (x, y))
                catch
                    # Skip invalid lines
                end
            end
        end
    end
    
    if length(red_tiles) < 2
        return ("0", "0")
    end
    
    # Part 1: Find largest rectangle area using any two red tiles as corners
    max_area_part1 = 0
    for i in 1:length(red_tiles)
        for j in i+1:length(red_tiles)
            (x1, y1) = red_tiles[i]
            (x2, y2) = red_tiles[j]
            width = abs(x1 - x2) + 1
            height = abs(y1 - y2) + 1
            area = width * height
            max_area_part1 = max(max_area_part1, area)
        end
    end
    
    # Part 2: Coordinate compression + flood-fill + prefix sums
    all_x_set = Set{Int}()
    all_y_set = Set{Int}()
    for (x, y) in red_tiles
        push!(all_x_set, x)
        push!(all_x_set, x + 1)
        push!(all_y_set, y)
        push!(all_y_set, y + 1)
    end
    
    all_x = sort(collect(all_x_set))
    all_y = sort(collect(all_y_set))
    
    x_to_cx = Dict(x => i for (i, x) in enumerate(all_x))
    y_to_cy = Dict(y => i for (i, y) in enumerate(all_y))
    
    width = length(all_x)
    height = length(all_y)
    
    # Build grid
    grid = falses(width, height)
    
    # Mark boundary
    for (x, y) in red_tiles
        if haskey(x_to_cx, x) && haskey(y_to_cy, y)
            grid[x_to_cx[x], y_to_cy[y]] = true
        end
    end
    
    # Connect consecutive red tiles
    for i in 1:length(red_tiles)
        (x1, y1) = red_tiles[i]
        (x2, y2) = red_tiles[mod1(i + 1, length(red_tiles))]
        
        if x1 == x2
            start_y = min(y1, y2)
            end_y = max(y1, y2)
            for y in start_y:end_y
                if haskey(x_to_cx, x1) && haskey(y_to_cy, y)
                    grid[x_to_cx[x1], y_to_cy[y]] = true
                end
            end
        elseif y1 == y2
            start_x = min(x1, x2)
            end_x = max(x1, x2)
            for x in start_x:end_x
                if haskey(x_to_cx, x) && haskey(y_to_cy, y1)
                    grid[x_to_cx[x], y_to_cy[y1]] = true
                end
            end
        end
    end
    
    # Point-in-polygon
    function point_in_polygon(px::Int, py::Int)::Bool
        inside = false
        for i in 1:length(red_tiles)
            (x1, y1) = red_tiles[i]
            (x2, y2) = red_tiles[mod1(i + 1, length(red_tiles))]
            if (y1 > py) != (y2 > py)
                intersect_x = if y2 != y1
                    (py - y1) * (x2 - x1) / (y2 - y1) + x1
                else
                    px
                end
                if px < intersect_x
                    inside = !inside
                end
            end
        end
        inside
    end
    
    # Flood fill interior
    found_interior = false
    for cx in 1:width
        found_interior && break
        for cy in 1:height
            found_interior && break
            if !grid[cx, cy]
                orig_x = all_x[cx]
                orig_y = all_y[cy]
                if point_in_polygon(orig_x, orig_y)
                    stack = [(cx, cy)]
                    while !isempty(stack)
                        (x, y) = pop!(stack)
                        if x > width || y > height || grid[x, y]
                            continue
                        end
                        orig_x = all_x[x]
                        orig_y = all_y[y]
                        if point_in_polygon(orig_x, orig_y)
                            grid[x, y] = true
                            if x > 1 push!(stack, (x - 1, y)) end
                            if x < width push!(stack, (x + 1, y)) end
                            if y > 1 push!(stack, (x, y - 1)) end
                            if y < height push!(stack, (x, y + 1)) end
                        end
                    end
                    found_interior = true
                end
            end
        end
    end
    
    # Build 2D prefix sum
    prefix = zeros(Int, width + 1, height + 1)
    for cx in 1:width
        for cy in 1:height
            prefix[cx + 1, cy + 1] = prefix[cx, cy + 1] + prefix[cx + 1, cy] - 
                                      prefix[cx, cy] + (grid[cx, cy] ? 1 : 0)
        end
    end
    
    rect_sum(cx1, cx2, cy1, cy2) = prefix[cx2 + 1, cy2 + 1] - prefix[cx1, cy2 + 1] - 
                                    prefix[cx2 + 1, cy1] + prefix[cx1, cy1]
    
    # Generate candidates sorted by area descending
    candidates = []
    for i in 1:length(red_tiles)
        for j in i+1:length(red_tiles)
            (x1, y1) = red_tiles[i]
            (x2, y2) = red_tiles[j]
            min_x = min(x1, x2)
            max_x = max(x1, x2)
            min_y = min(y1, y2)
            max_y = max(y1, y2)
            area = (max_x - min_x + 1) * (max_y - min_y + 1)
            
            if haskey(x_to_cx, min_x) && haskey(x_to_cx, max_x) && 
               haskey(y_to_cy, min_y) && haskey(y_to_cy, max_y)
                cx1 = x_to_cx[min_x]
                cx2 = x_to_cx[max_x]
                cy1 = y_to_cy[min_y]
                cy2 = y_to_cy[max_y]
                push!(candidates, (min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2))
            end
        end
    end
    
    sort!(candidates, by=x->x[5], rev=true)
    
    # Check candidates
    max_area_part2 = 0
    for (min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2) in candidates
        if area <= max_area_part2
            break
        end
        
        valid_count = rect_sum(cx1, cx2, cy1, cy2)
        expected_cells = (cx2 - cx1 + 1) * (cy2 - cy1 + 1)
        
        if valid_count == expected_cells
            all_valid = true
            for (x, y) in [(min_x, min_y), (min_x, max_y), (max_x, min_y), (max_x, max_y)]
                if haskey(x_to_cx, x) && haskey(y_to_cy, y)
                    if !grid[x_to_cx[x], y_to_cy[y]]
                        all_valid = false
                        break
                    end
                else
                    if !point_in_polygon(x, y)
                        all_valid = false
                        break
                    end
                end
            end
            
            if all_valid
                max_area_part2 = area
                break
            end
        end
    end
    
    return (string(max_area_part1), string(max_area_part2))
end

function main()
    data = String(read("../data/input.txt"))
    part1, part2 = solve(String(strip(data)))
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
