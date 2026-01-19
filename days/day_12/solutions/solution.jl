function solve(input_data::String)::Tuple{String, String}
    lines = split(strip(input_data), '\n')
    if isempty(lines)
        return ("0", "Final star")
    end
    
    # Parse shapes (first 6 shapes, numbered 0-5)
    shape_areas = zeros(Int, 6)
    i = 1
    shape_idx = 0
    
    while i <= length(lines) && shape_idx < 6
        line = strip(lines[i])
        # Check if this is a shape header (format: "number:")
        if length(line) > 0 && endswith(line, ':')
            shape_num_str = line[1:end-1]
            try
                shape_num = parse(Int, shape_num_str)
                if shape_num == shape_idx
                    # Read the next 3 lines for the shape grid
                    shape_grid = String[]
                    for j in 0:2
                        if i + 1 + j <= length(lines)
                            push!(shape_grid, strip(lines[i + 1 + j]))
                        else
                            push!(shape_grid, "")
                        end
                    end
                    
                    # Count '#' characters in the shape
                    area = 0
                    for row in shape_grid
                        area += count(c -> c == '#', row)
                    end
                    shape_areas[shape_idx + 1] = area
                    shape_idx += 1
                    i += 4 # Skip shape header + 3 grid lines + empty line (if present)
                    continue
                end
            catch
                # Not a valid shape number
            end
        end
        i += 1
    end
    
    # Find where queries start (skip empty lines after shapes)
    query_start = i
    while query_start <= length(lines) && isempty(strip(lines[query_start]))
        query_start += 1
    end
    
    # Parse queries
    possible_count = 0
    for line_idx in query_start:length(lines)
        line = strip(lines[line_idx])
        if isempty(line)
            continue
        end
        
        # Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
        if !(':' in line)
            continue
        end
        
        parts = split(line, ':', limit=2)
        if length(parts) != 2
            continue
        end
        
        # Parse dimensions
        dims = strip(parts[1])
        if !('x' in dims)
            continue
        end
        
        dim_parts = split(dims, 'x')
        if length(dim_parts) != 2
            continue
        end
        
        width = try
            parse(Int, dim_parts[1])
        catch
            continue
        end
        
        height = try
            parse(Int, dim_parts[2])
        catch
            continue
        end
        
        # Parse counts
        count_parts = split(strip(parts[2]), r"\s+")
        if length(count_parts) != 6
            continue
        end
        
        counts = Int[]
        valid_counts = true
        try
            for c in count_parts
                push!(counts, parse(Int, c))
            end
        catch
            valid_counts = false
        end
        
        if !valid_counts || length(counts) != 6
            continue
        end
        
        # Calculate area check
        region_area = width * height
        required_area = 0
        for j in 1:6
            required_area += shape_areas[j] * counts[j]
        end
        
        if required_area <= region_area
            possible_count += 1
        end
    end
    
    # Part 2: Final star (no computation needed)
    part2 = "Final star"
    
    return (string(possible_count), part2)
end

if abspath(PROGRAM_FILE) == @__FILE__
    data = read("../data/input.txt", String)
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end
