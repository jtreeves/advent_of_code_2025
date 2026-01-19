function solve(input_data::String)::Tuple{Int64, Int64}
    lines = split(strip(input_data), '\n')
    if isempty(lines)
        return (0, 0)
    end
    
    grid = [collect(line) for line in lines]
    rows = length(grid)
    cols = rows > 0 ? length(grid[1]) : 0
    
    # Find starting position S
    start_row = start_col = nothing
    for r in 1:rows
        for c in 1:cols
            if grid[r][c] == 'S'
                start_row = r
                start_col = c
                break
            end
        end
        start_row !== nothing && break
    end
    
    if start_row === nothing
        return (0, 0)
    end
    
    # Part 1: Count total splits
    split_count = Int64(0)
    active_beams = Set([start_col])
    
    # Process each row starting from the row after S
    for r in (start_row + 1):rows
        next_beams = Set{Int}()
        for col in active_beams
            if grid[r][col] == '.'
                # Beam continues down
                push!(next_beams, col)
            elseif grid[r][col] == '^'
                # Beam splits
                split_count += 1
                # Add beams to left and right
                if col - 1 >= 1
                    push!(next_beams, col - 1)
                end
                if col + 1 <= cols
                    push!(next_beams, col + 1)
                end
            end
        end
        active_beams = next_beams
    end
    
    # Part 2: Count beams reaching bottom row
    beam_counts = zeros(Int64, rows, cols)
    beam_counts[start_row, start_col] = 1 # Start with 1 beam at S
    
    # Process each row starting from the row after S
    for r in (start_row + 1):rows
        for c in 1:cols
            prev_count = beam_counts[r - 1, c]
            if prev_count > 0
                if grid[r][c] == '.'
                    # Beam continues down
                    beam_counts[r, c] += prev_count
                elseif grid[r][c] == '^'
                    # Beam splits into left and right
                    if c - 1 >= 1
                        beam_counts[r, c - 1] += prev_count
                    end
                    if c + 1 <= cols
                        beam_counts[r, c + 1] += prev_count
                    end
                end
            end
        end
    end
    
    # Sum all beams in bottom row
    bottom_beam_count = sum(beam_counts[rows, :])
    
    return (split_count, bottom_beam_count)
end

content = read("../data/input.txt", String)
part1, part2 = solve(content)
println("Part 1: $part1")
println("Part 2: $part2")
