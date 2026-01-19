function count_neighbors(grid, i, j, rows, cols)
    count = 0
    for di in -1:1
        for dj in -1:1
            if di == 0 && dj == 0
                continue
            end
            ni = i + di
            nj = j + dj
            if ni >= 1 && ni <= rows && nj >= 1 && nj <= cols && grid[ni][nj] == '@'
                count += 1
            end
        end
    end
    return count
end

function solve(lines)
    rows = length(lines)
    cols = rows > 0 ? length(lines[1]) : 0
    
    # Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    part1_count = 0
    for i in 1:rows
        for j in 1:cols
            if lines[i][j] == '@'
                neighbors = count_neighbors(lines, i, j, rows, cols)
                if neighbors < 4
                    part1_count += 1
                end
            end
        end
    end
    
    # Part 2: Iteratively remove accessible rolls until none can be removed
    grid = [collect(line) for line in lines]
    
    part2_count = 0
    while true
        to_remove = []
        for i in 1:rows
            for j in 1:cols
                if grid[i][j] == '@'
                    # Convert grid to string array for neighbor counting
                    grid_str = [String(grid[k]) for k in 1:rows]
                    neighbors = count_neighbors(grid_str, i, j, rows, cols)
                    if neighbors < 4
                        push!(to_remove, (i, j))
                    end
                end
            end
        end
        
        if isempty(to_remove)
            break
        end
        
        # Remove all marked positions
        for (i, j) in to_remove
            grid[i][j] = '.'
        end
        part2_count += length(to_remove)
    end
    
    return part1_count, part2_count
end

lines = [strip(line) for line in readlines("../data/input.txt")]
part1, part2 = solve(lines)
println("Part 1: $(part1)")
println("Part 2: $(part2)")
