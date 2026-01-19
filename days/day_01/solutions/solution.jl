function solve(lines)
    # Part 1: Count times dial ends at 0 after a rotation
    position = 50
    count_part1 = 0
    
    for line in lines
        isempty(strip(line)) && continue
        
        direction = line[1]
        distance = parse(Int, line[2:end])
        
        # Apply rotation
        if direction == 'L'
            position = mod(mod(position - distance, 100) + 100, 100)
        else # direction == 'R'
            position = mod(position + distance, 100)
        end
        
        # Check if ended at 0
        if position == 0
            count_part1 += 1
        end
    end
    
    # Part 2: Count times dial is at 0 during entire process
    position = 50
    count_part2 = 0
    
    for line in lines
        isempty(strip(line)) && continue
        
        direction = line[1]
        distance = parse(Int, line[2:end])
        
        start_pos = position
        
        # Check each click position during rotation
        for click in 1:distance
            click_pos = if direction == 'L'
                mod(mod(start_pos - click, 100) + 100, 100)
            else # direction == 'R'
                mod(start_pos + click, 100)
            end
            
            if click_pos == 0
                count_part2 += 1
            end
        end
        
        # Update position after rotation
        if direction == 'L'
            position = mod(mod(position - distance, 100) + 100, 100)
        else
            position = mod(position + distance, 100)
        end
    end
    
    return count_part1, count_part2
end

lines = [strip(line) for line in readlines("../data/input.txt")]
part1, part2 = solve(lines)
println("Part 1: $(part1)")
println("Part 2: $(part2)")
