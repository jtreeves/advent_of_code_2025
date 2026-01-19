function read_input_raw(file_path::String)::String
    return read(file_path, String)
end

mutable struct ParseResult
    target_pattern::Vector{Bool}
    buttons::Vector{Vector{Int}}
    joltages::Vector{Int}
end

function parse_line(line::String)::ParseResult
    result = ParseResult(Bool[], Vector{Int}[], Int[])
    
    # Extract pattern: [.##.]
    pattern_re = r"\[([.#]+)\]"
    pattern_match = match(pattern_re, line)
    if pattern_match !== nothing
        pattern_str = pattern_match.captures[1]
        result.target_pattern = [c == '#' for c in pattern_str]
    end
    
    # Extract buttons: (1,3) (2) etc.
    button_re = r"\(([^)]*)\)"
    for m in eachmatch(button_re, line)
        btn_str = strip(m.captures[1])
        if isempty(btn_str)
            push!(result.buttons, Int[])
        else
            lights = [parse(Int, x) for x in split(btn_str, ',')]
            push!(result.buttons, lights)
        end
    end
    
    # Extract joltages: {3,5,4,7}
    joltage_re = r"\{([^}]+)\}"
    joltage_match = match(joltage_re, line)
    if joltage_match !== nothing
        joltage_str = joltage_match.captures[1]
        result.joltages = [parse(Int, strip(x)) for x in split(joltage_str, ',')]
    end
    
    return result
end

function gaussian_elimination_gf2(matrix::Vector{Vector{Bool}}, target::Vector{Bool})::Union{Int, Nothing}
    num_buttons = length(matrix)
    num_lights = length(target)
    
    # Create augmented matrix [A | b]
    aug = Vector{Vector{Bool}}()
    for i in 1:num_lights
        row = Bool[]
        for j in 1:num_buttons
            if j <= length(matrix) && i <= length(matrix[j])
                push!(row, matrix[j][i])
            else
                push!(row, false)
            end
        end
        push!(row, target[i])
        push!(aug, row)
    end
    
    # Gaussian elimination mod 2
    pivot_row = 1
    pivot_col = 1
    
    while pivot_row <= num_lights && pivot_col <= num_buttons
        # Find pivot
        pivot_idx = nothing
        for i in pivot_row:num_lights
            if aug[i][pivot_col]
                pivot_idx = i
                break
            end
        end
        
        if pivot_idx === nothing
            pivot_col += 1
            continue
        end
        
        # Swap rows
        if pivot_idx != pivot_row
            aug[pivot_row], aug[pivot_idx] = aug[pivot_idx], aug[pivot_row]
        end
        
        # Eliminate
        for i in (pivot_row + 1):num_lights
            if aug[i][pivot_col]
                for j in 1:(num_buttons + 1)
                    aug[i][j] = aug[i][j] ⊻ aug[pivot_row][j]
                end
            end
        end
        
        pivot_row += 1
        pivot_col += 1
    end
    
    # Check for inconsistency (0 = 1)
    for i in pivot_row:num_lights
        if aug[i][num_buttons + 1] && !any(aug[i][1:num_buttons])
            return nothing # No solution
        end
    end
    
    # Back substitution to find solution (set free variables to 0 to minimize)
    solution = falses(num_buttons)
    used_rows = Set{Int}()
    
    # Process from bottom up
    for i in num_lights:-1:1
        # Find first non-zero column
        pivot_col_idx = nothing
        for j in 1:num_buttons
            if aug[i][j] && !(j in used_rows)
                pivot_col_idx = j
                push!(used_rows, j)
                break
            end
        end
        
        if pivot_col_idx !== nothing
            # Calculate value
            val = aug[i][num_buttons + 1]
            for j in (pivot_col_idx + 1):num_buttons
                if aug[i][j] && solution[j]
                    val = !val
                end
            end
            solution[pivot_col_idx] = val
        end
    end
    
    # Count number of presses
    return count(solution)
end

function solve_part2_ilp(buttons::Vector{Vector{Int}}, joltages::Vector{Int}, target_pattern::Vector{Bool})::Union{Int, Nothing}
    num_buttons = length(buttons)
    num_lights = length(joltages)
    
    # Base case: all joltages are 0
    if all(j == 0 for j in joltages)
        return 0
    end
    
    # Bifurcate approach: try all parity solutions (buttons pressed 0 or 1 time)
    # that achieve the required parity, then divide by 2 and recurse
    button_matrix = Vector{Bool}[]
    for btn_idx in 1:num_buttons
        row = falses(num_lights)
        for light in buttons[btn_idx]
            if light >= 0 && light < num_lights
                row[light + 1] = true  # Convert to 1-indexed
            end
        end
        push!(button_matrix, row)
    end
    
    # Required parity: pattern ON = odd, pattern OFF = even
    required_parity = copy(target_pattern)
    
    # Find minimum parity presses needed (Part 1 style)
    parity_toggles = copy(required_parity)
    min_parity_presses = gaussian_elimination_gf2(button_matrix, parity_toggles)
    
    if min_parity_presses !== nothing
        # Try all combinations starting from minimum
        best_result = nothing
        
        # Helper function to generate combinations
        function generate_combinations(n::Int, k::Int)::Vector{Vector{Int}}
            if k == 0
                return [Int[]]
            elseif k > n
                return Vector{Int}[]
            elseif k == n
                return [[i for i in 1:n]]
            else
                result = Vector{Int}[]
                # Recursive: include first element or not
                for combo in generate_combinations(n - 1, k - 1)
                    push!(result, [1; [x + 1 for x in combo]])
                end
                for combo in generate_combinations(n - 1, k)
                    push!(result, [x + 1 for x in combo])
                end
                return result
            end
        end
        
        # Try all combinations - Reddit emphasizes trying ALL parity solutions
        max_parity_to_try = num_buttons
        
        for k in min_parity_presses:max_parity_to_try
            # Pruning: if we already have a solution and k is getting large, skip
            if best_result !== nothing && k > best_result
                break
            end
            
            for button_combo in generate_combinations(num_buttons, k)
                # Simulate pressing these buttons once
                resulting_joltages = copy(joltages)
                for btn_idx in button_combo
                    for light in buttons[btn_idx]
                        if light >= 0 && light < num_lights
                            resulting_joltages[light + 1] += 1  # Convert to 1-indexed
                        end
                    end
                end
                
                # Check if parity matches
                resulting_parity = [j % 2 == 1 for j in resulting_joltages]
                if resulting_parity == required_parity
                    # Check if all remaining are even (can divide by 2)
                    if all(j % 2 == 0 for j in resulting_joltages)
                        # Divide by 2 and recurse
                        halved_joltages = [j ÷ 2 for j in resulting_joltages]
                        remaining_result = solve_part2_ilp(buttons, halved_joltages, target_pattern)
                        if remaining_result !== nothing
                            total = k + remaining_result * 2
                            if best_result === nothing || total < best_result
                                best_result = total
                            end
                        end
                    end
                end
            end
        end
        
        if best_result !== nothing
            return best_result
        end
    end
    
    # Fallback: brute force with divide-by-2 optimization
    if all(j % 2 == 0 for j in joltages) && any(j > 0 for j in joltages)
        halved_joltages = [j ÷ 2 for j in joltages]
        result = solve_part2_ilp(buttons, halved_joltages, target_pattern)
        if result !== nothing
            return result * 2
        end
        return nothing
    end
    
    # Final fallback: bounded DFS
    max_joltage = isempty(joltages) ? 0 : maximum(joltages)
    
    function dfs(button_idx::Int, current_joltages::Vector{Int}, presses_so_far::Int, best::Union{Int, Nothing})::Union{Int, Nothing}
        if button_idx > num_buttons
            if all(i -> current_joltages[i] == joltages[i], 1:num_lights)
                return best === nothing ? presses_so_far : min(best, presses_so_far)
            end
            return best
        end
        
        if best !== nothing && presses_so_far >= best
            return best
        end
        
        best_result = best
        for presses in 0:max_joltage
            if best !== nothing && presses_so_far + presses >= best
                break
            end
            
            new_joltages = copy(current_joltages)
            for light in buttons[button_idx]
                if light >= 0 && light < num_lights
                    new_joltages[light + 1] += presses  # Convert to 1-indexed
                end
            end
            
            if any(i -> new_joltages[i] > joltages[i], 1:num_lights)
                continue
            end
            
            result = dfs(button_idx + 1, new_joltages, presses_so_far + presses, best_result)
            if result !== nothing
                best_result = best_result === nothing ? result : min(best_result, result)
            end
        end
        
        return best_result
    end
    
    initial_joltages = zeros(Int, num_lights)
    return dfs(1, initial_joltages, 0, nothing)
end

function solve(input_data::String)::Tuple{String, String}
    lines = split(String(strip(input_data)), '\n')
    if isempty(lines)
        return ("0", "0")
    end
    
    part1_total = 0
    part2_total = 0
    
    for line in lines
        line_str = String(strip(line))
        if isempty(line_str)
            continue
        end
        
        parsed = parse_line(line_str)
        if isempty(parsed.target_pattern)
            continue
        end
        
        num_lights = length(parsed.target_pattern)
        
        # Part 1: GF(2) linear system
        # Build incidence matrix: matrix[i][j] = true if button i toggles light j
        button_matrix = Vector{Bool}[]
        for i in 1:length(parsed.buttons)
            row = falses(num_lights)
            for light in parsed.buttons[i]
                if light >= 0 && light < num_lights
                    row[light + 1] = true  # Convert to 1-indexed
                end
            end
            push!(button_matrix, row)
        end
        
        # Target: all start OFF, need to toggle to match pattern
        # Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
        required_toggles = copy(parsed.target_pattern)
        
        result = gaussian_elimination_gf2(button_matrix, required_toggles)
        if result !== nothing
            part1_total += result
        end
        
        # Part 2: Integer Linear Programming
        if length(parsed.joltages) == num_lights
            result2 = solve_part2_ilp(parsed.buttons, parsed.joltages, parsed.target_pattern)
            if result2 !== nothing
                part2_total += result2
            end
        end
    end
    
    return (string(part1_total), string(part2_total))
end

function main()
    data = read("../data/input.txt", String)
    data = String(strip(data))  # Convert SubString to String
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
