mutable struct Problem
    start_col::Int
    end_col::Int
    op::Char
end

function solve(input_data::String)::Tuple{Int64, Int64}
    lines = split(strip(input_data), '\n')
    if isempty(lines)
        return (0, 0)
    end
    
    # Find maximum line length and pad all lines
    max_len = maximum(length, lines)
    padded_lines = [lpad(line, max_len) for line in lines]
    
    # Operator row is the last row
    op_row_idx = length(padded_lines)
    op_row = padded_lines[op_row_idx]
    num_rows = padded_lines[1:(op_row_idx - 1)]
    
    # Part 1: Parse horizontally
    part1_total = Int64(0)
    
    # Find problem boundaries (columns that are all spaces)
    is_space_col = [all(line[col] == ' ' for line in padded_lines) for col in 1:max_len]
    
    # Group columns into problems
    problems = Problem[]
    i = 1
    while i <= max_len
        if !is_space_col[i]
            # Start of a problem
            start_col = i
            while i <= max_len && !is_space_col[i]
                i += 1
            end
            end_col = i - 1
            # Extract operator for this problem
            op = nothing
            for j in start_col:end_col
                if j <= length(op_row) && (op_row[j] == '+' || op_row[j] == '*')
                    op = op_row[j]
                    break
                end
            end
            if op !== nothing
                push!(problems, Problem(start_col, end_col, op))
            end
        else
            i += 1
        end
    end
    
    # Solve Part 1: Extract numbers horizontally
    for prob in problems
        numbers = Int64[]
        for row in num_rows
            # Extract the substring for this problem
            problem_str = strip(row[prob.start_col:prob.end_col])
            # Split by spaces and parse numbers
            parts = split(problem_str)
            for part in parts
                num = tryparse(Int64, part)
                if num !== nothing
                    push!(numbers, num)
                end
            end
        end
        
        # Apply operator
        if !isempty(numbers)
            result = if prob.op == '+'
                sum(numbers)
            else
                prod(numbers)
            end
            part1_total += result
        end
    end
    
    # Part 2: Parse vertically (columns, right-to-left)
    # Approach: Use Julia's array operations and broadcasting
    part2_total = Int64(0)
    
    for prob in problems
        # Extract columns (transpose) using array indexing
        col_strings = []
        for col in prob.start_col:prob.end_col
            if col <= length(is_space_col) && is_space_col[col]
                continue
            end
            col_chars = [row[col] for row in num_rows if col <= length(row)]
            col_str = strip(join(filter(c -> isdigit(c) || c == ' ', col_chars)))
            if !isempty(col_str)
                push!(col_strings, col_str)
            end
        end
        
        # Reverse for right-to-left reading
        col_strings = reverse(col_strings)
        
        # Parse numbers
        numbers = Int64[]
        for col_str in col_strings
            digits_only = filter(isdigit, col_str)
            if !isempty(digits_only)
                num = tryparse(Int64, digits_only)
                if num !== nothing
                    push!(numbers, num)
                end
            end
        end
        
        # Apply operator
        if !isempty(numbers)
            result = if prob.op == '+'
                sum(numbers)
            else
                prod(numbers)
            end
            part2_total += result
        end
    end
    
    return (part1_total, part2_total)
end

content = read("../data/input.txt", String)
(part1, part2) = solve(content)
println("Part 1: $(part1)")
println("Part 2: $(part2)")
