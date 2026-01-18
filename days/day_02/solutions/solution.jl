# Check if ID is invalid for Part 1: exactly two identical sequences
function is_invalid_part1(id_str::String)::Bool
    n = length(id_str)
    if n % 2 != 0
        return false
    end
    half = n ÷ 2
    return id_str[1:half] == id_str[half+1:end]
end

# Check if ID is invalid for Part 2: sequence repeated 2+ times
function is_invalid_part2(id_str::String)::Bool
    n = length(id_str)
    for k in 2:n
        if n % k == 0
            seq_len = n ÷ k
            pattern = id_str[1:seq_len]
            repeated = repeat(pattern, k)
            if id_str == repeated
                return true
            end
        end
    end
    return false
end

# Parse a range string like "start-end"
function parse_range(range_str::String)::Tuple{Int, Int}
    parts = split(range_str, '-')
    start = parse(Int, strip(parts[1]))
    end_val = parse(Int, strip(parts[2]))
    return (start, end_val)
end

# Parse a line of comma-separated ranges
function parse_ranges(line::String)::Vector{Tuple{Int, Int}}
    parts = split(line, ',')
    ranges = Tuple{Int, Int}[]
    for part in parts
        part_str = String(strip(part))
        if length(part_str) > 0
            push!(ranges, parse_range(part_str))
        end
    end
    return ranges
end

function solve(input_data::String)::Tuple{String, String}
    lines = split(strip(input_data), '\n')
    
    part1_sum::Int64 = 0
    part2_sum::Int64 = 0
    
    for line in lines
        line_str = String(strip(line))
        if length(line_str) == 0
            continue
        end
        ranges = parse_ranges(line_str)
        
        for (start, end_val) in ranges
            for num in start:end_val
                id_str = string(num)
                
                if is_invalid_part1(id_str)
                    part1_sum += num
                end
                
                if is_invalid_part2(id_str)
                    part2_sum += num
                end
            end
        end
    end
    
    return (string(part1_sum), string(part2_sum))
end

function main()
    data = read("../data/input.txt", String) |> strip |> String
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
