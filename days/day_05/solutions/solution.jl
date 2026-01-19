function solve(input_data::String)::Tuple{Int64, Int64}
    lines = split(strip(input_data), '\n')
    
    # Find blank line separator
    blank_idx = findfirst(l -> strip(l) == "", lines)
    blank_idx = blank_idx === nothing ? length(lines) : blank_idx
    
    # Parse ranges (first section)
    ranges = []
    for i in 1:blank_idx
        line = strip(lines[i])
        if line != ""
            parts = split(line, '-')
            start = parse(Int64, parts[1])
            e = parse(Int64, parts[2])
            push!(ranges, (start, e))
        end
    end
    
    # Parse IDs to check (second section)
    ids = []
    for i in (blank_idx + 1):length(lines)
        line = strip(lines[i])
        if line != ""
            push!(ids, parse(Int64, line))
        end
    end
    
    # Part 1: Count how many IDs fall into any range
    part1_count = 0
    for id_val in ids
        for (start, e) in ranges
            if start <= id_val <= e
                part1_count += 1
                break
            end
        end
    end
    
    # Part 2: Merge ranges and count total unique IDs covered
    # Sort ranges by start value
    sorted_ranges = sort(ranges, by = x -> x[1])
    
    # Merge overlapping/adjacent ranges
    merged = []
    if !isempty(sorted_ranges)
        push!(merged, [sorted_ranges[1][1], sorted_ranges[1][2]])
        for i in 2:length(sorted_ranges)
            (curr_start, curr_end) = sorted_ranges[i]
            last = merged[end]
            # Check if overlaps or is adjacent (curr_start <= last[2] + 1)
            if curr_start <= last[2] + 1
                # Merge: update end to max of both ends
                last[2] = max(last[2], curr_end)
            else
                # No overlap, add as new range
                push!(merged, [curr_start, curr_end])
            end
        end
    end
    
    # Calculate total unique IDs covered
    part2_total = sum(e - s + 1 for (s, e) in ([x[1], x[2]] for x in merged))
    
    return (part1_count, part2_total)
end

content = read("../data/input.txt", String)
(part1, part2) = solve(content)
println("Part 1: $(part1)")
println("Part 2: $(part2)")
