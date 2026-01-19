# Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
struct MemoKey
    node::String
    visited_fft::Bool
    visited_dac::Bool
end

function solve(input_data)
    lines = split(strip(input_data), "\n")
    
    # Build graph: device -> list of outputs
    graph = Dict{String, Vector{String}}()
    for line in lines
        isempty(strip(line)) && continue
        
        parts = split(line, ":")
        length(parts) != 2 && continue
        
        device = strip(parts[1])
        outputs_str = length(parts) > 1 ? strip(parts[2]) : ""
        outputs = isempty(outputs_str) ? String[] : split(outputs_str, r"\s+")
        graph[device] = outputs
    end
    
    # Part 1: Count paths from "you" to "out"
    function count_paths_part1(node, memo)
        if node == "out"
            return 1
        end
        
        if haskey(memo, node)
            return memo[node]
        end
        
        count = 0
        neighbors = get(graph, node, String[])
        for neighbor in neighbors
            count += count_paths_part1(neighbor, memo)
        end
        
        memo[node] = count
        return count
    end
    
    part1_count = 0
    if haskey(graph, "you")
        part1_memo = Dict{String, Int64}()
        part1_count = count_paths_part1("you", part1_memo)
    end
    
    function count_paths_part2(node, visited_fft, visited_dac, memo)
        if node == "out"
            return (visited_fft && visited_dac) ? 1 : 0
        end
        
        key = MemoKey(node, visited_fft, visited_dac)
        if haskey(memo, key)
            return memo[key]
        end
        
        # Update flags when visiting fft or dac
        new_visited_fft = visited_fft || (node == "fft")
        new_visited_dac = visited_dac || (node == "dac")
        
        count = 0
        neighbors = get(graph, node, String[])
        for neighbor in neighbors
            count += count_paths_part2(neighbor, new_visited_fft, new_visited_dac, memo)
        end
        
        memo[key] = count
        return count
    end
    
    part2_count = 0
    if haskey(graph, "svr")
        part2_memo = Dict{MemoKey, Int64}()
        part2_count = count_paths_part2("svr", false, false, part2_memo)
    end
    
    return string(part1_count), string(part2_count)
end

input_data = read("../data/input.txt", String)
part1, part2 = solve(input_data)
println("Part 1: $(part1)")
println("Part 2: $(part2)")
