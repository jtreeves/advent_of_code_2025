# Placeholder for Day 01 Julia solution
# Note: Julia utility functions would be in utilities/julia/get_input.jl
# For now, using inline function - will be replaced with proper includes

include("../../../utilities/julia/get_input.jl")

function solve(input_data::String)::Tuple{String, String}
    println("Day 01 Julia placeholder")
    lines = split(strip(input_data), '\n')
    println("Lines: ", lines)
    
    # Part 1
    part1_result = "TODO"
    
    # Part 2
    part2_result = "TODO"
    
    return (part1_result, part2_result)
end

function main()
    # Use utility function to get input
    data = read_input_raw("../data/input.txt")
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
