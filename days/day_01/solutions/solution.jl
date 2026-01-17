# Placeholder for Day 01 Julia solution
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
    data = read("../data/input.txt", String) |> strip
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
