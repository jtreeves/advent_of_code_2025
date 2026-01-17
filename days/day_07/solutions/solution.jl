# Placeholder for Day 2 Julia solution
function solve(input_data::String)::Tuple{String, String}
    println("Day 2 Julia placeholder")
    lines = split(strip(input_data), '\n')
    
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
