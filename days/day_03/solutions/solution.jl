# Find the largest N-digit number by selecting N digits in order from bank
function find_largest_subsequence(bank::String, n::Int)::Int
    bank_len = length(bank)
    if bank_len < n
        return 0
    end
    
    result = Char[]
    start = 1  # Julia is 1-indexed
    
    for i in 1:n
        remaining_needed = n - i
        end_pos = bank_len - remaining_needed
        
        max_digit = bank[start]
        max_pos = start
        for j in (start + 1):end_pos
            if bank[j] > max_digit
                max_digit = bank[j]
                max_pos = j
            end
        end
        
        push!(result, max_digit)
        start = max_pos + 1
    end
    
    parse(Int, String(result))
end

function solve(input_data::String)::Tuple{String, String}
    lines = split(strip(input_data), '\n')
    banks = [String(x) for x in filter(x -> x != "", lines)]
    
    part1_sum = sum([find_largest_subsequence(bank, 2) for bank in banks])
    
    banks_12plus = filter(x -> length(x) >= 12, banks)
    part2_sum = sum([find_largest_subsequence(bank, 12) for bank in banks_12plus])
    
    return (string(part1_sum), string(part2_sum))
end

function main()
    data = read("../data/input.txt", String)
    data_str = String(strip(data))
    part1, part2 = solve(data_str)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
