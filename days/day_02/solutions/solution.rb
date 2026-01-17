# Placeholder for Day 2 Ruby solution
def solve(input_data)
  puts "Day 2 Ruby placeholder"
  lines = input_data.strip.split("\n")
  puts "Lines: #{lines}"
  
  # Part 1
  part1_result = "TODO"
  
  # Part 2
  part2_result = "TODO"
  
  [part1_result, part2_result]
end

def main
  data = File.read("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main
