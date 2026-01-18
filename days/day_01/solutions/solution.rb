# Placeholder for Day 01 Ruby solution
# Note: Ruby utility functions would be in utilities/ruby/get_input.rb
# For now, using inline function - will be replaced with proper requires

require_relative '../../../utilities/ruby/get_input'

def solve(input_data)
  puts "Day 01 Ruby placeholder"
  lines = input_data.strip.split("\n")
  puts "Lines: #{lines}"
  
  # Part 1
  part1_result = "TODO"
  
  # Part 2
  part2_result = "TODO"
  
  [part1_result, part2_result]
end

def main
  # Use utility function to get input
  data = read_input_raw("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main
