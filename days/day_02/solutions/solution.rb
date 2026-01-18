# Check if ID is invalid for Part 1: exactly two identical sequences
def is_invalid_part1(id_str)
  n = id_str.length
  return false if n % 2 != 0
  half = n / 2
  id_str[0, half] == id_str[half, half]
end

# Check if ID is invalid for Part 2: sequence repeated 2+ times
def is_invalid_part2(id_str)
  n = id_str.length
  (2..n).any? do |k|
    next false if n % k != 0
    seq_len = n / k
    pattern = id_str[0, seq_len]
    id_str == pattern * k
  end
end

# Parse a range string like "start-end"
def parse_range(range_str)
  parts = range_str.split('-')
  [parts[0].strip.to_i, parts[1].strip.to_i]
end

# Parse a line of comma-separated ranges
def parse_ranges(line)
  line.split(',').map(&:strip).reject(&:empty?).map { |r| parse_range(r) }
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  
  part1_sum = 0
  part2_sum = 0
  
  lines.each do |line|
    next if line.strip.empty?
    ranges = parse_ranges(line)
    
    ranges.each do |start, end_num|
      (start..end_num).each do |num|
        id_str = num.to_s
        
        part1_sum += num if is_invalid_part1(id_str)
        part2_sum += num if is_invalid_part2(id_str)
      end
    end
  end
  
  [part1_sum.to_s, part2_sum.to_s]
end

def main
  data = File.read("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main
