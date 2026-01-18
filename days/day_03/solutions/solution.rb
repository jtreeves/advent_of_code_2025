# Find the largest N-digit number by selecting N digits in order from bank
def find_largest_subsequence(bank, n)
  bank_len = bank.length
  return 0 if bank_len < n
  
  result = []
  start = 0
  
  n.times do |i|
    remaining_needed = n - i - 1
    end_pos = bank_len - remaining_needed
    
    max_digit = bank[start]
    max_pos = start
    (start + 1...end_pos).each do |j|
      if bank[j] > max_digit
        max_digit = bank[j]
        max_pos = j
      end
    end
    
    result << max_digit
    start = max_pos + 1
  end
  
  result.join.to_i
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  
  part1_sum = 0
  part2_sum = 0
  
  lines.each do |line|
    bank = line.strip
    next if bank.empty?
    
    part1_sum += find_largest_subsequence(bank, 2)
    
    if bank.length >= 12
      part2_sum += find_largest_subsequence(bank, 12)
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
