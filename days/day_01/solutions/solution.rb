require_relative '../../../utilities/ruby/get_input.rb'

def solve(lines)
  # Part 1: Count times dial ends at 0 after a rotation
  position = 50
  count_part1 = 0
  
  lines.each do |line|
    line = line.strip
    next if line.empty?
    
    direction = line[0]
    distance = line[1..-1].to_i
    
    # Apply rotation
    if direction == 'L'
      position = ((position - distance) % 100 + 100) % 100
    else # direction == 'R'
      position = (position + distance) % 100
    end
    
    # Check if ended at 0
    if position == 0
      count_part1 += 1
    end
  end
  
  # Part 2: Count times dial is at 0 during entire process
  position = 50
  count_part2 = 0
  
  lines.each do |line|
    line = line.strip
    next if line.empty?
    
    direction = line[0]
    distance = line[1..-1].to_i
    
    start_pos = position
    
    # Check each click position during rotation
    (1..distance).each do |click|
      click_pos = if direction == 'L'
        ((start_pos - click) % 100 + 100) % 100
      else # direction == 'R'
        (start_pos + click) % 100
      end
      
      if click_pos == 0
        count_part2 += 1
      end
    end
    
    # Update position after rotation
    if direction == 'L'
      position = ((position - distance) % 100 + 100) % 100
    else
      position = (position + distance) % 100
    end
  end
  
  [count_part1, count_part2]
end

lines = get_input(1)
part1, part2 = solve(lines)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
