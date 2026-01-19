require 'set'

def parse_line(line)
  # Extract pattern: [.##.]
  pattern_match = line.match(/\[([.#]+)\]/)
  return { target_pattern: [], buttons: [], joltages: [] } unless pattern_match
  pattern_str = pattern_match[1]
  target_pattern = pattern_str.chars.map { |c| c == '#' }
  
  # Extract buttons: (1,3) (2) etc.
  buttons = []
  line.scan(/\(([^)]*)\)/) do |btn_str|
    btn_str = btn_str[0].strip
    if btn_str.empty?
      buttons << []
    else
      lights = btn_str.split(',').map { |x| x.strip.to_i }
      buttons << lights
    end
  end
  
  # Extract joltages: {3,5,4,7}
  joltage_match = line.match(/\{([^}]+)\}/)
  joltages = []
  if joltage_match
    joltage_str = joltage_match[1]
    joltages = joltage_str.split(',').map { |x| x.strip.to_i }
  end
  
  { target_pattern: target_pattern, buttons: buttons, joltages: joltages }
end

def gaussian_elimination_gf2(matrix, target)
  num_buttons = matrix.length
  num_lights = target.length
  
  # Create augmented matrix [A | b]
  aug = []
  (0...num_lights).each do |i|
    row = []
    (0...num_buttons).each do |j|
      val = if j < matrix.length && i < matrix[j].length
              matrix[j][i]
            else
              false
            end
      row << val
    end
    row << target[i]
    aug << row
  end
  
  # Gaussian elimination mod 2
  pivot_row = 0
  pivot_col = 0
  
  while pivot_row < num_lights && pivot_col < num_buttons
    # Find pivot
    pivot_idx = nil
    (pivot_row...num_lights).each do |i|
      if aug[i][pivot_col]
        pivot_idx = i
        break
      end
    end
    
    unless pivot_idx
      pivot_col += 1
      next
    end
    
    # Swap rows
    aug[pivot_row], aug[pivot_idx] = aug[pivot_idx], aug[pivot_row] if pivot_idx != pivot_row
    
    # Eliminate
    ((pivot_row + 1)...num_lights).each do |i|
      if aug[i][pivot_col]
        (0..num_buttons).each do |j|
          aug[i][j] = aug[i][j] ^ aug[pivot_row][j]
        end
      end
    end
    
    pivot_row += 1
    pivot_col += 1
  end
  
  # Check for inconsistency (0 = 1)
  (pivot_row...num_lights).each do |i|
    if aug[i][num_buttons] && !aug[i][0...num_buttons].any?
      return nil # No solution
    end
  end
  
  # Back substitution to find solution (set free variables to 0 to minimize)
  solution = [false] * num_buttons
  used_rows = Set.new
  
  # Process from bottom up
  (num_lights - 1).downto(0) do |i|
    # Find first non-zero column
    pivot_col_idx = nil
    (0...num_buttons).each do |j|
      if aug[i][j] && !used_rows.include?(j)
        pivot_col_idx = j
        used_rows.add(j)
        break
      end
    end
    
    if pivot_col_idx
      # Calculate value
      val = aug[i][num_buttons]
      ((pivot_col_idx + 1)...num_buttons).each do |j|
        if aug[i][j] && solution[j]
          val = !val
        end
      end
      solution[pivot_col_idx] = val
    end
  end
  
  # Count number of presses
  solution.count { |x| x }
end

def solve_part2_ilp(buttons, joltages)
  num_buttons = buttons.length
  num_lights = joltages.length
  
  # Bounded search: max presses per button is max joltage
  max_joltage = joltages.max || 0
  
  dfs = lambda do |button_idx, current_joltages, presses_so_far, best|
    if button_idx >= num_buttons
      if current_joltages.zip(joltages).all? { |a, b| a == b }
        return best ? [best, presses_so_far].min : presses_so_far
      end
      return best
    end
    
    # Pruning: if current presses already exceed best, skip
    return best if best && presses_so_far >= best
    
    # Calculate minimum additional presses needed (lower bound)
    remaining_needs = current_joltages.zip(joltages).map { |curr, target| [target - curr, 0].max }
    sum_remaining = remaining_needs.sum
    if sum_remaining > 0
      max_lights_per_button = buttons[button_idx..-1].map(&:length).max || 1
      if max_lights_per_button > 0
        min_additional = (sum_remaining + max_lights_per_button - 1) / max_lights_per_button
        return best if best && presses_so_far + min_additional >= best
      end
    end
    
    # Try 0 to max_joltage presses of current button
    best_result = best
    (0..max_joltage).each do |presses|
      break if best && presses_so_far + presses >= best
      
      new_joltages = current_joltages.dup
      buttons[button_idx].each do |light|
        if light >= 0 && light < num_lights
          new_joltages[light] += presses
        end
      end
      
      # Check if any exceeds target (pruning)
      next if new_joltages.zip(joltages).any? { |a, b| a > b }
      
      result = dfs.call(button_idx + 1, new_joltages, presses_so_far + presses, best_result)
      best_result = result if result
    end
    
    best_result
  end
  
  # Start with all joltages at 0
  initial_joltages = [0] * num_lights
  dfs.call(0, initial_joltages, 0, nil)
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  return ["0", "0"] if lines.empty?
  
  part1_total = 0
  part2_total = 0
  
  lines.each do |line|
    next if line.strip.empty?
    
    parsed = parse_line(line)
    next if parsed[:target_pattern].empty?
    
    num_lights = parsed[:target_pattern].length
    
    # Part 1: GF(2) linear system
    # Build incidence matrix: matrix[i][j] = true if button i toggles light j
    button_matrix = Array.new(parsed[:buttons].length) { Array.new(num_lights, false) }
    parsed[:buttons].each_with_index do |btn, i|
      btn.each do |light|
        if light >= 0 && light < num_lights
          button_matrix[i][light] = true
        end
      end
    end
    
    # Target: all start OFF, need to toggle to match pattern
    # Required toggles: target_pattern (1 = needs toggle, 0 = no toggle)
    required_toggles = parsed[:target_pattern].dup
    
    result = gaussian_elimination_gf2(button_matrix, required_toggles)
    part1_total += result if result
    
    # Part 2: Integer Linear Programming
    if parsed[:joltages].length == num_lights
      result2 = solve_part2_ilp(parsed[:buttons], parsed[:joltages])
      part2_total += result2 if result2
    end
  end
  
  [part1_total.to_s, part2_total.to_s]
end

content = File.read("../data/input.txt")
part1, part2 = solve(content)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
