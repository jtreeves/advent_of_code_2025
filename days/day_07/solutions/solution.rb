require 'set'

def read_input_raw(file_path)
  File.read(file_path)
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  return ["0", "0"] if lines.empty?

  grid = lines.map { |line| line.chars }
  rows = grid.length
  cols = rows > 0 ? grid[0].length : 0

  # Find starting position S
  start_row = start_col = nil
  grid.each_with_index do |row, r|
    row.each_with_index do |cell, c|
      if cell == 'S'
        start_row = r
        start_col = c
        break
      end
    end
    break if start_row
  end

  return ["0", "0"] unless start_row

  # Part 1: Count total splits
  split_count = 0
  active_beams = Set.new([start_col])

  # Process each row starting from the row after S
  (start_row + 1...rows).each do |r|
    next_beams = Set.new
    active_beams.each do |col|
      if grid[r][col] == '.'
        # Beam continues down
        next_beams.add(col)
      elsif grid[r][col] == '^'
        # Beam splits
        split_count += 1
        # Add beams to left and right
        next_beams.add(col - 1) if col - 1 >= 0
        next_beams.add(col + 1) if col + 1 < cols
      end
    end
    active_beams = next_beams
  end

  # Part 2: Count beams reaching bottom row
  beam_counts = Array.new(rows) { Array.new(cols, 0) }
  beam_counts[start_row][start_col] = 1 # Start with 1 beam at S

  # Process each row starting from the row after S
  (start_row + 1...rows).each do |r|
    (0...cols).each do |c|
      prev_count = beam_counts[r - 1][c]
      if prev_count > 0
        if grid[r][c] == '.'
          # Beam continues down
          beam_counts[r][c] += prev_count
        elsif grid[r][c] == '^'
          # Beam splits into left and right
          beam_counts[r][c - 1] += prev_count if c - 1 >= 0
          beam_counts[r][c + 1] += prev_count if c + 1 < cols
        end
      end
    end
  end

  # Sum all beams in bottom row
  bottom_beam_count = beam_counts[rows - 1].sum

  [split_count.to_s, bottom_beam_count.to_s]
end

content = read_input_raw("../data/input.txt")
part1, part2 = solve(content)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
