require_relative '../../../utilities/ruby/get_input.rb'

def count_neighbors(grid, i, j, rows, cols)
  count = 0
  (-1..1).each do |di|
    (-1..1).each do |dj|
      next if di == 0 && dj == 0
      ni = i + di
      nj = j + dj
      if ni >= 0 && ni < rows && nj >= 0 && nj < cols && grid[ni][nj] == '@'
        count += 1
      end
    end
  end
  count
end

def solve(lines)
  rows = lines.size
  cols = rows > 0 ? lines[0].length : 0
  
  # Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
  part1_count = 0
  (0...rows).each do |i|
    (0...cols).each do |j|
      if lines[i][j] == '@'
        neighbors = count_neighbors(lines, i, j, rows, cols)
        if neighbors < 4
          part1_count += 1
        end
      end
    end
  end
  
  # Part 2: Iteratively remove accessible rolls until none can be removed
  grid = lines.map { |line| line.dup }
  
  part2_count = 0
  loop do
    to_remove = []
    (0...rows).each do |i|
      (0...cols).each do |j|
        if grid[i][j] == '@'
          neighbors = count_neighbors(grid, i, j, rows, cols)
          if neighbors < 4
            to_remove << [i, j]
          end
        end
      end
    end
    
    break if to_remove.empty?
    
    # Remove all marked positions
    to_remove.each do |i, j|
      grid[i][j] = '.'
    end
    part2_count += to_remove.size
  end
  
  [part1_count, part2_count]
end

lines = get_input(4)
part1, part2 = solve(lines)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
