# Day 9 Ruby solution
def solve(input_data)
  lines = input_data.strip.split("\n").map(&:strip).reject(&:empty?)
  
  # Parse coordinates
  red_tiles = []
  lines.each do |line|
    if line.include?(',')
      parts = line.split(',')
      if parts.length == 2
        begin
          x = parts[0].strip.to_i
          y = parts[1].strip.to_i
          red_tiles << [x, y]
        rescue
          # Skip invalid lines
        end
      end
    end
  end
  
  if red_tiles.length < 2
    return ["0", "0"]
  end
  
  # Part 1: Find largest rectangle area using any two red tiles as corners
  max_area_part1 = 0
  (0...red_tiles.length).each do |i|
    (i + 1...red_tiles.length).each do |j|
      x1, y1 = red_tiles[i]
      x2, y2 = red_tiles[j]
      width = (x1 - x2).abs + 1
      height = (y1 - y2).abs + 1
      area = width * height
      max_area_part1 = [max_area_part1, area].max
    end
  end
  
  # Part 2: Coordinate compression + flood-fill + prefix sums
  all_x_set = Set.new
  all_y_set = Set.new
  red_tiles.each do |x, y|
    all_x_set.add(x)
    all_x_set.add(x + 1)
    all_y_set.add(y)
    all_y_set.add(y + 1)
  end
  
  all_x = all_x_set.to_a.sort
  all_y = all_y_set.to_a.sort
  
  x_to_cx = {}
  y_to_cy = {}
  all_x.each_with_index { |x, i| x_to_cx[x] = i }
  all_y.each_with_index { |y, i| y_to_cy[y] = i }
  
  width = all_x.length
  height = all_y.length
  
  # Build grid
  grid = Array.new(width) { Array.new(height, false) }
  
  # Mark boundary
  red_tiles.each do |x, y|
    if x_to_cx[x] && y_to_cy[y]
      grid[x_to_cx[x]][y_to_cy[y]] = true
    end
  end
  
  # Connect consecutive red tiles
  (0...red_tiles.length).each do |i|
    x1, y1 = red_tiles[i]
    x2, y2 = red_tiles[(i + 1) % red_tiles.length]
    
    if x1 == x2
      start_y = [y1, y2].min
      end_y = [y1, y2].max
      (start_y..end_y).each do |y|
        if x_to_cx[x1] && y_to_cy[y]
          grid[x_to_cx[x1]][y_to_cy[y]] = true
        end
      end
    elsif y1 == y2
      start_x = [x1, x2].min
      end_x = [x1, x2].max
      (start_x..end_x).each do |x|
        if x_to_cx[x] && y_to_cy[y1]
          grid[x_to_cx[x]][y_to_cy[y1]] = true
        end
      end
    end
  end
  
  # Point-in-polygon
  point_in_polygon = lambda do |px, py|
    inside = false
    (0...red_tiles.length).each do |i|
      x1, y1 = red_tiles[i]
      x2, y2 = red_tiles[(i + 1) % red_tiles.length]
      if (y1 > py) != (y2 > py)
        intersect_x = if y2 != y1
          (py - y1) * (x2 - x1).to_f / (y2 - y1) + x1
        else
          px.to_f
        end
        if px < intersect_x
          inside = !inside
        end
      end
    end
    inside
  end
  
  # Flood fill interior
  found_interior = false
  (0...width).each do |cx|
    break if found_interior
    (0...height).each do |cy|
      break if found_interior
      if !grid[cx][cy]
        orig_x = all_x[cx]
        orig_y = all_y[cy]
        if point_in_polygon.call(orig_x, orig_y)
          stack = [[cx, cy]]
          while !stack.empty?
            x, y = stack.pop
            if x >= width || y >= height || grid[x][y]
              next
            end
            orig_x = all_x[x]
            orig_y = all_y[y]
            if point_in_polygon.call(orig_x, orig_y)
              grid[x][y] = true
              stack << [x - 1, y] if x > 0
              stack << [x + 1, y] if x + 1 < width
              stack << [x, y - 1] if y > 0
              stack << [x, y + 1] if y + 1 < height
            end
          end
          found_interior = true
        end
      end
    end
  end
  
  # Build 2D prefix sum
  prefix = Array.new(width + 1) { Array.new(height + 1, 0) }
  (0...width).each do |cx|
    (0...height).each do |cy|
      prefix[cx + 1][cy + 1] = prefix[cx][cy + 1] + prefix[cx + 1][cy] - 
                                prefix[cx][cy] + (grid[cx][cy] ? 1 : 0)
    end
  end
  
  rect_sum = lambda do |cx1, cx2, cy1, cy2|
    prefix[cx2 + 1][cy2 + 1] - prefix[cx1][cy2 + 1] - 
    prefix[cx2 + 1][cy1] + prefix[cx1][cy1]
  end
  
  # Generate candidates sorted by area descending
  candidates = []
  (0...red_tiles.length).each do |i|
    (i + 1...red_tiles.length).each do |j|
      x1, y1 = red_tiles[i]
      x2, y2 = red_tiles[j]
      min_x = [x1, x2].min
      max_x = [x1, x2].max
      min_y = [y1, y2].min
      max_y = [y1, y2].max
      area = (max_x - min_x + 1) * (max_y - min_y + 1)
      
      if x_to_cx[min_x] && x_to_cx[max_x] && y_to_cy[min_y] && y_to_cy[max_y]
        cx1 = x_to_cx[min_x]
        cx2 = x_to_cx[max_x]
        cy1 = y_to_cy[min_y]
        cy2 = y_to_cy[max_y]
        candidates << [min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2]
      end
    end
  end
  
  candidates.sort_by! { |c| -c[4] }
  
  # Check candidates
  max_area_part2 = 0
  candidates.each do |min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2|
    break if area <= max_area_part2
    
    valid_count = rect_sum.call(cx1, cx2, cy1, cy2)
    expected_cells = (cx2 - cx1 + 1) * (cy2 - cy1 + 1)
    
    if valid_count == expected_cells
      all_valid = true
      [[min_x, min_y], [min_x, max_y], [max_x, min_y], [max_x, max_y]].each do |x, y|
        if x_to_cx[x] && y_to_cy[y]
          if !grid[x_to_cx[x]][y_to_cy[y]]
            all_valid = false
            break
          end
        else
          if !point_in_polygon.call(x, y)
            all_valid = false
            break
          end
        end
      end
      
      if all_valid
        max_area_part2 = area
        break
      end
    end
  end
  
  [max_area_part1.to_s, max_area_part2.to_s]
end

def main
  require 'set'
  data = File.read("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main
