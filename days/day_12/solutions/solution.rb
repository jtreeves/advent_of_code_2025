def read_input_raw(file_path)
  File.read(file_path)
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  return ["0", "Final star"] if lines.empty?
  
  # Parse shapes (first 6 shapes, numbered 0-5)
  shape_areas = [0] * 6
  i = 0
  shape_idx = 0
  
  while i < lines.length && shape_idx < 6
    line = lines[i].strip
    # Check if this is a shape header (format: "number:")
    if line.length > 0 && line.end_with?(':')
      begin
        shape_num = Integer(line[0..-2])
        if shape_num == shape_idx
          # Read the next 3 lines for the shape grid
          shape_grid = []
          (0...3).each do |j|
            if i + 1 + j < lines.length
              shape_grid << lines[i + 1 + j].strip
            else
              shape_grid << ""
            end
          end
          
          # Count '#' characters in the shape
          area = 0
          shape_grid.each do |row|
            area += row.count('#')
          end
          shape_areas[shape_idx] = area
          shape_idx += 1
          i += 4 # Skip shape header + 3 grid lines + empty line (if present)
          next
        end
      rescue ArgumentError
        # Not a valid shape number
      end
    end
    i += 1
  end
  
  # Find where queries start (skip empty lines after shapes)
  query_start = i
  while query_start < lines.length && lines[query_start].strip.empty?
    query_start += 1
  end
  
  # Parse queries
  possible_count = 0
  (query_start...lines.length).each do |line_idx|
    line = lines[line_idx].strip
    next if line.empty?
    
    # Parse query: "widthxheight: count0 count1 count2 count3 count4 count5"
    next unless line.include?(':')
    
    parts = line.split(':', 2)
    next if parts.length != 2
    
    # Parse dimensions
    dims = parts[0].strip
    next unless dims.include?('x')
    
    dim_parts = dims.split('x')
    next if dim_parts.length != 2
    
    begin
      width = Integer(dim_parts[0])
      height = Integer(dim_parts[1])
    rescue ArgumentError
      next
    end
    
    # Parse counts
    count_parts = parts[1].strip.split
    next if count_parts.length != 6
    
    counts = []
    valid_counts = true
    begin
      count_parts.each do |c|
        counts << Integer(c)
      end
    rescue ArgumentError
      valid_counts = false
    end
    
    next unless valid_counts
    
    # Calculate area check
    region_area = width * height
    required_area = 0
    (0...6).each do |j|
      required_area += shape_areas[j] * counts[j]
    end
    
    possible_count += 1 if required_area <= region_area
  end
  
  # Part 2: Final star (no computation needed)
  part2 = "Final star"
  
  [possible_count.to_s, part2]
end

if __FILE__ == $0
  data = read_input_raw("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end
