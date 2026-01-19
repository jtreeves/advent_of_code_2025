def solve(input_data)
  lines = input_data.strip.split("\n")
  
  # Find blank line separator
  blank_idx = lines.index { |line| line.strip.empty? } || lines.size
  
  # Parse ranges (first section)
  ranges = []
  (0...blank_idx).each do |i|
    line = lines[i].strip
    next if line.empty?
    parts = line.split('-')
    start = parts[0].to_i
    e = parts[1].to_i
    ranges << [start, e]
  end
  
  # Parse IDs to check (second section)
  ids = []
  ((blank_idx + 1)...lines.size).each do |i|
    line = lines[i].strip
    next if line.empty?
    ids << line.to_i
  end
  
  # Part 1: Count how many IDs fall into any range
  part1_count = 0
  ids.each do |id_val|
    ranges.each do |start, e|
      if start <= id_val && id_val <= e
        part1_count += 1
        break
      end
    end
  end
  
  # Part 2: Merge ranges and count total unique IDs covered
  # Sort ranges by start value
  sorted_ranges = ranges.sort_by { |r| r[0] }
  
  # Merge overlapping/adjacent ranges
  merged = []
  if !sorted_ranges.empty?
    merged << [sorted_ranges[0][0], sorted_ranges[0][1]]
    (1...sorted_ranges.size).each do |i|
      curr_start, curr_end = sorted_ranges[i]
      last = merged.last
      # Check if overlaps or is adjacent (curr_start <= last[1] + 1)
      if curr_start <= last[1] + 1
        # Merge: update end to max of both ends
        last[1] = [last[1], curr_end].max
      else
        # No overlap, add as new range
        merged << [curr_start, curr_end]
      end
    end
  end
  
  # Calculate total unique IDs covered
  part2_total = merged.sum { |start, e| e - start + 1 }
  
  [part1_count, part2_total]
end

content = File.read("../data/input.txt")
part1, part2 = solve(content)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
