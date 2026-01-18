def read_input_raw(file_path)
  File.read(file_path)
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  return ["0", "0"] if lines.empty?

  # Find maximum line length and pad all lines
  max_len = lines.map(&:length).max
  padded_lines = lines.map { |line| line.ljust(max_len) }

  # Operator row is the last row
  op_row_idx = padded_lines.length - 1
  op_row = padded_lines[op_row_idx]
  num_rows = padded_lines[0...op_row_idx]

  # Part 1: Parse horizontally
  part1_total = 0

  # Find problem boundaries (columns that are all spaces)
  is_space_col = (0...max_len).map do |col|
    padded_lines.all? { |line| line[col] == ' ' }
  end

  # Group columns into problems
  problems = []
  i = 0
  while i < max_len
    unless is_space_col[i]
      # Start of a problem
      start_col = i
      while i < max_len && !is_space_col[i]
        i += 1
      end
      end_col = i
      # Extract operator for this problem
      op = nil
      (start_col...end_col).each do |j|
        if op_row[j] == '+' || op_row[j] == '*'
          op = op_row[j]
          break
        end
      end
      if op
        problems << [start_col, end_col, op]
      end
    else
      i += 1
    end
  end

  # Solve Part 1: Extract numbers horizontally
  problems.each do |start_col, end_col, op|
    numbers = []
    # Extract numbers from each row in this problem area
    num_rows.each do |row|
      # Extract the substring for this problem
      problem_str = row[start_col...end_col].strip
      # Split by spaces and parse numbers
      parts = problem_str.split
      parts.each do |part|
        num = part.to_i
        numbers << num if num != 0 || part == "0"
      end
    end

    # Apply operator
    if numbers.length > 0
      result = if op == '+'
                 numbers.sum
               else
                 numbers.reduce(1, :*)
               end
      part1_total += result
    end
  end

  # Part 2: Parse vertically (columns, right-to-left)
  # Approach: Use Ruby's elegant array methods to transpose and map
  part2_total = 0
  
  problems.each do |start_col, end_col, op|
    # Transpose columns to rows, filtering spaces
    col_strings = (start_col...end_col)
      .reject { |col| is_space_col[col] }
      .map do |col|
        num_rows.map { |row| row[col] || ' ' }
          .join
          .strip
          .gsub(/\s/, '')
      end
      .reverse  # Right-to-left means reverse order
    
    # Parse each column string as a number
    numbers = col_strings
      .select { |s| !s.empty? }
      .map(&:to_i)
      .select { |n| n > 0 }
    
    # Apply operator
    if numbers.length > 0
      result = if op == '+'
                 numbers.sum
               else
                 numbers.reduce(1, :*)
               end
      part2_total += result
    end
  end

  [part1_total.to_s, part2_total.to_s]
end

content = read_input_raw("../data/input.txt")
part1, part2 = solve(content)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
