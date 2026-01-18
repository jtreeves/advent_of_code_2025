defmodule Solution do
  def solve(input_data) do
    lines = String.split(String.trim(input_data), "\n")
    if lines == [], do: {0, 0}, else: do_solve(lines)
  end
  
  defp do_solve(lines) do
    # Find maximum line length and pad all lines
    max_len = lines |> Enum.map(&String.length/1) |> Enum.max()
    padded_lines = lines |> Enum.map(&String.pad_trailing(&1, max_len))
    
    # Operator row is the last row
    op_row_idx = length(padded_lines) - 1
    op_row = Enum.at(padded_lines, op_row_idx)
    num_rows = Enum.take(padded_lines, op_row_idx)
    
    # Find problem boundaries (columns that are all spaces)
    is_space_col = 0..(max_len - 1)
      |> Enum.map(fn col ->
        Enum.all?(padded_lines, fn line ->
          col >= String.length(line) || String.at(line, col) == " "
        end)
      end)
    
    # Group columns into problems
    problems = find_problems(0, max_len, is_space_col, op_row, [])
    
    # Part 1: Extract numbers horizontally
    part1_total = problems
      |> Enum.map(fn {start_col, end_col, op} ->
        numbers = num_rows
          |> Enum.flat_map(fn row ->
            problem_str = row
              |> String.slice(start_col, end_col - start_col)
              |> String.trim()
            problem_str
              |> String.split(~r/\s+/)
              |> Enum.filter(&(&1 != ""))
              |> Enum.map(&String.to_integer/1)
          end)
        
        if numbers != [] do
          result = case op do
            "+" -> Enum.sum(numbers)
            "*" -> Enum.product(numbers)
          end
          result
        else
          0
        end
      end)
      |> Enum.sum()
    
    # Part 2: Parse vertically (columns, right-to-left)
    # Approach: Use Elixir pipes and Enum functions
    part2_total = problems
      |> Enum.map(fn {start_col, end_col, op} ->
        # Extract column strings using pipes
        col_strings = start_col..(end_col - 1)
          |> Enum.reject(fn col -> Enum.at(is_space_col, col) end)
          |> Enum.map(fn col ->
            num_rows
              |> Enum.map(fn row -> String.at(row, col) || " " end)
              |> Enum.filter(fn ch -> Regex.match?(~r/[\d\s]/, ch) end)
              |> Enum.join("")
              |> String.trim()
          end)
          |> Enum.filter(&(&1 != ""))
          |> Enum.reverse()  # Right-to-left reading
        
        # Parse numbers
        numbers = col_strings
          |> Enum.map(&String.replace(&1, ~r/\s/, ""))
          |> Enum.filter(&(&1 != ""))
          |> Enum.map(&String.to_integer/1)
        
        # Apply operator
        if numbers != [] do
          result = case op do
            "+" -> Enum.sum(numbers)
            "*" -> Enum.product(numbers)
          end
          result
        else
          0
        end
      end)
      |> Enum.sum()
    
    {part1_total, part2_total}
  end
  
  defp find_problems(i, max_len, _is_space_col, _op_row, acc) when i >= max_len, do: acc
  
  defp find_problems(i, max_len, is_space_col, op_row, acc) do
    if Enum.at(is_space_col, i) do
      find_problems(i + 1, max_len, is_space_col, op_row, acc)
    else
      start_col = i
      {end_col, new_i} = find_problem_end(i, max_len, is_space_col)
      
      op = op_row
        |> String.slice(start_col, end_col - start_col)
        |> String.to_charlist()
        |> Enum.find(fn ch -> ch == ?+ || ch == ?* end)
      
      new_acc = if op do
        [{start_col, end_col, List.to_string([op])} | acc]
      else
        acc
      end
      
      find_problems(new_i, max_len, is_space_col, op_row, new_acc) |> Enum.reverse()
    end
  end
  
  defp find_problem_end(i, max_len, is_space_col) do
    if i >= max_len || Enum.at(is_space_col, i) do
      {i, i}
    else
      find_problem_end(i + 1, max_len, is_space_col)
    end
  end
end

content = File.read!("../data/input.txt")
{part1, part2} = Solution.solve(content)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
