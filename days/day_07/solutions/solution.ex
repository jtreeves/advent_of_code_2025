defmodule Solution do
  def solve(input_data) do
    lines = String.split(String.trim(input_data), "\n")
    if lines == [], do: {"0", "0"}, else: do_solve(lines)
  end
  
  defp do_solve(lines) do
    grid = Enum.map(lines, &String.to_charlist/1)
    rows = length(grid)
    cols = if rows > 0, do: length(hd(grid)), else: 0
    
    # Find starting position S
    start_pos = find_start(grid, 0, 0, rows, cols)
    {start_row, start_col} = start_pos
    
    if start_row == -1 do
      {"0", "0"}
    else
      # Part 1: Count total splits
      split_count = count_splits(grid, start_row, start_col, rows, cols)
      
      # Part 2: Count beams reaching bottom row
      beam_counts = process_beams(grid, start_row, start_col, rows, cols)
      bottom_beam_count = Enum.sum(Enum.at(beam_counts, rows - 1))
      
      {Integer.to_string(split_count), Integer.to_string(bottom_beam_count)}
    end
  end
  
  defp find_start(_grid, r, _c, rows, _cols) when r >= rows, do: {-1, -1}
  defp find_start(grid, r, c, rows, cols) do
    row = Enum.at(grid, r)
    if c >= length(row) do
      find_start(grid, r + 1, 0, rows, cols)
    else
      if Enum.at(row, c) == ?S do
        {r, c}
      else
        find_start(grid, r, c + 1, rows, cols)
      end
    end
  end
  
  defp count_splits(grid, start_row, start_col, rows, cols) do
    count_splits_impl(grid, start_row + 1, MapSet.new([start_col]), rows, cols, 0)
  end
  
  defp count_splits_impl(_grid, r, _active_beams, rows, _cols, count) when r >= rows, do: count
  defp count_splits_impl(grid, r, active_beams, rows, cols, count) do
    row = Enum.at(grid, r)
    {next_beams, new_count} = Enum.reduce(active_beams, {MapSet.new(), count}, fn col, {acc, cnt} ->
      cell = Enum.at(row, col)
      cond do
        cell == ?. -> {MapSet.put(acc, col), cnt}
        cell == ?^ ->
          beams = acc
            |> (fn a -> if col - 1 >= 0, do: MapSet.put(a, col - 1), else: a end).()
            |> (fn a -> if col + 1 < cols, do: MapSet.put(a, col + 1), else: a end).()
          {beams, cnt + 1}
        true -> {acc, cnt}
      end
    end)
    count_splits_impl(grid, r + 1, next_beams, rows, cols, new_count)
  end
  
  defp process_beams(grid, start_row, start_col, rows, cols) do
    # Initialize beam counts as list of lists
    initial = for r <- 0..(rows - 1) do
      for c <- 0..(cols - 1) do
        if r == start_row && c == start_col, do: 1, else: 0
      end
    end
    process_beams_impl(grid, initial, start_row + 1, rows, cols)
  end
  
  defp process_beams_impl(_grid, beam_counts, r, rows, _cols) when r >= rows, do: beam_counts
  defp process_beams_impl(grid, beam_counts, r, rows, cols) do
    row = Enum.at(grid, r)
    updated_counts = Enum.reduce(0..(cols - 1), beam_counts, fn c, acc ->
      prev_row = Enum.at(acc, r - 1)
      prev_count = Enum.at(prev_row, c)
      if prev_count > 0 do
        cell = Enum.at(row, c)
        current_row = Enum.at(acc, r)
        cond do
          cell == ?. ->
            new_row = List.replace_at(current_row, c, Enum.at(current_row, c) + prev_count)
            List.replace_at(acc, r, new_row)
          cell == ?^ ->
            acc = if c - 1 >= 0 do
              current_row = Enum.at(acc, r)
              new_row = List.replace_at(current_row, c - 1, Enum.at(current_row, c - 1) + prev_count)
              List.replace_at(acc, r, new_row)
            else
              acc
            end
            if c + 1 < cols do
              current_row = Enum.at(acc, r)
              new_row = List.replace_at(current_row, c + 1, Enum.at(current_row, c + 1) + prev_count)
              List.replace_at(acc, r, new_row)
            else
              acc
            end
          true -> acc
        end
      else
        acc
      end
    end)
    process_beams_impl(grid, updated_counts, r + 1, rows, cols)
  end
end

content = File.read!("../data/input.txt")
{part1, part2} = Solution.solve(content)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
