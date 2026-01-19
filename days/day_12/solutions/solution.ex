defmodule Solution do
  def solve(input_data) do
    lines = String.split(String.trim(input_data), "\n")
    if lines == [], do: {"0", "Final star"}, else: do_solve(lines)
  end
  
  defp do_solve(lines) do
    # Parse shapes (first 6 shapes, numbered 0-5)
    {shape_areas, query_start} = parse_shapes(lines, 0, 0, [])
    
    # Parse queries
    possible_count = count_possible_queries(Enum.drop(lines, query_start), shape_areas)
    
    # Part 2: Final star (no computation needed)
    part2 = "Final star"
    
    {Integer.to_string(possible_count), part2}
  end
  
  defp parse_shapes([], _i, _shape_idx, acc), do: {Enum.reverse(acc), 0}
  defp parse_shapes(_lines, _i, 6, acc), do: {Enum.reverse(acc), 0}
  defp parse_shapes(lines, i, shape_idx, acc) when i >= length(lines), do: {Enum.reverse(acc), i}
  
  defp parse_shapes(lines, i, shape_idx, acc) do
    line = Enum.at(lines, i) |> String.trim()
    cond do
      String.ends_with?(line, ":") ->
        shape_num_str = String.slice(line, 0, String.length(line) - 1)
        case Integer.parse(shape_num_str) do
          {shape_num, ""} when shape_num == shape_idx ->
            # Read the next 3 lines for the shape grid
            shape_grid = Enum.map(0..2, fn j ->
              if i + 1 + j < length(lines) do
                Enum.at(lines, i + 1 + j) |> String.trim()
              else
                ""
              end
            end)
            
            # Count '#' characters in the shape
            area = Enum.sum(Enum.map(shape_grid, fn row ->
              String.length(row) - String.length(String.replace(row, "#", ""))
            end))
            
            parse_shapes(lines, i + 4, shape_idx + 1, [area | acc])
          _ ->
            parse_shapes(lines, i + 1, shape_idx, acc)
        end
      true ->
        parse_shapes(lines, i + 1, shape_idx, acc)
    end
  end
  
  defp count_possible_queries([], _shape_areas), do: 0
  defp count_possible_queries([line | rest], shape_areas) do
    line = String.trim(line)
    cond do
      line == "" ->
        count_possible_queries(rest, shape_areas)
      String.contains?(line, ":") ->
        [dims, counts_str] = String.split(line, ":", parts: 2)
        dims = String.trim(dims)
        counts_str = String.trim(counts_str)
        
        cond do
          String.contains?(dims, "x") ->
            [width_str, height_str] = String.split(dims, "x")
            case {Integer.parse(width_str), Integer.parse(height_str)} do
              {{width, ""}, {height, ""}} ->
                count_parts = String.split(counts_str, ~r/\s+/)
                if length(count_parts) == 6 do
                  counts = Enum.map(count_parts, &String.to_integer/1)
                  
                  # Calculate area check
                  region_area = width * height
                  required_area = Enum.sum(Enum.zip_with(shape_areas, counts, fn s, c -> s * c end))
                  
                  count = if required_area <= region_area, do: 1, else: 0
                  count + count_possible_queries(rest, shape_areas)
                else
                  count_possible_queries(rest, shape_areas)
                end
              _ ->
                count_possible_queries(rest, shape_areas)
            end
          true ->
            count_possible_queries(rest, shape_areas)
        end
      true ->
        count_possible_queries(rest, shape_areas)
    end
  end
end

{:ok, content} = File.read("../data/input.txt")
{part1, part2} = Solution.solve(content)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
