defmodule Solution do
  def solve(input_data) do
    lines = String.split(String.trim(input_data), "\n")

    # Find blank line separator
    blank_idx = Enum.find_index(lines, fn line -> String.trim(line) == "" end) || length(lines)

    # Parse ranges (first section)
    ranges = lines
      |> Enum.take(blank_idx)
      |> Enum.filter(&(String.trim(&1) != ""))
      |> Enum.map(fn line ->
        [start_str, end_str] = String.split(line, "-")
        {String.to_integer(start_str), String.to_integer(end_str)}
      end)

    # Parse IDs to check (second section)
    ids = lines
      |> Enum.drop(blank_idx + 1)
      |> Enum.filter(&(String.trim(&1) != ""))
      |> Enum.map(&String.to_integer/1)

    # Part 1: Count how many IDs fall into any range
    part1_count = Enum.count(ids, fn id_val ->
      Enum.any?(ranges, fn {start, e} -> start <= id_val && id_val <= e end)
    end)

    # Part 2: Merge ranges and count total unique IDs covered
    # Sort ranges by start value
    sorted_ranges = Enum.sort_by(ranges, fn {start, _} -> start end)

    # Merge overlapping/adjacent ranges
    merged = if sorted_ranges == [] do
      []
    else
      {merged_ranges, _} = Enum.reduce(sorted_ranges, {[], nil}, fn {curr_start, curr_end}, {acc, last} ->
        if last == nil do
          {[[curr_start, curr_end]], {curr_start, curr_end}}
        else
          {last_start, last_end} = last
          if curr_start <= last_end + 1 do
            # Merge: update end to max of both ends
            new_last = {last_start, max(last_end, curr_end)}
            new_acc = List.replace_at(acc, length(acc) - 1, [last_start, max(last_end, curr_end)])
            {new_acc, new_last}
          else
            # No overlap, add as new range
            {acc ++ [[curr_start, curr_end]], {curr_start, curr_end}}
          end
        end
      end)
      merged_ranges
    end

    # Calculate total unique IDs covered
    part2_total = Enum.reduce(merged, 0, fn [start, e], sum -> sum + (e - start + 1) end)

    {part1_count, part2_total}
  end
end

content = File.read!("../data/input.txt")
{part1, part2} = Solution.solve(content)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
