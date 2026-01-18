# Check if ID is invalid for Part 1: exactly two identical sequences
defmodule Solution do
  defp invalid_part1?(id_str) do
    n = String.length(id_str)
    if rem(n, 2) == 0 do
      half = div(n, 2)
      first_half = String.slice(id_str, 0, half)
      second_half = String.slice(id_str, half, half)
      first_half == second_half
    else
      false
    end
  end
  
  # Check if ID is invalid for Part 2: sequence repeated 2+ times
  defp invalid_part2?(id_str) do
    n = String.length(id_str)
    Enum.any?(2..n, fn k ->
      if rem(n, k) == 0 do
        seq_len = div(n, k)
        pattern = String.slice(id_str, 0, seq_len)
        repeated = String.duplicate(pattern, k)
        id_str == repeated
      else
        false
      end
    end)
  end
  
  # Parse a range string like "start-end"
  defp parse_range(range_str) do
    [start_str, end_str] = String.split(String.trim(range_str), "-")
    {String.to_integer(String.trim(start_str)), String.to_integer(String.trim(end_str))}
  end
  
  # Parse a line of comma-separated ranges
  defp parse_ranges(line) do
    line
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(&parse_range/1)
  end
  
  def solve(input_data) do
    lines = input_data |> String.trim() |> String.split("\n")
    
    {part1_sum, part2_sum} = 
      Enum.reduce(lines, {0, 0}, fn line, {p1_acc, p2_acc} ->
        if String.trim(line) == "" do
          {p1_acc, p2_acc}
        else
          ranges = parse_ranges(line)
          
          {p1_sum, p2_sum} = 
            Enum.reduce(ranges, {0, 0}, fn {start, end_val}, {acc1, acc2} ->
              {sum1, sum2} = 
                Enum.reduce(start..end_val, {0, 0}, fn num, {a1, a2} ->
                  id_str = Integer.to_string(num)
                  {
                    a1 + if(invalid_part1?(id_str), do: num, else: 0),
                    a2 + if(invalid_part2?(id_str), do: num, else: 0)
                  }
                end)
              {acc1 + sum1, acc2 + sum2}
            end)
          
          {p1_acc + p1_sum, p2_acc + p2_sum}
        end
      end)
    
    {Integer.to_string(part1_sum), Integer.to_string(part2_sum)}
  end
end

{part1, part2} = 
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
