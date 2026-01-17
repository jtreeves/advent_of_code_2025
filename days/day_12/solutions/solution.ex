# Placeholder for Day 12 Elixir solution
defmodule Solution do
  def solve(input_data) do
    IO.puts("Day 12 Elixir placeholder")
    lines = input_data |> String.trim() |> String.split("\n")
    IO.inspect(lines, label: "Lines")
    
    # Part 1
    part1_result = "TODO"
    
    # Part 2
    part2_result = "TODO"
    
    {part1_result, part2_result}
  end
end

{part1, part2} = 
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
