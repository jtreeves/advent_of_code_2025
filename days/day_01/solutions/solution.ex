# Placeholder for Day 01 Elixir solution
# Note: Elixir utility functions would be in utilities/elixir/get_input.ex
# For now, using inline function - will be replaced with proper requires

Code.require_file("../../../utilities/elixir/get_input.ex")

defmodule Solution do
  def solve(input_data) do
    IO.puts("Day 01 Elixir placeholder")
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
  |> GetInput.read_input_raw()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
