Code.load_file("../../../utilities/elixir/get_input.ex")

defmodule Solution do
  def solve(lines) do
    # Part 1: Count times dial ends at 0 after a rotation
    part1 = solve_part1(lines, 50, 0)
    
    # Part 2: Count times dial is at 0 during entire process
    part2 = solve_part2(lines, 50, 0)
    
    {part1, part2}
  end
  
  defp solve_part1([], _position, count), do: count
  
  defp solve_part1([line | rest], position, count) do
    line = String.trim(line)
    if line == "" do
      solve_part1(rest, position, count)
    else
      <<direction::binary-size(1)>> <> dist_str = line
      distance = String.to_integer(dist_str)
      
      new_pos = if direction == "L" do
        rem(rem(position - distance, 100) + 100, 100)
      else
        rem(position + distance, 100)
      end
      
      new_count = if new_pos == 0, do: count + 1, else: count
      solve_part1(rest, new_pos, new_count)
    end
  end
  
  defp solve_part2([], _position, count), do: count
  
  defp solve_part2([line | rest], position, count) do
    line = String.trim(line)
    if line == "" do
      solve_part2(rest, position, count)
    else
      <<direction::binary-size(1)>> <> dist_str = line
      distance = String.to_integer(dist_str)
      
      start_pos = position
      
      # Count zeros during rotation
      zeros_in_rotation = 1..distance
        |> Enum.reduce(0, fn click, cnt ->
          click_pos = if direction == "L" do
            rem(rem(start_pos - click, 100) + 100, 100)
          else
            rem(start_pos + click, 100)
          end
          
          if click_pos == 0, do: cnt + 1, else: cnt
        end)
      
      new_pos = if direction == "L" do
        rem(rem(position - distance, 100) + 100, 100)
      else
        rem(position + distance, 100)
      end
      
      solve_part2(rest, new_pos, count + zeros_in_rotation)
    end
  end
end

lines = GetInput.read_input("../data/input.txt")
{part1, part2} = Solution.solve(lines)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
