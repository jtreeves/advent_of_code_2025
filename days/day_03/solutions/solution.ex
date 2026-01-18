# Find the largest N-digit number by selecting N digits in order from bank
defmodule Solution do
  def find_largest_subsequence(bank, n) when byte_size(bank) < n, do: 0
  
  def find_largest_subsequence(bank, n) do
    bank_len = String.length(bank)
    
    {result, _} = Enum.reduce(0..(n - 1), {"", 0}, fn i, {acc, start} ->
      remaining_needed = n - i - 1
      end_pos = bank_len - remaining_needed
      
      candidates = String.slice(bank, start, end_pos - start)
      max_char = String.graphemes(candidates) |> Enum.max()
      candidates_list = String.graphemes(candidates)
      max_pos = start + Enum.find_index(candidates_list, &(&1 == max_char))
      
      {acc <> max_char, max_pos + 1}
    end)
    
    String.to_integer(result)
  end
  
  def solve(input_data) do
    banks = input_data
            |> String.trim()
            |> String.split("\n")
            |> Enum.filter(&(&1 != ""))
    
    part1_sum = banks
                |> Enum.map(&find_largest_subsequence(&1, 2))
                |> Enum.sum()
    
    part2_sum = banks
                |> Enum.filter(&(String.length(&1) >= 12))
                |> Enum.map(&find_largest_subsequence(&1, 12))
                |> Enum.sum()
    
    {to_string(part1_sum), to_string(part2_sum)}
  end
end

{part1, part2} = 
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
