defmodule Solution do
  def solve(input_data) do
    lines = String.trim(input_data) |> String.split("\n")

    # Build graph: device -> list of outputs
    graph = Enum.reduce(lines, %{}, fn line, acc ->
      case String.split(line, ":", parts: 2) do
        [device, outputs_str] ->
          device = String.trim(device)
          outputs_str = String.trim(outputs_str)
          outputs = if outputs_str == "", do: [], else: String.split(outputs_str, ~r/\s+/)
          Map.put(acc, device, outputs)
        _ ->
          acc
      end
    end)

    # Part 1: Count paths from "you" to "out"
    {part1_count, _} = if Map.has_key?(graph, "you") do
      count_paths_part1("you", graph, %{})
    else
      {0, %{}}
    end

    # Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
    {part2_count, _} = if Map.has_key?(graph, "svr") do
      count_paths_part2("svr", false, false, graph, %{})
    else
      {0, %{}}
    end

    {to_string(part1_count), to_string(part2_count)}
  end

  defp count_paths_part1(node, graph, memo) do
    if node == "out" do
      {1, memo}
    else
      case Map.fetch(memo, node) do
        {:ok, value} -> {value, memo}
        :error ->
          neighbors = Map.get(graph, node, [])
          {count, new_memo} = Enum.reduce(neighbors, {0, memo}, fn neighbor, {acc, mem} ->
            {val, updated_mem} = count_paths_part1(neighbor, graph, mem)
            {acc + val, updated_mem}
          end)
          final_memo = Map.put(new_memo, node, count)
          {count, final_memo}
      end
    end
  end

  defp count_paths_part2(node, visited_fft, visited_dac, graph, memo) do
    if node == "out" do
      result = if visited_fft && visited_dac, do: 1, else: 0
      {result, memo}
    else
      key = {node, visited_fft, visited_dac}
      case Map.fetch(memo, key) do
        {:ok, value} -> {value, memo}
        :error ->
          new_visited_fft = visited_fft || (node == "fft")
          new_visited_dac = visited_dac || (node == "dac")
          neighbors = Map.get(graph, node, [])
          {count, new_memo} = Enum.reduce(neighbors, {0, memo}, fn neighbor, {acc, mem} ->
            {val, updated_mem} = count_paths_part2(neighbor, new_visited_fft, new_visited_dac, graph, mem)
            {acc + val, updated_mem}
          end)
          final_memo = Map.put(new_memo, key, count)
          {count, final_memo}
      end
    end
  end
end

input_data = File.read!("../data/input.txt")
{part1, part2} = Solution.solve(input_data)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
