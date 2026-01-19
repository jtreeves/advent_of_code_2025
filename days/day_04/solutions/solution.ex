defmodule Solution do
  def count_neighbors(grid, i, j, rows, cols) do
    Enum.sum(for di <- -1..1, dj <- -1..1, not (di == 0 and dj == 0) do
      ni = i + di
      nj = j + dj
      if ni >= 0 and ni < rows and nj >= 0 and nj < cols and
         Enum.at(Enum.at(grid, ni), nj) == "@" do
        1
      else
        0
      end
    end)
  end

  def solve(lines) do
    rows = length(lines)
    cols = if rows > 0, do: String.length(hd(lines)), else: 0

    # Part 1: Count accessible rolls (fewer than 4 neighbors that are '@')
    part1 = Enum.sum(for i <- 0..(rows-1), j <- 0..(cols-1) do
      line = Enum.at(lines, i)
      if String.at(line, j) == "@" do
        neighbors = count_neighbors(lines, i, j, rows, cols)
        if neighbors < 4, do: 1, else: 0
      else
        0
      end
    end)

    # Part 2: Iteratively remove accessible rolls until none can be removed
    grid = Enum.map(lines, &String.graphemes/1) |> Enum.map(&List.to_tuple/1)
    {part2, _} = remove_iteratively(grid, rows, cols, 0)

    {part1, part2}
  end

  defp remove_iteratively(grid, rows, cols, count) do
    to_remove = for i <- 0..(rows-1), j <- 0..(cols-1), into: [] do
      if elem(elem(grid, i), j) == "@" do
        grid_list = Enum.map(0..(rows-1), fn r ->
          Tuple.to_list(elem(grid, r))
        end)
        neighbors = count_neighbors(grid_list, i, j, rows, cols)
        if neighbors < 4, do: {i, j}, else: nil
      else
        nil
      end
    end |> Enum.filter(&(&1 != nil))

    if Enum.empty?(to_remove) do
      {count, grid}
    else
      new_grid = Enum.reduce(to_remove, grid, fn {i, j}, g ->
        row = elem(g, i)
        new_row = put_elem(row, j, ".")
        put_elem(g, i, new_row)
      end)
      remove_iteratively(new_grid, rows, cols, count + length(to_remove))
    end
  end
end

lines = "../data/input.txt" |> File.read!() |> String.trim() |> String.split("\n")
{part1, part2} = Solution.solve(lines)
IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
