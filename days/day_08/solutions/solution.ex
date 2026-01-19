defmodule Solution do
  defmodule Coord do
    defstruct [:x, :y, :z]
  end

  defmodule Pair do
    defstruct [:i, :j, :dist_sq]
  end

  def parse_coordinates(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.filter(&(&1 != ""))
    |> Enum.map(fn line ->
      [x, y, z] = line |> String.split(",") |> Enum.map(&String.to_integer/1)
      %Coord{x: x, y: y, z: z}
    end)
  end

  def squared_distance(%Coord{} = p1, %Coord{} = p2) do
    dx = p2.x - p1.x
    dy = p2.y - p1.y
    dz = p2.z - p1.z
    dx * dx + dy * dy + dz * dz
  end

  def solve(input_data) do
    lines = input_data |> String.trim() |> String.split("\n")
    coords = parse_coordinates(lines)
    n = length(coords)

    if n == 0 do
      {"0", "0"}
    else
      # Generate all pairs with squared distances
      pairs =
        if n > 1 do
          0..(n - 1)
          |> Enum.flat_map(fn i ->
            if i + 1 <= n - 1 do
              for j <- (i + 1)..(n - 1) do
                dist_sq = squared_distance(Enum.at(coords, i), Enum.at(coords, j))
                %Pair{i: i, j: j, dist_sq: dist_sq}
              end
            else
              []
            end
          end)
        else
          []
        end
        |> Enum.sort_by(& &1.dist_sq)

      # Part 1: Connect first 1000 pairs
      uf1 = init_union_find(n)
      {uf1, _} =
        pairs
        |> Enum.take(1000)
        |> Enum.reduce({uf1, 0}, fn pair, {uf, conn_count} ->
          {union(uf, pair.i, pair.j), conn_count + 1}
        end)

      # Get component sizes
      component_sizes =
        if n > 0 do
          0..(n - 1)
          |> Enum.reduce(%{}, fn i, acc ->
            root = find(uf1, i)
            Map.update(acc, root, get_size(uf1, root), fn _ -> get_size(uf1, root) end)
          end)
        else
          %{}
        end

      sizes = component_sizes |> Map.values() |> Enum.sort(:desc)
      part1 = if length(sizes) >= 3 do
        [s1, s2, s3 | _] = sizes
        s1 * s2 * s3
      else
        0
      end

      # Part 2: Connect until all in one circuit
      uf2 = init_union_find(n)
      final_pair =
        pairs
        |> Enum.reduce_while({uf2, nil}, fn pair, {uf, _} ->
          if uf.component_count == 1 do
            {:halt, {uf, nil}}
          else
            new_uf = union(uf, pair.i, pair.j)
            if new_uf.component_count == 1 do
              {:halt, {new_uf, pair}}
            else
              {:cont, {new_uf, nil}}
            end
          end
        end)
        |> elem(1)

      part2 = if final_pair do
        coord_i = Enum.at(coords, final_pair.i)
        coord_j = Enum.at(coords, final_pair.j)
        coord_i.x * coord_j.x
      else
        0
      end

      {Integer.to_string(part1), Integer.to_string(part2)}
    end
  end

  def init_union_find(n) do
    %{
      parent: for(i <- 0..(n - 1), do: i) |> List.to_tuple(),
      size: for(_ <- 1..n, do: 1) |> List.to_tuple(),
      component_count: n
    }
  end

  def find(uf, x) do
    p = elem(uf.parent, x)
    if p == x do
      x
    else
      root = find(uf, p)
      # Path compression not shown here for simplicity (would need mutable state)
      root
    end
  end

  def get_size(uf, root) do
    elem(uf.size, root)
  end

  def union(uf, x, y) do
    root_x = find(uf, x)
    root_y = find(uf, y)
    if root_x == root_y do
      uf
    else
      size_x = get_size(uf, root_x)
      size_y = get_size(uf, root_y)
      {rx, ry} = if size_x < size_y, do: {root_y, root_x}, else: {root_x, root_y}
      new_parent = uf.parent |> Tuple.to_list() |> List.replace_at(ry, rx) |> List.to_tuple()
      new_size = uf.size |> Tuple.to_list() |> List.replace_at(rx, get_size(uf, rx) + get_size(uf, ry)) |> List.to_tuple()
      %{uf | parent: new_parent, size: new_size, component_count: uf.component_count - 1}
    end
  end
end

{part1, part2} =
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
