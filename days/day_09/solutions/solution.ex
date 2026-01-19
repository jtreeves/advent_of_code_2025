# Day 9 Elixir solution
defmodule Solution do
  def parse_coordinates(lines) do
    lines
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.filter(&String.contains?(&1, ","))
    |> Enum.map(fn line ->
      case String.split(line, ",") do
        [x_str, y_str] ->
          try do
            {String.to_integer(String.trim(x_str)), String.to_integer(String.trim(y_str))}
          rescue
            ArgumentError -> nil
          end
        _ -> nil
      end
    end)
    |> Enum.filter(&(&1 != nil))
  end

  def point_in_polygon(px, py, polygon) do
    n = length(polygon)
    edges = Enum.with_index(polygon)
           |> Enum.map(fn {point, i} -> {point, Enum.at(polygon, rem(i + 1, n))} end)

    intersections = Enum.count(edges, fn {{x1, y1}, {x2, y2}} ->
      cond do
        (y1 > py) != (y2 > py) ->
          intersect_x = if y2 != y1 do
            (py - y1) * (x2 - x1) / (y2 - y1) + x1
          else
            px
          end
          px < intersect_x
        true -> false
      end
    end)

    rem(intersections, 2) == 1
  end

  def solve(input_data) do
    lines = input_data |> String.trim() |> String.split("\n")
    red_tiles = parse_coordinates(lines)

    if length(red_tiles) < 2 do
      {"0", "0"}
    else
      # Part 1: Find largest rectangle area
      max_area_part1 = red_tiles
        |> Enum.flat_map(fn {x1, y1} ->
          Enum.map(red_tiles, fn {x2, y2} ->
            if {x1, y1} < {x2, y2} do
              width = abs(x1 - x2) + 1
              height = abs(y1 - y2) + 1
              width * height
            else
              0
            end
          end)
        end)
        |> Enum.max()

      # Part 2: Coordinate compression + flood-fill + prefix sums
      all_x_set = red_tiles
        |> Enum.flat_map(fn {x, _} -> [x, x + 1] end)
        |> MapSet.new()

      all_y_set = red_tiles
        |> Enum.flat_map(fn {_, y} -> [y, y + 1] end)
        |> MapSet.new()

      all_x = all_x_set |> MapSet.to_list() |> Enum.sort()
      all_y = all_y_set |> MapSet.to_list() |> Enum.sort()

      x_to_cx = all_x |> Enum.with_index() |> Enum.into(%{})
      y_to_cy = all_y |> Enum.with_index() |> Enum.into(%{})

      width = length(all_x)
      height = length(all_y)

      # Build grid
      grid = for _ <- 0..(width - 1), do: for(_ <- 0..(height - 1), do: false)
      grid = List.to_tuple(Enum.map(grid, &List.to_tuple/1))

      # Mark boundary
      grid = Enum.reduce(red_tiles, grid, fn {x, y}, g ->
        case {Map.get(x_to_cx, x), Map.get(y_to_cy, y)} do
          {cx, cy} when not is_nil(cx) and not is_nil(cy) ->
            row = elem(g, cx)
            new_row = put_elem(row, cy, true)
            put_elem(g, cx, new_row)
          _ -> g
        end
      end)

      # Connect consecutive red tiles
      grid = Enum.with_index(red_tiles)
        |> Enum.reduce(grid, fn {{x1, y1}, i}, g ->
          {x2, y2} = Enum.at(red_tiles, rem(i + 1, length(red_tiles)))
          cond do
            x1 == x2 ->
              Enum.reduce(min(y1, y2)..max(y1, y2), g, fn y, acc ->
                case {Map.get(x_to_cx, x1), Map.get(y_to_cy, y)} do
                  {cx, cy} when not is_nil(cx) and not is_nil(cy) ->
                    row = elem(acc, cx)
                    new_row = put_elem(row, cy, true)
                    put_elem(acc, cx, new_row)
                  _ -> acc
                end
              end)
            y1 == y2 ->
              Enum.reduce(min(x1, x2)..max(x1, x2), g, fn x, acc ->
                case {Map.get(x_to_cx, x), Map.get(y_to_cy, y1)} do
                  {cx, cy} when not is_nil(cx) and not is_nil(cy) ->
                    row = elem(acc, cx)
                    new_row = put_elem(row, cy, true)
                    put_elem(acc, cx, new_row)
                  _ -> acc
                end
              end)
            true -> g
          end
        end)

      # Flood fill interior
      grid = Enum.reduce(0..(width - 1), grid, fn cx, g ->
        Enum.reduce(0..(height - 1), g, fn cy, acc ->
          row = elem(acc, cx)
          if elem(row, cy) do
            acc
          else
            orig_x = Enum.at(all_x, cx)
            orig_y = Enum.at(all_y, cy)
            if point_in_polygon(orig_x, orig_y, red_tiles) do
              # Simple flood fill - mark this point and neighbors
              stack = [{cx, cy}]
              flood_fill(acc, stack, all_x, all_y, x_to_cx, y_to_cy, width, height, red_tiles)
            else
              acc
            end
          end
        end)
      end)

      # Build prefix sum
      prefix = build_prefix(grid, width, height)

      # Generate candidates sorted by area descending
      candidates = red_tiles
        |> Enum.flat_map(fn {x1, y1} ->
          Enum.map(red_tiles, fn {x2, y2} ->
            if {x1, y1} < {x2, y2} do
              min_x = min(x1, x2)
              max_x = max(x1, x2)
              min_y = min(y1, y2)
              max_y = max(y1, y2)
              area = (max_x - min_x + 1) * (max_y - min_y + 1)

              cx1 = Map.get(x_to_cx, min_x)
              cx2 = Map.get(x_to_cx, max_x)
              cy1 = Map.get(y_to_cy, min_y)
              cy2 = Map.get(y_to_cy, max_y)

              if cx1 && cx2 && cy1 && cy2 do
                {min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2}
              else
                nil
              end
            else
              nil
            end
          end)
        end)
        |> Enum.filter(&(&1 != nil))
        |> Enum.sort_by(fn {_, _, _, _, area, _, _, _, _} -> -area end)

      # Check candidates
      max_area_part2 = Enum.reduce_while(candidates, 0, fn {min_x, max_x, min_y, max_y, area, cx1, cx2, cy1, cy2}, best ->
        if area <= best do
          {:halt, best}
        else
          valid_count = rect_sum(prefix, cx1, cx2, cy1, cy2)
          expected_cells = (cx2 - cx1 + 1) * (cy2 - cy1 + 1)

          if valid_count == expected_cells do
            corners = [{min_x, min_y}, {min_x, max_y}, {max_x, min_y}, {max_x, max_y}]
            all_valid = Enum.all?(corners, fn {x, y} ->
              cx = Map.get(x_to_cx, x)
              cy = Map.get(y_to_cy, y)
              if cx && cy do
                row = elem(grid, cx)
                elem(row, cy)
              else
                point_in_polygon(x, y, red_tiles)
              end
            end)

            if all_valid do
              {:halt, area}
            else
              {:cont, best}
            end
          else
            {:cont, best}
          end
        end
      end)

      {Integer.to_string(max_area_part1), Integer.to_string(max_area_part2)}
    end
  end

  defp flood_fill(grid, [], _, _, _, _, _, _, _), do: grid
  defp flood_fill(grid, [{cx, cy} | rest], all_x, all_y, x_to_cx, y_to_cy, width, height, polygon) do
    if cx >= width || cy >= height do
      flood_fill(grid, rest, all_x, all_y, x_to_cx, y_to_cy, width, height, polygon)
    else
      row = elem(grid, cx)
      if elem(row, cy) do
        flood_fill(grid, rest, all_x, all_y, x_to_cx, y_to_cy, width, height, polygon)
      else
        orig_x = Enum.at(all_x, cx)
        orig_y = Enum.at(all_y, cy)
        if point_in_polygon(orig_x, orig_y, polygon) do
          new_row = put_elem(row, cy, true)
          new_grid = put_elem(grid, cx, new_row)
          new_stack = rest ++
            (if cx > 0, do: [{cx - 1, cy}], else: []) ++
            (if cx + 1 < width, do: [{cx + 1, cy}], else: []) ++
            (if cy > 0, do: [{cx, cy - 1}], else: []) ++
            (if cy + 1 < height, do: [{cx, cy + 1}], else: [])
          flood_fill(new_grid, new_stack, all_x, all_y, x_to_cx, y_to_cy, width, height, polygon)
        else
          flood_fill(grid, rest, all_x, all_y, x_to_cx, y_to_cy, width, height, polygon)
        end
      end
    end
  end

  defp build_prefix(grid, width, height) do
    prefix = for _ <- 0..width, do: for(_ <- 0..height, do: 0)
    prefix = List.to_tuple(Enum.map(prefix, &List.to_tuple/1))

    Enum.reduce(0..(width - 1), prefix, fn cx, p ->
      Enum.reduce(0..(height - 1), p, fn cy, acc ->
        row = elem(grid, cx)
        val = if elem(row, cy), do: 1, else: 0

        p_row = elem(acc, cx + 1)
        p_val = elem(p_row, cy + 1) + elem(elem(acc, cx), cy + 1) +
                elem(p_row, cy) - elem(elem(acc, cx), cy) + val
        new_p_row = put_elem(p_row, cy + 1, p_val)
        put_elem(acc, cx + 1, new_p_row)
      end)
    end)
  end

  defp rect_sum(prefix, cx1, cx2, cy1, cy2) do
    p_row2 = elem(prefix, cx2 + 1)
    p_row1 = elem(prefix, cx1)
    elem(p_row2, cy2 + 1) - elem(p_row1, cy2 + 1) - elem(p_row2, cy1) + elem(p_row1, cy1)
  end
end

{part1, part2} =
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
