defmodule Solution do
  def solve(input_data) do
    lines = input_data
            |> String.trim()
            |> String.split("\n")
            |> Enum.filter(&(String.trim(&1) != ""))

    if lines == [] do
      {"0", "0"}
    else
      solve_lines(lines, 0, 0)
    end
  end

  defp solve_lines([], part1_total, part2_total) do
    {Integer.to_string(part1_total), Integer.to_string(part2_total)}
  end

  defp solve_lines([line | rest], part1_total, part2_total) do
    parsed = parse_line(line)

    if parsed.target_pattern == [] do
      solve_lines(rest, part1_total, part2_total)
    else
      num_lights = length(parsed.target_pattern)

      # Part 1: GF(2) linear system
      button_matrix = Enum.map(parsed.buttons, fn btn ->
        Enum.map(0..(num_lights - 1), fn light ->
          Enum.member?(btn, light)
        end)
      end)

      required_toggles = parsed.target_pattern
      result1 = gaussian_elimination_gf2(button_matrix, required_toggles)
      new_part1 = part1_total + (result1 || 0)

      # Part 2: ILP
      result2 = if length(parsed.joltages) == num_lights do
        solve_part2_ilp(parsed.buttons, parsed.joltages)
      else
        nil
      end
      new_part2 = part2_total + (result2 || 0)

      solve_lines(rest, new_part1, new_part2)
    end
  end

  defp parse_line(line) do
    # Extract pattern: [.##.]
    pattern_re = ~r/\[([.#]+)\]/
    pattern_match = Regex.run(pattern_re, line)
    target_pattern = if pattern_match do
      pattern_match
      |> Enum.at(1)
      |> String.graphemes()
      |> Enum.map(&(&1 == "#"))
    else
      []
    end

    # Extract buttons: (1,3) (2) etc.
    button_re = ~r/\(([^)]*)\)/
    buttons = Regex.scan(button_re, line)
             |> Enum.map(fn [_, btn_str] ->
               btn_str = String.trim(btn_str)
               if btn_str == "" do
                 []
               else
                 btn_str
                 |> String.split(",")
                 |> Enum.map(&String.trim/1)
                 |> Enum.map(&String.to_integer/1)
               end
             end)

    # Extract joltages: {3,5,4,7}
    joltage_re = ~r/\{([^}]+)\}/
    joltage_match = Regex.run(joltage_re, line)
    joltages = if joltage_match do
      joltage_match
      |> Enum.at(1)
      |> String.split(",")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)
    else
      []
    end

    %{target_pattern: target_pattern, buttons: buttons, joltages: joltages}
  end

  defp gaussian_elimination_gf2(matrix, target) do
    num_buttons = length(matrix)
    num_lights = length(target)

    # Create augmented matrix [A | b]
    aug = Enum.map(0..(num_lights - 1), fn i ->
      row = Enum.map(0..(num_buttons - 1), fn j ->
        if j < length(matrix) && i < length(Enum.at(matrix, j)) do
          Enum.at(Enum.at(matrix, j), i)
        else
          false
        end
      end)
      row ++ [Enum.at(target, i)]
    end)

    {aug_result, _, _} = do_gaussian_elimination(aug, 0, 0, num_buttons, num_lights)

    # Check for inconsistency
    inconsistent = Enum.any?(Enum.at(aug_result, num_lights - 1) || [], fn row ->
      Enum.at(row, num_buttons) && not Enum.any?(Enum.take(row, num_buttons))
    end)

    if inconsistent do
      nil
    else
      # Back substitution
      solution = List.duplicate(false, num_buttons)
      {final_solution, _} = do_back_substitution(aug_result, num_lights - 1, solution, MapSet.new(), num_buttons)
      Enum.count(final_solution, & &1)
    end
  end

  defp do_gaussian_elimination(aug, pivot_row, pivot_col, num_buttons, num_lights) do
    if pivot_row >= num_lights || pivot_col >= num_buttons do
      {aug, pivot_row, pivot_col}
    else
      # Find pivot
      pivot_idx = Enum.find_index(pivot_row..(num_lights - 1), fn i ->
        Enum.at(Enum.at(aug, i), pivot_col)
      end)

      if pivot_idx == nil do
        do_gaussian_elimination(aug, pivot_row, pivot_col + 1, num_buttons, num_lights)
      else
        actual_idx = pivot_row + pivot_idx

        # Swap rows
        aug_swapped = if actual_idx != pivot_row do
          List.replace_at(aug, pivot_row, Enum.at(aug, actual_idx))
          |> List.replace_at(actual_idx, Enum.at(aug, pivot_row))
        else
          aug
        end

        # Eliminate
        aug_eliminated = Enum.map(0..(num_lights - 1), fn i ->
          if i > pivot_row && Enum.at(Enum.at(aug_swapped, i), pivot_col) do
            Enum.zip_with([Enum.at(aug_swapped, i), Enum.at(aug_swapped, pivot_row)], fn [a, b] ->
              a != b  # XOR
            end)
          else
            Enum.at(aug_swapped, i)
          end
        end)

        do_gaussian_elimination(aug_eliminated, pivot_row + 1, pivot_col + 1, num_buttons, num_lights)
      end
    end
  end

  defp do_back_substitution(aug, i, solution, used_rows, num_buttons) when i < 0 do
    {solution, used_rows}
  end

  defp do_back_substitution(aug, i, solution, used_rows, num_buttons) do
    row = Enum.at(aug, i)

    # Find first non-zero column
    pivot_col_idx = Enum.find_index(0..(num_buttons - 1), fn j ->
      Enum.at(row, j) && not MapSet.member?(used_rows, j)
    end)

    if pivot_col_idx == nil do
      do_back_substitution(aug, i - 1, solution, used_rows, num_buttons)
    else
      used_rows_new = MapSet.put(used_rows, pivot_col_idx)

      # Calculate value
      val = Enum.reduce((pivot_col_idx + 1)..(num_buttons - 1), Enum.at(row, num_buttons), fn j, acc ->
        if Enum.at(row, j) && Enum.at(solution, j) do
          not acc
        else
          acc
        end
      end)

      solution_new = List.replace_at(solution, pivot_col_idx, val)
      do_back_substitution(aug, i - 1, solution_new, used_rows_new, num_buttons)
    end
  end

  defp solve_part2_ilp(buttons, joltages) do
    num_buttons = length(buttons)
    num_lights = length(joltages)
    max_joltage = Enum.max(joltages ++ [0])

    dfs(0, List.duplicate(0, num_lights), 0, nil, buttons, joltages, max_joltage)
  end

  defp dfs(button_idx, current_joltages, presses_so_far, best, buttons, joltages, max_joltage) do
    if button_idx >= length(buttons) do
      if Enum.zip_with(current_joltages, joltages, & &1 == &2) |> Enum.all?() do
        if best == nil, do: presses_so_far, else: min(best, presses_so_far)
      else
        best
      end
    else
      if best != nil && presses_so_far >= best do
        best
      else
        # Pruning: calculate lower bound
        remaining_needs = Enum.zip_with(current_joltages, joltages, fn curr, target ->
          max(0, target - curr)
        end)
        sum_remaining = Enum.sum(remaining_needs)

        # Check pruning
        should_prune = if sum_remaining > 0 do
          max_lights_per_button = buttons
                                  |> Enum.drop(button_idx)
                                  |> Enum.map(&length/1)
                                  |> Enum.max(&<=/2, fn -> 1 end)

          if max_lights_per_button > 0 do
            min_additional = div(sum_remaining + max_lights_per_button - 1, max_lights_per_button)
            best != nil && presses_so_far + min_additional >= best
          else
            false
          end
        else
          false
        end

        if should_prune do
          best
        else
          # Try 0 to max_joltage presses
          try do
            Enum.reduce(0..max_joltage, best, fn presses, best_result ->
              if best != nil && presses_so_far + presses >= best do
                throw({:break, best_result})
              end

              new_joltages = Enum.map(0..(length(current_joltages) - 1), fn i ->
                Enum.at(current_joltages, i) +
                (if Enum.member?(Enum.at(buttons, button_idx), i) do presses else 0 end)
              end)

              # Check if exceeds target
              exceeds = Enum.any?(Enum.zip(new_joltages, joltages), fn {a, b} -> a > b end)

              if exceeds do
                best_result
              else
                result = dfs(button_idx + 1, new_joltages, presses_so_far + presses, best_result, buttons, joltages, max_joltage)
                if result != nil do
                  if best_result == nil, do: result, else: min(best_result, result)
                else
                  best_result
                end
              end
            end)
          catch
            {:break, result} -> result
          end
        end
      end
    end
  end
end

{part1, part2} =
  "../data/input.txt"
  |> File.read!()
  |> Solution.solve()

IO.puts("Part 1: #{part1}")
IO.puts("Part 2: #{part2}")
