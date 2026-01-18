# Parsing utilities for Elixir solutions

defmodule Parse do
  # Parse integers from a line of text
  def parse_ints(line) do
    line
    |> String.split(~r/-?\d+/, include_captures: true)
    |> Enum.filter(&Regex.match?(~r/^-?\d+$/, &1))
    |> Enum.map(&String.to_integer/1)
  end
end
