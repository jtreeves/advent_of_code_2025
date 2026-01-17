# Common utility functions for Elixir solutions

defmodule Utils do
  def read_input(file_path) do
    file_path
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end

  def read_input_raw(file_path) do
    File.read!(file_path)
  end

  def parse_ints(line) do
    line
    |> String.split(~r/-?\d+/, include_captures: true)
    |> Enum.filter(&Regex.match?(~r/^-?\d+$/, &1))
    |> Enum.map(&String.to_integer/1)
  end

  # Placeholder for additional common utilities
end
