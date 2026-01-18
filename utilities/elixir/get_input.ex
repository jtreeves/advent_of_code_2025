# Input reading utilities for Elixir solutions

defmodule GetInput do
  # Read input from input.txt for the given day
  def get_input(day) do
    path = get_input_path(day)
    read_input(path)
  end

  # Read input from test_N.txt for the given day and test number
  def get_test_input(day, test_num) do
    path = get_test_input_path(day, test_num)
    read_input(path)
  end

  # Return the path to input.txt for the given day
  def get_input_path(day) do
    "../data/input.txt"
  end

  # Return the path to test_N.txt for the given day and test number
  def get_test_input_path(day, test_num) do
    "../data/test_#{test_num}.txt"
  end

  # Read input file and return lines as a list
  def read_input(file_path) do
    file_path
    |> File.read!()
    |> String.trim()
    |> String.split("\n")
  end

  # Read input file and return raw content
  def read_input_raw(file_path) do
    File.read!(file_path)
  end
end
