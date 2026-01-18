# Input reading utilities for Ruby solutions

# Read input from input.txt for the given day
def get_input(day)
  path = get_input_path(day)
  read_input(path)
end

# Read input from test_N.txt for the given day and test number
def get_test_input(day, test_num)
  path = get_test_input_path(day, test_num)
  read_input(path)
end

# Return the path to input.txt for the given day
def get_input_path(day)
  "../data/input.txt"
end

# Return the path to test_N.txt for the given day and test number
def get_test_input_path(day, test_num)
  "../data/test_#{test_num}.txt"
end

# Read input file and return lines as an array
def read_input(file_path)
  File.readlines(file_path).map(&:strip)
end

# Read input file and return raw content
def read_input_raw(file_path)
  File.read(file_path)
end
