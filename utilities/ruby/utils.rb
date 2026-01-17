# Common utility functions for Ruby solutions

def read_input(file_path)
  File.readlines(file_path).map(&:strip)
end

def read_input_raw(file_path)
  File.read(file_path)
end

def parse_ints(line)
  line.scan(/-?\d+/).map(&:to_i)
end

# Placeholder for additional common utilities
