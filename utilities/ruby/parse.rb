# Parsing utilities for Ruby solutions

# Parse integers from a line of text
def parse_ints(line)
  line.scan(/-?\d+/).map(&:to_i)
end
