# Common utility functions for Julia solutions

"""
Read input file and return lines as a vector of strings.
"""
function read_input(file_path::String)::Vector{String}
    lines = readlines(file_path)
    return [strip(line) for line in lines]
end

"""
Read input file and return raw content as a string.
"""
function read_input_raw(file_path::String)::String
    return read(file_path, String)
end

"""
Parse integers from a line of text.
"""
function parse_ints(line::String)::Vector{Int}
    matches = eachmatch(r"-?\d+", line)
    return [parse(Int, m.match) for m in matches]
end

# Placeholder for additional common utilities
