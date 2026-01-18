# Input reading utilities for Julia solutions

"""
Read input from input.txt for the given day.
"""
function get_input(day::Int)::Vector{String}
    path = get_input_path(day)
    return read_input(path)
end

"""
Read input from test_N.txt for the given day and test number.
"""
function get_test_input(day::Int, test_num::Int)::Vector{String}
    path = get_test_input_path(day, test_num)
    return read_input(path)
end

"""
Return the path to input.txt for the given day.
"""
function get_input_path(day::Int)::String
    return "../data/input.txt"
end

"""
Return the path to test_N.txt for the given day and test number.
"""
function get_test_input_path(day::Int, test_num::Int)::String
    return "../data/test_$test_num.txt"
end

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
