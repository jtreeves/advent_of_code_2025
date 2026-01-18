# Parsing utilities for Julia solutions

"""
Parse integers from a line of text.
"""
function parse_ints(line::String)::Vector{Int}
    matches = eachmatch(r"-?\d+", line)
    return [parse(Int, m.match) for m in matches]
end
