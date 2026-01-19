mutable struct UnionFind
    parent::Vector{Int}
    size::Vector{Int}
    component_count::Int
end

function UnionFind(n::Int)
    UnionFind(collect(1:n), ones(Int, n), n)
end

function find_root!(uf::UnionFind, x::Int)::Int
    if uf.parent[x] != x
        uf.parent[x] = find_root!(uf, uf.parent[x])
    end
    return uf.parent[x]
end

function union!(uf::UnionFind, x::Int, y::Int)::Bool
    root_x = find_root!(uf, x)
    root_y = find_root!(uf, y)
    if root_x == root_y
        return false
    end
    if uf.size[root_x] < uf.size[root_y]
        root_x, root_y = root_y, root_x
    end
    uf.parent[root_y] = root_x
    uf.size[root_x] += uf.size[root_y]
    uf.component_count -= 1
    return true
end

struct Coord
    x::Int
    y::Int
    z::Int
end

function parse_coordinates(lines::Vector{String})::Vector{Coord}
    coords = Coord[]
    for line in lines
        line = strip(line)
        isempty(line) && continue
        parts = split(line, ',')
        x = parse(Int, parts[1])
        y = parse(Int, parts[2])
        z = parse(Int, parts[3])
        push!(coords, Coord(x, y, z))
    end
    return coords
end

function squared_distance(p1::Coord, p2::Coord)::Int
    dx = p2.x - p1.x
    dy = p2.y - p1.y
    dz = p2.z - p1.z
    return dx * dx + dy * dy + dz * dz
end

function solve(input_data::String)::Tuple{String, String}
    lines = split(strip(input_data), '\n') .|> String
    coords = parse_coordinates(lines)
    n = length(coords)
    
    if n == 0
        return ("0", "0")
    end
    
    # Generate all pairs with squared distances
    pairs = Tuple{Int, Int, Int}[]
    for i in 1:n
        for j in (i+1):n
            dist_sq = squared_distance(coords[i], coords[j])
            push!(pairs, (i, j, dist_sq))
        end
    end
    
    # Sort by distance
    sort!(pairs, by = x -> x[3])
    
    # Part 1: Connect first 1000 pairs
    uf1 = UnionFind(n)
    connections_made = 0
    for (i, j, _) in pairs
        if connections_made >= 1000
            break
        end
        union!(uf1, i, j)
        connections_made += 1
    end
    
    # Get component sizes
    component_sizes = Dict{Int, Int}()
    for i in 1:n
        root = find_root!(uf1, i)
        component_sizes[root] = uf1.size[root]
    end
    
    sizes = sort(collect(values(component_sizes)), rev=true)
    part1 = length(sizes) >= 3 ? sizes[1] * sizes[2] * sizes[3] : 0
    
    # Part 2: Connect until all in one circuit
    uf2 = UnionFind(n)
    final_pair = nothing
    for (i, j, _) in pairs
        if uf2.component_count == 1
            break
        end
        if union!(uf2, i, j)
            if uf2.component_count == 1
                final_pair = (i, j)
                break
            end
        end
    end
    
    part2 = if final_pair !== nothing
        i, j = final_pair
        coords[i].x * coords[j].x
    else
        0
    end
    
    return (string(part1), string(part2))
end

function main()
    data = read("../data/input.txt", String) |> strip |> String
    part1, part2 = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
end

main()
