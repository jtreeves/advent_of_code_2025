Coord = Struct.new(:x, :y, :z)

Pair = Struct.new(:i, :j, :dist_sq)

class UnionFind
  attr_reader :size, :component_count

  def initialize(n)
    @parent = (0...n).to_a
    @size = Array.new(n, 1)
    @component_count = n
  end

  def find(x)
    if @parent[x] != x
      @parent[x] = find(@parent[x])
    end
    @parent[x]
  end

  def union(x, y)
    root_x = find(x)
    root_y = find(y)
    return false if root_x == root_y

    root_x, root_y = root_y, root_x if @size[root_x] < @size[root_y]
    @parent[root_y] = root_x
    @size[root_x] += @size[root_y]
    @component_count -= 1
    true
  end
end

def parse_coordinates(lines)
  lines.map do |line|
    line = line.strip
    next nil if line.empty?
    parts = line.split(',')
    Coord.new(parts[0].to_i, parts[1].to_i, parts[2].to_i)
  end.compact
end

def squared_distance(p1, p2)
  dx = p2.x - p1.x
  dy = p2.y - p1.y
  dz = p2.z - p1.z
  dx * dx + dy * dy + dz * dz
end

def solve(input_data)
  lines = input_data.strip.split("\n")
  coords = parse_coordinates(lines)

  n = coords.length
  return ["0", "0"] if n == 0

  # Generate all pairs with squared distances
  pairs = []
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      dist_sq = squared_distance(coords[i], coords[j])
      pairs << Pair.new(i, j, dist_sq)
    end
  end

  # Sort by distance
  pairs.sort_by! { |p| p.dist_sq }

  # Part 1: Connect first 1000 pairs
  uf1 = UnionFind.new(n)
  connections_made = 0
  pairs.each do |pair|
    break if connections_made >= 1000
    uf1.union(pair.i, pair.j)
    connections_made += 1
  end

  # Get component sizes
  component_sizes = {}
  (0...n).each do |i|
    root = uf1.find(i)
    component_sizes[root] = uf1.size[root]
  end

  sizes = component_sizes.values.sort.reverse
  part1 = sizes.length >= 3 ? sizes[0] * sizes[1] * sizes[2] : 0

  # Part 2: Connect until all in one circuit
  uf2 = UnionFind.new(n)
  final_pair = nil
  pairs.each do |pair|
    break if uf2.component_count == 1
    if uf2.union(pair.i, pair.j)
      if uf2.component_count == 1
        final_pair = pair
        break
      end
    end
  end

  part2 = final_pair ? coords[final_pair.i].x * coords[final_pair.j].x : 0

  [part1.to_s, part2.to_s]
end

def main
  data = File.read("../data/input.txt")
  part1, part2 = solve(data)
  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

main
