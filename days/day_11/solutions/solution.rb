def solve(input_data)
  lines = input_data.strip.split("\n")
  
  # Build graph: device -> list of outputs
  graph = {}
  lines.each do |line|
    line = line.strip
    next if line.empty?
    
    parts = line.split(':')
    next if parts.length != 2
    
    device = parts[0].strip
    outputs_str = parts[1].strip
    outputs = outputs_str.empty? ? [] : outputs_str.split(/\s+/)
    graph[device] = outputs
  end
  
  # Part 1: Count paths from "you" to "out"
  def count_paths_part1(node, graph, memo)
    return 1 if node == "out"
    return memo[node] if memo.key?(node)
    
    count = 0
    (graph[node] || []).each do |neighbor|
      count += count_paths_part1(neighbor, graph, memo)
    end
    
    memo[node] = count
    count
  end
  
  part1_count = 0
  if graph.key?("you")
    part1_memo = {}
    part1_count = count_paths_part1("you", graph, part1_memo)
  end
  
  # Part 2: Count paths from "svr" to "out" that include both "fft" and "dac"
  def count_paths_part2(node, visited_fft, visited_dac, graph, memo)
    if node == "out"
      return (visited_fft && visited_dac) ? 1 : 0
    end
    
    key = [node, visited_fft, visited_dac]
    return memo[key] if memo.key?(key)
    
    # Update flags when visiting fft or dac
    new_visited_fft = visited_fft || (node == "fft")
    new_visited_dac = visited_dac || (node == "dac")
    
    count = 0
    (graph[node] || []).each do |neighbor|
      count += count_paths_part2(neighbor, new_visited_fft, new_visited_dac, graph, memo)
    end
    
    memo[key] = count
    count
  end
  
  part2_count = 0
  if graph.key?("svr")
    part2_memo = {}
    part2_count = count_paths_part2("svr", false, false, graph, part2_memo)
  end
  
  [part1_count.to_s, part2_count.to_s]
end

input_data = File.read("../data/input.txt")
part1, part2 = solve(input_data)
puts "Part 1: #{part1}"
puts "Part 2: #{part2}"
