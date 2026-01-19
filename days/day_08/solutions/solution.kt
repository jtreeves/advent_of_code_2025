import java.io.File

data class Coord(val x: Long, val y: Long, val z: Long)

data class PairData(val i: Int, val j: Int, val distSq: Long)

class UnionFind(n: Int) {
    private val parent = IntArray(n) { it }
    val size = LongArray(n) { 1 }
    var componentCount = n

    fun find(x: Int): Int {
        if (parent[x] != x) {
            parent[x] = find(parent[x])
        }
        return parent[x]
    }

    fun union(x: Int, y: Int): Boolean {
        val rootX = find(x)
        val rootY = find(y)
        if (rootX == rootY) {
            return false
        }
        val (rx, ry) = if (size[rootX] < size[rootY]) {
            Pair(rootY, rootX)
        } else {
            Pair(rootX, rootY)
        }
        parent[ry] = rx
        size[rx] += size[ry]
        componentCount--
        return true
    }
}

fun parseCoordinates(lines: List<String>): List<Coord> {
    return lines.mapNotNull { line ->
        val trimmed = line.trim()
        if (trimmed.isEmpty()) return@mapNotNull null
        val parts = trimmed.split(",")
        if (parts.size != 3) return@mapNotNull null
        Coord(parts[0].toLong(), parts[1].toLong(), parts[2].toLong())
    }
}

fun squaredDistance(p1: Coord, p2: Coord): Long {
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    val dz = p2.z - p1.z
    return dx * dx + dy * dy + dz * dz
}

fun solve(inputData: String): Pair<String, String> {
    val lines = inputData.trim().split("\n")
    val coords = parseCoordinates(lines)

    val n = coords.size
    if (n == 0) {
        return Pair("0", "0")
    }

    // Generate all pairs with squared distances
    val pairs = mutableListOf<PairData>()
    for (i in 0 until n) {
        for (j in (i + 1) until n) {
            val distSq = squaredDistance(coords[i], coords[j])
            pairs.add(PairData(i, j, distSq))
        }
    }

    // Sort by distance
    pairs.sortBy { it.distSq }

    // Part 1: Connect first 1000 pairs
    val uf1 = UnionFind(n)
    var connectionsMade = 0
    for (pair in pairs) {
        if (connectionsMade >= 1000) break
        uf1.union(pair.i, pair.j)
        connectionsMade++
    }

    // Get component sizes
    val componentSizes = mutableMapOf<Int, Long>()
    for (i in 0 until n) {
        val root = uf1.find(i)
        componentSizes[root] = uf1.size[root]
    }

    val sizes = componentSizes.values.sortedDescending()
    val part1 = if (sizes.size >= 3) {
        sizes[0] * sizes[1] * sizes[2]
    } else {
        0L
    }

    // Part 2: Connect until all in one circuit
    val uf2 = UnionFind(n)
    var finalPair: PairData? = null
    for (pair in pairs) {
        if (uf2.componentCount == 1) break
        if (uf2.union(pair.i, pair.j)) {
            if (uf2.componentCount == 1) {
                finalPair = pair
                break
            }
        }
    }

    val part2 = if (finalPair != null) {
        coords[finalPair.i].x * coords[finalPair.j].x
    } else {
        0L
    }

    return Pair(part1.toString(), part2.toString())
}

fun main() {
    val data = File("../data/input.txt").readText()
    val (part1, part2) = solve(data)
    println("Part 1: $part1")
    println("Part 2: $part2")
}
