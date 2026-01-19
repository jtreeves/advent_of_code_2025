import Data.List hiding (union)
import Data.Maybe
import qualified Data.Map.Strict as Map

data Coord = Coord { x :: Integer, y :: Integer, z :: Integer } deriving (Eq, Show)

type Pair = (Int, Int, Integer)

data UnionFind = UF {
    parent :: [Int],
    size :: [Integer],
    componentCount :: Int
} deriving (Show)

parseCoordinates :: [String] -> [Coord]
parseCoordinates lines = map parseLine (filter (not . null . trim) lines)
  where
    trim = dropWhile (== ' ')
    parseLine line =
        let parts = split ',' (trim line)
            x = read (parts !! 0) :: Integer
            y = read (parts !! 1) :: Integer
            z = read (parts !! 2) :: Integer
        in Coord x y z
    split _ [] = []
    split c s = let (part, rest) = break (== c) s
                in part : case rest of
                    [] -> []
                    (_:rs) -> split c rs

squaredDistance :: Coord -> Coord -> Integer
squaredDistance p1 p2 =
    let dx = x p2 - x p1
        dy = y p2 - y p1
        dz = z p2 - z p1
    in dx * dx + dy * dy + dz * dz

initUnionFind :: Int -> UnionFind
initUnionFind n = UF {
    parent = [0..n-1],
    size = replicate n 1,
    componentCount = n
}

findRoot :: UnionFind -> Int -> (UnionFind, Int)
findRoot uf x =
    let p = parent uf !! x
    in if p == x
       then (uf, x)
       else let (uf', root) = findRoot uf p
                newParent = take x (parent uf') ++ [root] ++ drop (x + 1) (parent uf')
            in (uf' { parent = newParent }, root)

union :: UnionFind -> Int -> Int -> UnionFind
union uf x y =
    let (uf1, rootX) = findRoot uf x
        (uf2, rootY) = findRoot uf1 y
    in if rootX == rootY
       then uf2
       else let sizeX = size uf2 !! rootX
                sizeY = size uf2 !! rootY
                (rx, ry) = if sizeX < sizeY then (rootY, rootX) else (rootX, rootY)
                newParent = take ry (parent uf2) ++ [rx] ++ drop (ry + 1) (parent uf2)
                newSize = take rx (size uf2) ++ [size uf2 !! rx + size uf2 !! ry] ++ drop (rx + 1) (size uf2)
            in uf2 { parent = newParent, size = newSize, componentCount = componentCount uf2 - 1 }

solve :: String -> (String, String)
solve inputData =
    let lines' = lines inputData
        coords = parseCoordinates lines'
        n = length coords
    in if n == 0
       then ("0", "0")
       else let -- Generate all pairs
                pairs = [(i, j, squaredDistance (coords !! i) (coords !! j)) | i <- [0..n-1], j <- [i+1..n-1]]
                sortedPairs = sortOn (\(_, _, dist) -> dist) pairs
                
                -- Part 1: Connect first 1000 pairs
                uf1 = foldl (\uf (i, j, _) -> union uf i j) (initUnionFind n) (take 1000 sortedPairs)
                
                -- Get component sizes
                componentSizes = Map.fromList $ map (\i -> let (_, root) = findRoot uf1 i in (root, size uf1 !! root)) [0..n-1]
                sizes = sortBy (flip compare) $ Map.elems componentSizes
                part1 = if length sizes >= 3 then product (take 3 sizes) else 0
                
                -- Part 2: Connect until all in one circuit
                finalPair = findFinalPair (initUnionFind n) sortedPairs
                part2 = case finalPair of
                    Just (i, j, _) -> x (coords !! i) * x (coords !! j)
                    Nothing -> 0
            in (show part1, show part2)

findFinalPair :: UnionFind -> [Pair] -> Maybe Pair
findFinalPair uf [] = Nothing
findFinalPair uf ((i, j, dist):pairs)
    | componentCount uf == 1 = Nothing
    | otherwise = let newUF = union uf i j
                  in if componentCount newUF == 1
                     then Just (i, j, dist)
                     else findFinalPair newUF pairs

main :: IO ()
main = do
    input <- readFile "../data/input.txt"
    let (part1, part2) = solve input
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2
