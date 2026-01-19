import System.IO
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

solve :: String -> (Integer, Integer)
solve inputData = 
    let inputLines = lines inputData
        grid = map id inputLines
        rows = length grid
        cols = if rows > 0 then length (head grid) else 0
        
        -- Find starting position S
        startPos = findStart 0 0 grid rows cols
        startRow = fst startPos
        startCol = snd startPos
    in
    if startRow == -1
    then (0, 0)
    else
        let part1 = countSplits startRow startCol grid rows cols
            beamCounts = processBeams startRow startCol grid rows cols
            part2 = sum (last beamCounts)
        in (toInteger part1, part2)

findStart :: Int -> Int -> [String] -> Int -> Int -> (Int, Int)
findStart r c grid rows cols
    | r >= rows = (-1, -1)
    | c >= cols = findStart (r + 1) 0 grid rows cols
    | (grid !! r) !! c == 'S' = (r, c)
    | otherwise = findStart r (c + 1) grid rows cols

countSplits :: Int -> Int -> [String] -> Int -> Int -> Int
countSplits startRow startCol grid rows cols =
    let go r activeBeams count
            | r >= rows = count
            | otherwise =
                let nextBeams = Set.fromList (concatMap (\col ->
                        if (grid !! r) !! col == '.'
                        then [col]
                        else if (grid !! r) !! col == '^'
                        then filter (\c -> c >= 0 && c < cols) [col - 1, col + 1]
                        else []) (Set.toList activeBeams))
                    newCount = count + length (filter (\col -> (grid !! r) !! col == '^') (Set.toList activeBeams))
                in go (r + 1) nextBeams newCount
    in go (startRow + 1) (Set.singleton startCol) 0

processBeams :: Int -> Int -> [String] -> Int -> Int -> [[Integer]]
processBeams startRow startCol grid rows cols =
    let initial = replicate rows (replicate cols (0 :: Integer))
        initialWithStart = update2D initial startRow startCol (const 1)
        go r counts
            | r >= rows = counts
            | otherwise =
                let newCounts = foldl (\acc c ->
                        let prevCount = (acc !! (r - 1)) !! c
                        in if prevCount > 0
                        then if (grid !! r) !! c == '.'
                            then update2D acc r c (+ prevCount)
                            else if (grid !! r) !! c == '^'
                            then foldl (\acc2 newCol ->
                                if newCol >= 0 && newCol < cols
                                then update2D acc2 r newCol (+ prevCount)
                                else acc2) acc [c - 1, c + 1]
                            else acc
                        else acc) counts [0..cols-1]
                in go (r + 1) newCounts
    in go (startRow + 1) initialWithStart

update2D :: [[Integer]] -> Int -> Int -> (Integer -> Integer) -> [[Integer]]
update2D grid r c f = 
    let row = grid !! r
        newRow = take c row ++ [f (row !! c)] ++ drop (c + 1) row
    in take r grid ++ [newRow] ++ drop (r + 1) grid

main :: IO ()
main = do
    content <- readInputRaw "../data/input.txt"
    let (part1, part2) = solve content
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
