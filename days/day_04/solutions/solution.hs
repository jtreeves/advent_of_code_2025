import System.IO
import Data.Char

-- Import utilities
getInputPath :: Int -> FilePath
getInputPath _ = "../data/input.txt"

readInput :: FilePath -> IO [String]
readInput filePath = do
    content <- readFile filePath
    return $ lines $ trim content
    where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

countNeighbors :: [String] -> Int -> Int -> Int -> Int -> Int
countNeighbors grid i j rows cols = 
    sum [1 | di <- [-1..1], dj <- [-1..1], di /= 0 || dj /= 0,
        let ni = i + di, let nj = j + dj,
        ni >= 0 && ni < rows && nj >= 0 && nj < cols,
        grid !! ni !! nj == '@']

solve :: [String] -> (Int, Int)
solve lines = (solvePart1 lines, solvePart2 lines)

solvePart1 :: [String] -> Int
solvePart1 lines = 
    let rows = length lines
        cols = if rows > 0 then length (head lines) else 0
    in sum [1 | i <- [0..rows-1], j <- [0..cols-1],
        lines !! i !! j == '@',
        countNeighbors lines i j rows cols < 4]

solvePart2 :: [String] -> Int
solvePart2 lines = 
    let rows = length lines
        cols = if rows > 0 then length (head lines) else 0
        grid = map (map id) lines  -- Create mutable copy
    in removeIteratively (map (map id) lines) rows cols 0
    where
        removeIteratively :: [[Char]] -> Int -> Int -> Int -> Int
        removeIteratively grid rows cols count =
            let toRemove = [(i, j) | i <- [0..rows-1], j <- [0..cols-1],
                grid !! i !! j == '@',
                countNeighbors (map (\r -> map (\c -> if (i, j) `elem` [] then '.' else c) r) grid) i j rows cols < 4]
            in if null toRemove
               then count
               else let newGrid = foldl (\g (i, j) -> 
                            take i g ++ [take j (g !! i) ++ ['.'] ++ drop (j+1) (g !! i)] ++ drop (i+1) g) grid toRemove
                    in removeIteratively newGrid rows cols (count + length toRemove)

main :: IO ()
main = do
    lines <- readInput (getInputPath 4)
    let (part1, part2) = solve lines
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
