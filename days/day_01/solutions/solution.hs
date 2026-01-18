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

solve :: [String] -> (Int, Int)
solve lines = (solvePart1 lines 50 0, solvePart2 lines 50 0)

solvePart1 :: [String] -> Int -> Int -> Int
solvePart1 [] _ count = count
solvePart1 (line:rest) position count =
    let trimmed = trim line
    in if trimmed == ""
       then solvePart1 rest position count
       else let direction = head trimmed
                distance = read (tail trimmed) :: Int
                newPos = if direction == 'L'
                         then ((position - distance) `mod` 100 + 100) `mod` 100
                         else (position + distance) `mod` 100
                newCount = if newPos == 0 then count + 1 else count
            in solvePart1 rest newPos newCount
    where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

solvePart2 :: [String] -> Int -> Int -> Int
solvePart2 [] _ count = count
solvePart2 (line:rest) position count =
    let trimmed = trim line
    in if trimmed == ""
       then solvePart2 rest position count
       else let direction = head trimmed
                distance = read (tail trimmed) :: Int
                startPos = position
                -- Count zeros during rotation
                zerosInRotation = sum [1 | click <- [1..distance],
                    let clickPos = if direction == 'L'
                                   then ((startPos - click) `mod` 100 + 100) `mod` 100
                                   else (startPos + click) `mod` 100,
                    clickPos == 0]
                newPos = if direction == 'L'
                         then ((position - distance) `mod` 100 + 100) `mod` 100
                         else (position + distance) `mod` 100
            in solvePart2 rest newPos (count + zerosInRotation)
    where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

main :: IO ()
main = do
    lines <- readInput (getInputPath 1)
    let (part1, part2) = solve lines
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
