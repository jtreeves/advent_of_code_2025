import System.IO
import Data.List

-- Check if ID is invalid for Part 1: exactly two identical sequences
isInvalidPart1 :: String -> Bool
isInvalidPart1 idStr
  | length idStr `mod` 2 /= 0 = False
  | otherwise = let half = length idStr `div` 2
                    (first, second) = splitAt half idStr
                in first == second

-- Check if ID is invalid for Part 2: sequence repeated 2+ times
isInvalidPart2 :: String -> Bool
isInvalidPart2 idStr = any (\k -> length idStr `mod` k == 0 &&
                                   let seqLen = length idStr `div` k
                                       pattern = take seqLen idStr
                                   in idStr == concat (replicate k pattern))
                           [2..length idStr]

-- Helper function to split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str = case break (== delimiter) str of
  (token, []) -> [token]
  (token, _:rest) -> token : splitOn delimiter rest

-- Parse a range string like "start-end"
parseRange :: String -> (Int, Int)
parseRange s = let [start, end] = map read $ splitOn '-' s
               in (start, end)

-- Parse a line of comma-separated ranges
parseRanges :: String -> [(Int, Int)]
parseRanges line = map parseRange $ filter (not . null) $ splitOn ',' line

-- Solve Day 2
solve :: String -> (String, String)
solve inputData = (show part1Sum, show part2Sum)
  where
    lines' = filter (not . null) $ lines inputData
    allRanges = concatMap parseRanges lines'
    
    part1Sum = sum [num | (start, end) <- allRanges,
                         num <- [start..end],
                         isInvalidPart1 (show num)]
    
    part2Sum = sum [num | (start, end) <- allRanges,
                         num <- [start..end],
                         isInvalidPart2 (show num)]

main :: IO ()
main = do
  inputData <- readFile "../data/input.txt"
  let (part1, part2) = solve inputData
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
