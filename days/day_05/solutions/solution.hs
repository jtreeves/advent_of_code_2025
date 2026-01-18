import System.IO
import Data.List
import Data.Char

readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

parseRange :: String -> (Integer, Integer)
parseRange line = let [start_str, end_str] = words (map (\c -> if c == '-' then ' ' else c) line)
                  in (read start_str, read end_str)

solve :: String -> (Integer, Integer)
solve inputData = 
    let inputLines = lines inputData
        blankIdx = case findIndex (\l -> trim l == "") inputLines of
                      Just idx -> idx
                      Nothing -> length inputLines
        
        -- Parse ranges (first section)
        ranges = map parseRange (take blankIdx inputLines)
        
        -- Parse IDs to check (second section)
        ids = map read (drop (blankIdx + 1) inputLines)
        
        -- Part 1: Count how many IDs fall into any range
        part1Count = toInteger $ length (filter (\idVal -> any (\(start, e) -> start <= idVal && idVal <= e) ranges) ids)
        
        -- Part 2: Merge ranges and count total unique IDs covered
        -- Sort ranges by start value
        sortedRanges = sortBy (\(a, _) (b, _) -> compare a b) ranges
        
        -- Merge overlapping/adjacent ranges
        merged = mergeRanges sortedRanges
        
        -- Calculate total unique IDs covered
        part2Total = sum (map (\(start, e) -> e - start + 1) merged)
    in
        (part1Count, part2Total)

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [x] = [x]
mergeRanges ((start1, end1):(start2, end2):rest) =
    if start2 <= end1 + 1
    then mergeRanges ((start1, max end1 end2):rest)
    else (start1, end1) : mergeRanges ((start2, end2):rest)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

main :: IO ()
main = do
    content <- readInputRaw "../data/input.txt"
    let (part1, part2) = solve content
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
