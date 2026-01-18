import System.IO

-- Find the largest N-digit number by selecting N digits in order from bank
findLargestSubsequence :: String -> Int -> Int
findLargestSubsequence bank n
  | length bank < n = 0
  | otherwise = read $ snd $ helper 0 "" [0 .. n - 1]
  where
    bankLen = length bank
    helper start result [] = (start, result)
    helper start result (i:is) =
      let remainingNeeded = n - i - 1
          end = bankLen - remainingNeeded
          candidates = take (end - start) $ drop start bank
          maxDigit = maximum candidates
          maxPos = start + length (takeWhile (/= maxDigit) candidates)
       in helper (maxPos + 1) (result ++ [maxDigit]) is

solve :: String -> (String, String)
solve inputData = (show part1Sum, show part2Sum)
  where
    banks = filter (not . null) $ map (filter (/= '\r')) $ lines inputData
    part1Sum = sum $ map (`findLargestSubsequence` 2) banks
    part2Sum = sum $ map (`findLargestSubsequence` 12) $ filter ((>= 12) . length) banks

main :: IO ()
main = do
  inputData <- readFile "../data/input.txt"
  let (part1, part2) = solve inputData
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
