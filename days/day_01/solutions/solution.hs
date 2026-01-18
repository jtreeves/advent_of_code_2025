-- Placeholder for Day 01 Haskell solution
-- Note: Haskell utility functions would be in utilities/haskell/GetInput.hs
-- For now, using inline function - will be replaced with proper imports

import System.IO

-- Utility function - will be in utilities/haskell/GetInput.hs
readInputRaw :: FilePath -> IO String
readInputRaw path = readFile path

solve :: String -> (String, String)
solve inputData = (part1, part2)
  where
    lines' = lines inputData
    part1 = "TODO"
    part2 = "TODO"

main :: IO ()
main = do
  -- Use utility function to get input
  data <- readInputRaw "../data/input.txt"
  let (part1, part2) = solve data
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
