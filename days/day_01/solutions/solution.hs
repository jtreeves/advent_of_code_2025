import System.IO

-- Placeholder for Day 01 Haskell solution
solve :: String -> (String, String)
solve inputData = (part1, part2)
  where
    lines' = lines inputData
    part1 = "TODO"
    part2 = "TODO"

main :: IO ()
main = do
  data <- readFile "../data/input.txt"
  let (part1, part2) = solve data
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
