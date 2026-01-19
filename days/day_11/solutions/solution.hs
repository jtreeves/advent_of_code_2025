import System.IO
import Data.Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List

-- Import utilities
getInputPath :: Int -> FilePath
getInputPath _ = "../data/input.txt"

readInput :: FilePath -> IO String
readInput filePath = readFile filePath

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delimiter str =
  case break (== delimiter) str of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delimiter after

buildGraph :: [String] -> Map String [String]
buildGraph inputLines = foldl' buildGraph' Map.empty inputLines
  where
    buildGraph' graph line =
      let parts = splitOn ':' line
      in if length parts == 2
         then let device = trim $ head parts
                  outputs_str = trim $ parts !! 1
                  outputs = if null outputs_str
                           then []
                           else words outputs_str
              in Map.insert device outputs graph
         else graph

countPathsPart1 :: String -> Map String [String] -> Map String Integer -> (Integer, Map String Integer)
countPathsPart1 node graph memo =
  if node == "out"
  then (1, memo)
  else case Map.lookup node memo of
    Just value -> (value, memo)
    Nothing ->
      let neighbors = Map.findWithDefault [] node graph
          (count, newMemo) = foldl' (\(acc, mem) neighbor ->
                                       let (val, updatedMem) = countPathsPart1 neighbor graph mem
                                       in (acc + val, updatedMem))
                                    (0, memo)
                                    neighbors
      in (count, Map.insert node count newMemo)

countPathsPart2 :: String -> Bool -> Bool -> Map String [String] -> Map (String, Bool, Bool) Integer -> (Integer, Map (String, Bool, Bool) Integer)
countPathsPart2 node visitedFFT visitedDAC graph memo =
  if node == "out"
  then (if visitedFFT && visitedDAC then 1 else 0, memo)
  else let key = (node, visitedFFT, visitedDAC)
       in case Map.lookup key memo of
         Just value -> (value, memo)
         Nothing ->
           let newVisitedFFT = visitedFFT || (node == "fft")
               newVisitedDAC = visitedDAC || (node == "dac")
               neighbors = Map.findWithDefault [] node graph
               (count, newMemo) = foldl' (\(acc, mem) neighbor ->
                                            let (val, updatedMem) = countPathsPart2 neighbor newVisitedFFT newVisitedDAC graph mem
                                            in (acc + val, updatedMem))
                                         (0, memo)
                                         neighbors
           in (count, Map.insert key count newMemo)

solve :: String -> (String, String)
solve inputData =
  let inputLines = filter (not . null . trim) $ Data.List.lines inputData
      graph = buildGraph inputLines
      
      part1Count = if Map.member "you" graph
                   then fst $ countPathsPart1 "you" graph Map.empty
                   else 0
      
      part2Count = if Map.member "svr" graph
                   then fst $ countPathsPart2 "svr" False False graph Map.empty
                   else 0
  in (show part1Count, show part2Count)

main :: IO ()
main = do
  inputData <- readInput "../data/input.txt"
  let (part1, part2) = solve inputData
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
