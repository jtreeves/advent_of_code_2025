import System.IO
import Data.List
import Data.Char
import Data.Maybe

data ParseResult = ParseResult {
    targetPattern :: [Bool],
    buttons :: [[Int]],
    joltages :: [Int]
}

parseLine :: String -> ParseResult
parseLine line = ParseResult target_pattern' buttons' joltages'
  where
    -- Extract pattern: [.##.]
    patternStart = maybe 0 id $ findIndex (== '[') line
    patternEnd = maybe 0 id $ findIndex (== ']') $ drop patternStart line
    target_pattern' = if patternEnd > 0
                      then map (== '#') $ take (patternEnd - 1) $ drop (patternStart + 1) line
                      else []
    
    -- Extract buttons: (1,3) (2) etc.
    buttons' = parseButtons line []
      where
        parseButtons str acc =
          case findIndex (== '(') str of
            Nothing -> reverse acc
            Just start ->
              case findIndex (== ')') $ drop start str of
                Nothing -> reverse acc
                Just end ->
                  let btnStr = take (end - 1) $ drop (start + 1) str
                      trimmed = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse btnStr
                      parsed = if null trimmed
                               then []
                               else map read $ words $ map (\c -> if c == ',' then ' ' else c) trimmed
                  in parseButtons (drop (start + end + 1) str) (parsed : acc)
    
    -- Extract joltages: {3,5,4,7}
    joltageStart = maybe 0 id $ findIndex (== '{') line
    joltageEnd = maybe 0 id $ findIndex (== '}') $ drop joltageStart line
    joltages' = if joltageEnd > 0
                then map read $ words $ map (\c -> if c == ',' then ' ' else c) $ take (joltageEnd - 1) $ drop (joltageStart + 1) line
                else []

gaussianEliminationGF2 :: [[Bool]] -> [Bool] -> Maybe Int
gaussianEliminationGF2 matrix target =
    let numButtons = length matrix
        numLights = length target
        -- Create augmented matrix [A | b]
        aug = map (\i -> map (\j -> if j < length matrix && i < length (matrix !! j)
                                     then (matrix !! j) !! i
                                     else False) [0..numButtons-1] ++ [target !! i]) [0..numLights-1]
        (augResult, _, _) = gaussianElimination aug 0 0 numButtons numLights
    in if isInconsistent augResult numLights numButtons
       then Nothing
       else Just $ countSolution $ backSubstitution augResult (numLights-1) (replicate numButtons False) [] numButtons
  where
    gaussianElimination aug pivotRow pivotCol numButtons numLights
        | pivotRow >= numLights || pivotCol >= numButtons = (aug, pivotRow, pivotCol)
        | otherwise =
            let pivotIdx = findIndex (\i -> (aug !! i) !! pivotCol) [pivotRow..numLights-1]
            in case pivotIdx of
                Nothing -> gaussianElimination aug pivotRow (pivotCol+1) numButtons numLights
                Just idx ->
                    let actualIdx = pivotRow + idx
                        augSwapped = if actualIdx /= pivotRow
                                     then swapRows aug pivotRow actualIdx
                                     else aug
                        augEliminated = map (\i -> if i > pivotRow && (augSwapped !! i) !! pivotCol
                                                   then zipWith (/=) (augSwapped !! i) (augSwapped !! pivotRow)
                                                   else augSwapped !! i) [0..numLights-1]
                    in gaussianElimination augEliminated (pivotRow+1) (pivotCol+1) numButtons numLights
    
    swapRows aug i j = take i aug ++ [aug !! j] ++ drop (i+1) (take j aug) ++ [aug !! i] ++ drop (j+1) aug
    
    isInconsistent aug numLights numButtons =
        any (\i -> (aug !! i) !! numButtons && not (any id (take numButtons (aug !! i)))) [0..numLights-1]
    
    backSubstitution aug i solution usedRows numButtons
        | i < 0 = solution
        | otherwise =
            let row = aug !! i
                pivotColIdx = findIndex (\j -> row !! j && j `notElem` usedRows) [0..numButtons-1]
            in case pivotColIdx of
                Nothing -> backSubstitution aug (i-1) solution usedRows numButtons
                Just idx ->
                    let val = foldl (\acc j -> if row !! j && solution !! j then not acc else acc)
                                   (row !! numButtons) [idx+1..numButtons-1]
                        solution' = take idx solution ++ [val] ++ drop (idx+1) solution
                    in backSubstitution aug (i-1) solution' (idx:usedRows) numButtons
    
    countSolution = length . filter id

solvePart2ILP :: [[Int]] -> [Int] -> Maybe Int
solvePart2ILP buttons joltages =
    let numButtons = length buttons
        numLights = length joltages
        maxJoltage = maximum (0:joltages)
    in dfs 0 (replicate numLights 0) 0 Nothing buttons joltages maxJoltage
  where
    dfs buttonIdx currentJoltages pressesSoFar best buttons joltages maxJoltage
        | buttonIdx >= length buttons =
            if all (\(a,b) -> a == b) (zip currentJoltages joltages)
            then Just $ maybe pressesSoFar (min pressesSoFar) best
            else best
        | maybe False (<= pressesSoFar) best = best
        | otherwise =
            let remainingNeeds = map (\(curr, target) -> max 0 (target - curr)) (zip currentJoltages joltages)
                sumRemaining = sum remainingNeeds
                pruned = if sumRemaining > 0
                         then let maxLightsPerButton = maximum (1 : map length (drop buttonIdx buttons))
                              in maybe False (\b -> pressesSoFar + ((sumRemaining + maxLightsPerButton - 1) `div` maxLightsPerButton) >= b) best
                         else False
            in if pruned then best
               else foldl (\bestResult presses ->
                   if maybe False (\b -> pressesSoFar + presses >= b) best then bestResult
                   else let newJoltages = zipWith (+) currentJoltages (map (\i -> if i `elem` (buttons !! buttonIdx) then presses else 0) [0..length joltages-1])
                            exceeds = any (\(a,b) -> a > b) (zip newJoltages joltages)
                        in if exceeds
                            then bestResult
                            else let result = dfs (buttonIdx+1) newJoltages (pressesSoFar+presses) bestResult buttons joltages maxJoltage
                                 in case (result, bestResult) of
                                     (Just r, Just br) -> Just (min br r)
                                     (Just r, Nothing) -> Just r
                                     _ -> bestResult
                  ) best [0..maxJoltage]

solve :: String -> (String, String)
solve inputData =
    let lines' = filter (not . all isSpace) (lines inputData)
    in if null lines'
       then ("0", "0")
       else let (p1, p2) = foldl' (\(acc1, acc2) line ->
                        let parsed = parseLine line
                        in if null (targetPattern parsed)
                           then (acc1, acc2)
                           else let numLights = length (targetPattern parsed)
                                    -- Part 1: Build incidence matrix
                                    buttonMatrix = map (\btn -> map (`elem` btn) [0..numLights-1]) (buttons parsed)
                                    requiredToggles = targetPattern parsed
                                    result1 = gaussianEliminationGF2 buttonMatrix requiredToggles
                                    newP1 = acc1 + fromMaybe 0 result1
                                    -- Part 2: ILP
                                    result2 = if length (joltages parsed) == numLights
                                              then solvePart2ILP (buttons parsed) (joltages parsed)
                                              else Nothing
                                    newP2 = acc2 + fromMaybe 0 result2
                                in (newP1, newP2)
                    ) (0, 0) lines'
            in (show p1, show p2)

main :: IO ()
main = do
    inputData <- readFile "../data/input.txt"
    let (part1, part2) = solve inputData
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
