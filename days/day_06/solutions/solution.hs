import System.IO
import Data.List
import Data.Char
import Data.Maybe

readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

data Problem = Problem { startCol :: Int, endCol :: Int, op :: Char }

solve :: String -> (Integer, Integer)
solve inputData = 
    let inputLines = lines inputData
        trimmedLines = map trim inputLines
    in
    if null trimmedLines
    then (0, 0)
    else
        let maxLen = maximum (map length trimmedLines)
            paddedLines = map (`padRight` maxLen) trimmedLines
            opRowIdx = length paddedLines - 1
            opRow = paddedLines !! opRowIdx
            numRows = take opRowIdx paddedLines
            
            -- Find problem boundaries (columns that are all spaces)
            isSpaceCol = [all (\line -> col >= length line || line !! col == ' ') paddedLines | col <- [0..maxLen-1]]
            
            -- Group columns into problems
            problems = findProblems 0 maxLen isSpaceCol opRow []
            
            -- Part 1: Extract numbers horizontally
            part1Total = sum (map (solveProblem numRows) problems)
            
            -- Part 2: Parse vertically (columns, right-to-left)
            -- Approach: Use pure functional transformations
            part2Total = sum (map (solveProblemPart2 numRows isSpaceCol) problems)
        in
            (part1Total, part2Total)

findProblems :: Int -> Int -> [Bool] -> String -> [Problem] -> [Problem]
findProblems i maxLen isSpaceCol opRow acc
    | i >= maxLen = reverse acc
    | isSpaceCol !! i = findProblems (i+1) maxLen isSpaceCol opRow acc
    | otherwise = 
        let startCol = i
            endCol = findProblemEnd i maxLen isSpaceCol
            op = findOp (drop startCol (take endCol opRow))
        in
            case op of
                Just c -> findProblems endCol maxLen isSpaceCol opRow (Problem startCol endCol c : acc)
                Nothing -> findProblems endCol maxLen isSpaceCol opRow acc

findProblemEnd :: Int -> Int -> [Bool] -> Int
findProblemEnd i maxLen isSpaceCol
    | i >= maxLen = i
    | isSpaceCol !! i = i
    | otherwise = findProblemEnd (i+1) maxLen isSpaceCol

findOp :: String -> Maybe Char
findOp str = find (`elem` "+*") str

solveProblem :: [String] -> Problem -> Integer
solveProblem numRows (Problem startCol endCol op) =
    let numbers = concatMap (parseNumbers startCol endCol) numRows
    in
        if null numbers
        then 0
        else
            let result = case op of
                    '+' -> sum numbers
                    '*' -> product numbers
            in result

parseNumbers :: Int -> Int -> String -> [Integer]
parseNumbers startCol endCol row =
    let problemStr = trim (take (endCol - startCol) (drop startCol row))
        parts = words problemStr
    in
        mapMaybe parseInteger parts

parseInteger :: String -> Maybe Integer
parseInteger s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

padRight :: String -> Int -> String
padRight s len = s ++ replicate (len - length s) ' '

solveProblemPart2 :: [String] -> [Bool] -> Problem -> Integer
solveProblemPart2 numRows isSpaceCol (Problem startCol endCol op) =
    let -- Extract column strings (transpose)
        colStrings = reverse [colStr | col <- [startCol..endCol-1],
                                       not (isSpaceCol !! col),
                                       let colStr = trim (map (\row -> row !! col) numRows),
                                       not (null colStr)]
        
        -- Parse numbers from column strings
        numbers = mapMaybe (\s -> parseInteger (filter isDigit s)) colStrings
        
        -- Apply operator
        result = case op of
            '+' -> sum numbers
            '*' -> product numbers
    in if null numbers then 0 else result

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

main :: IO ()
main = do
    content <- readInputRaw "../data/input.txt"
    let (part1, part2) = solve content
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
