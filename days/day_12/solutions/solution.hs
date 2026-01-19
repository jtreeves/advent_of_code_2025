import System.IO
import Data.List
import Data.Char

readInputRaw :: FilePath -> IO String
readInputRaw filePath = readFile filePath

solve :: String -> (String, String)
solve inputData =
    let inputLines = lines inputData
        trimmedLines = map (dropWhile isSpace . reverse . dropWhile isSpace . reverse) inputLines
    in
    if null trimmedLines
    then ("0", "Final star")
    else
        let (shapeAreas, queryStart) = parseShapes trimmedLines 0 0 []
            possibleCount = countPossibleQueries (drop queryStart trimmedLines) shapeAreas
        in
            (show possibleCount, "Final star")

parseShapes :: [String] -> Int -> Int -> [Int] -> ([Int], Int)
parseShapes [] _ _ acc = (reverse acc, 0)
parseShapes inputLines i shapeIdx acc
    | shapeIdx >= 6 = (reverse acc, i)
    | i >= length inputLines = (reverse acc, i)
    | otherwise =
        let line = inputLines !! i
            trimmed = dropWhile isSpace line
        in
        if not (null trimmed) && last trimmed == ':'
        then
            case reads (init trimmed) :: [(Int, String)] of
                [(shapeNum, "")] | shapeNum == shapeIdx ->
                    let shapeGrid = take 3 (drop (i + 1) inputLines)
                        area = sum (map (length . filter (=='#')) shapeGrid)
                    in
                    parseShapes inputLines (i + 4) (shapeIdx + 1) (area : acc)
                _ -> parseShapes inputLines (i + 1) shapeIdx acc
        else
            parseShapes inputLines (i + 1) shapeIdx acc

countPossibleQueries :: [String] -> [Int] -> Int
countPossibleQueries [] _ = 0
countPossibleQueries (line:rest) shapeAreas =
    let trimmed = dropWhile isSpace line
    in
    if null trimmed
    then countPossibleQueries rest shapeAreas
    else
        case break (==':') trimmed of
            (dims, ':':countsStr) ->
                case break (=='x') dims of
                    (widthStr, 'x':heightStr) ->
                        case (reads widthStr :: [(Int, String)], reads heightStr :: [(Int, String)]) of
                            ([(width, "")], [(height, "")]) ->
                                let counts = map read (words (dropWhile isSpace countsStr))
                                in
                                if length counts == 6
                                then
                                    let regionArea = width * height
                                        requiredArea = sum (zipWith (*) shapeAreas counts)
                                    in
                                    (if requiredArea <= regionArea then 1 else 0) + countPossibleQueries rest shapeAreas
                                else countPossibleQueries rest shapeAreas
                            _ -> countPossibleQueries rest shapeAreas
                    _ -> countPossibleQueries rest shapeAreas
            _ -> countPossibleQueries rest shapeAreas

main :: IO ()
main = do
    inputData <- readInputRaw "../data/input.txt"
    let (part1, part2) = solve inputData
    putStrLn $ "Part 1: " ++ part1
    putStrLn $ "Part 2: " ++ part2
