import System.IO
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

type Point = (Integer, Integer)

parseCoordinates :: [String] -> [Point]
parseCoordinates lines = 
    let parseLine line = case break (== ',') (dropWhile (== ' ') line) of
            (xStr, ',' : yStr) -> case (reads xStr :: [(Integer, String)], reads (dropWhile (== ' ') yStr) :: [(Integer, String)]) of
                ([(x, "")], [(y, "")]) -> Just (x, y)
                _ -> Nothing
            _ -> Nothing
    in mapMaybe parseLine lines

solve :: String -> (String, String)
solve inputData = 
    let lines' = filter (not . null) $ map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse) $ lines inputData
        redTiles = parseCoordinates lines'
    in
    if length redTiles < 2
    then ("0", "0")
    else
        -- Part 1: Find largest rectangle area
        let maxAreaPart1 = maximum [(abs (x1 - x2) + 1) * (abs (y1 - y2) + 1) | 
                           (x1, y1) <- redTiles, 
                           (x2, y2) <- redTiles, 
                           (x1, y1) < (x2, y2)]
            
            -- Part 2: Coordinate compression
            allXSet = Set.fromList $ concatMap (\(x, _) -> [x, x + 1]) redTiles
            allYSet = Set.fromList $ concatMap (\(_, y) -> [y, y + 1]) redTiles
            allX = sort $ Set.toList allXSet
            allY = sort $ Set.toList allYSet
            
            xToCx = Map.fromList $ zip allX [0..]
            yToCy = Map.fromList $ zip allY [0..]
            
            width = length allX
            height = length allY
            
            -- Build grid (using list of lists for simplicity)
            initialGrid = replicate width $ replicate height False
            
            -- Mark boundary
            markBoundary grid = foldl' markTile grid redTiles
                where markTile g (x, y) = 
                        case (Map.lookup x xToCx, Map.lookup y yToCy) of
                            (Just cx, Just cy) -> update2D g cx cy True
                            _ -> g
            
            -- Connect consecutive tiles
            connectTiles grid = foldl' connectPair grid (zip redTiles (tail redTiles ++ [head redTiles]))
                where connectPair g ((x1, y1), (x2, y2))
                        | x1 == x2 = foldl' (\g' y -> case (Map.lookup x1 xToCx, Map.lookup y yToCy) of
                                                       (Just cx, Just cy) -> update2D g' cx cy True
                                                       _ -> g') g [min y1 y2..max y1 y2]
                        | y1 == y2 = foldl' (\g' x -> case (Map.lookup x xToCx, Map.lookup y1 yToCy) of
                                                       (Just cx, Just cy) -> update2D g' cx cy True
                                                       _ -> g') g [min x1 x2..max x1 x2]
                        | otherwise = g
            
            update2D grid x y val = take x grid ++ [take y (grid !! x) ++ [val] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid
            
            gridWithBoundary = connectTiles (markBoundary initialGrid)
            
            -- Point-in-polygon
            pointInPolygon (px, py) = 
                let checkEdge (x1, y1) (x2, y2) = 
                        if (y1 > py) /= (y2 > py)
                        then let intersectX = if y2 /= y1 
                                             then fromIntegral (py - y1) * fromIntegral (x2 - x1) / fromIntegral (y2 - y1) + fromIntegral x1
                                             else fromIntegral px
                             in px < floor intersectX
                        else False
                    edges = zip redTiles (tail redTiles ++ [head redTiles])
                in odd $ length $ filter id $ map (uncurry checkEdge) edges
            
            -- Flood fill (simplified - mark all interior points)
            floodFill grid = 
                let interiorPoints = [(cx, cy) | cx <- [0..width-1], cy <- [0..height-1],
                                                 not (grid !! cx !! cy),
                                                 pointInPolygon (allX !! cx, allY !! cy)]
                in foldl' (\g (cx, cy) -> update2D g cx cy True) grid interiorPoints
            
            filledGrid = floodFill gridWithBoundary
            
            -- Build prefix sum
            buildPrefix grid = 
                let prefixRow row = scanl1 (+) row
                    prefixCols = map prefixRow grid
                    prefixRows = transpose $ map prefixRow $ transpose prefixCols
                in replicate (width + 1) (replicate (height + 1) 0) -- Simplified for now
            
            -- Simplified Part 2: Use Set approach with sorted candidates
            validTilesSet = Set.fromList $ 
                [(allX !! cx, allY !! cy) | cx <- [0..width-1], cy <- [0..height-1], filledGrid !! cx !! cy]
            
            candidates = sortBy (\(_,_,_,_,a1) (_,_,_,_,a2) -> compare a2 a1) $
                [(x1, y1, x2, y2, (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)) |
                 (x1, y1) <- redTiles, (x2, y2) <- redTiles, (x1, y1) < (x2, y2)]
            
            isValidRect (x1, y1) (x2, y2) = 
                let minX = min x1 x2
                    maxX = max x1 x2
                    minY = min y1 y2
                    maxY = max y1 y2
                    rectPoints = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
                in all (`Set.member` validTilesSet) rectPoints
            
            maxAreaPart2 = case find (\(x1, y1, x2, y2, _) -> isValidRect (x1, y1) (x2, y2)) candidates of
                Just (_, _, _, _, area) -> area
                Nothing -> 0
        in (show maxAreaPart1, show maxAreaPart2)

main :: IO ()
main = do
  data' <- readFile "../data/input.txt"
  let (part1, part2) = solve data'
  putStrLn $ "Part 1: " ++ part1
  putStrLn $ "Part 2: " ++ part2
