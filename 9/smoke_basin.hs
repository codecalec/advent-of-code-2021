import System.IO

import Data.Array
import Data.Char
import Data.Graph
import Data.Maybe
import Data.List


type HeightMap = Array (Int, Int) Int


readNumbers :: FilePath -> IO [[Int]]
readNumbers path = map (map digitToInt) . lines <$> readFile path

formGrid :: [[Int]] -> HeightMap
formGrid xss = array ((0, 0),(n-1,m-1)) (f xss 0)
    where
        n = length xss
        m = length (xss !! 1)

        f :: [[Int]] -> Int -> [((Int,Int),Int)]
        f (xs:xss) row = g xs row 0 ++ f xss (row+1)
        f [] _ = []

        g :: [Int] -> Int -> Int -> [((Int,Int),Int)]
        g (x:xs) i j = ((i,j),x):g xs i (j+1)
        g [] _ j = []

sumRiskLevel :: [((Int,Int),Int)] -> Int
sumRiskLevel xs = sum $ map (\(x,v) -> 1 + v) xs

findLowPoints :: HeightMap -> [((Int,Int),Int)]
findLowPoints heights = filter (\((x,y),_) -> isLowPoint (x,y) heights) (assocs heights)

isLowPoint :: (Int,Int) -> HeightMap -> Bool
isLowPoint (row, col) heights  = all (height <) surroundingPoints
    where
        height = heights ! (row,col)
        (minRow, minCol) = (0,0)
        (maxRow, maxCol) = maximum $ indices heights
        getValue (row,col)
            | row > maxRow = maxBound::Int
            | row < minRow = maxBound::Int
            | col > maxCol = maxBound::Int
            | col < minCol = maxBound::Int
            | otherwise = heights ! (row,col)
        surroundingPoints = map getValue [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

gridGraph :: HeightMap -> (Graph, Vertex -> (Int, (Int,Int), [(Int,Int)]), (Int,Int)-> Maybe Vertex)
gridGraph heights
    = graphFromEdges $ filter (\(x,y,z) -> x/=9) [(v, point, findConnectedVertices point) | (point,v) <- assocs heights]
    where
        (minRow, minCol) = (0,0)
        (maxRow, maxCol) = maximum $ indices heights
        findConnectedVertices :: (Int,Int) -> [(Int,Int)]
        findConnectedVertices (row,col)
            = filter (\(row',col') -> row' `elem` [minRow..maxRow] && col' `elem` [minCol..maxCol]) [(row-1, col), (row+1, col), (row, col-1), (row, col+1)]

main :: IO ()
main = do
    grid <- formGrid <$> readNumbers "input"
    let (graph, nodeFromVertex, vertexFromKey) = gridGraph grid
    let lowPoints = findLowPoints grid
    let lengths = map (\((row,col),v) -> length $ reachable graph (fromMaybe (-1) $ vertexFromKey (row,col))) lowPoints
    print (product $ take 3 $ sortBy (flip compare) lengths)
