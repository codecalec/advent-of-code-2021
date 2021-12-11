import System.IO

import Debug.Trace

import Data.Char
import Data.List


type Point = (Int, Int)
type Line = (Point, Point)

readLines :: FilePath -> IO [Line]
readLines path = map coordsToLine . lines <$> readFile path

coordsToLine :: String -> Line
coordsToLine coords = (digitsToPoint first, digitsToPoint second)
    where
        (first, second) = (head $ words coords, words coords !! 2)

        digitsToPoint :: String -> Point
        digitsToPoint xs = (read $ takeWhile (/=',') xs, read $ drop 1 $ dropWhile (/=',') xs)

findNumSafePoints :: [Line] -> Int
findNumSafePoints lines = length $ filter f $ findPointOccurences lines
    where
        f (point,n) = n >= 2


findPointOccurences :: [Line] -> [(Point, Int)]
findPointOccurences lines = zip (sort points) (map length $ group $ sort points)
    where
        points = concatMap lineToPoints lines

getRange :: Int -> Int -> [Int]
getRange a b = if a == b then [a] else a:getRange (a + inc) b
    where
        inc = if b > a then 1 else -1

lineToPoints :: Line -> [Point]
lineToPoints ((x1,y1), (x2, y2))
    | x1 == x2 = [ (x1,y)  | y <- getRange y1 y2]
    | y1 == y2 = [ (x,y1) | x <- getRange x1 x2]
    | otherwise = zip (getRange x1 x2) (getRange y1 y2)
