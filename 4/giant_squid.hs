import System.IO

import Debug.Trace

import Data.Char
import Data.List

type Board = [[Int]]

checkBoard :: Board -> Bool
checkBoard = elem []

checkBoards :: [Board] -> Maybe Board
checkBoards [] = Nothing
checkBoards (x:xs) = if checkBoard x then Just x else checkBoards xs

unmarkedSum :: Board -> Int
unmarkedSum = sum . map sum

markNumber :: Int -> Board -> Board
markNumber num = map removeNumber
    where
        removeNumber xs = front ++ drop 1 back
            where (front, back) = break (num ==) xs

getBoards :: [String] -> [Board]
getBoards lines = f (drop 2 lines)
    where
        f :: [String] -> [Board]
        f [] = []
        f lines = makeBoard xs : f (drop 1 ys)
            where
                (xs, ys) =  splitAt 5 lines

makeBoard :: [String] -> Board
makeBoard = map (map (read . removeSpaces) . words)
    where
        removeSpaces :: String -> String
        removeSpaces word = filter isDigit word

readInput :: FilePath -> IO [String]
readInput path = lines <$> readFile path


winningBoard :: [Board] -> [Int] -> (Board, Int)
winningBoard boards numbers =  callNumbers boards numbers
    where
        callNumbers boards (n:ns) =
            case checkBoards newBoards of
                Just x -> (x, n)
                Nothing -> callNumbers newBoards ns
            where newBoards = map (markNumber n) boards
        callNumbers boards [] = error "run out of numbers"

solveBoards :: [String] -> Int
solveBoards lines = do
    let bingoNumbers = map read $ words $ map (\x -> if x==',' then ' ' else x) $ head lines
    let rowBoards = getBoards lines
    let boards = rowBoards ++ map transpose rowBoards
    let (winBoard,n) = winningBoard boards bingoNumbers
    unmarkedSum winBoard * n

worstBoard :: [Board] -> [Int] -> (Board, Int)
worstBoard boards numbers = callNumbers boards numbers
    where
        callNumbers [x] numbers = (x, head numbers)
        callNumbers boards (n:ns) = callNumbers newBoards ns
            where newBoards =  removeFinishedBoards $ map (markNumber n) boards
        callNumbers boards [] = error "run out of numbers"

removeFinishedBoards :: [Board] -> [Board]
removeFinishedBoards = filter (not . checkBoard)

solveWorstBoards :: [String] -> (Int,Int)
solveWorstBoards lines = do
    let bingoNumbers = map read $ words $ map (\x -> if x==',' then ' ' else x) $ head lines
    let rowBoards = getBoards lines
    let colBoards = map transpose rowBoards
    let (worstRow, nRow) = worstBoard rowBoards bingoNumbers
    let (worstCol, nCol) = worstBoard colBoards bingoNumbers
    --unmarkedSum worst * n
    ( unmarkedSum worstCol * nCol, unmarkedSum worstRow * nRow)
