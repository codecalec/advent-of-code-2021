import System.IO
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad

 --aaaa
--b    c
--b    c
 --dddd
--e    f
--e    f
 --gggg


type DigitMap = Map Int Int
type CharMap = Map Char Char

readDigits :: FilePath -> (IO [[String]],IO [[String]])
readDigits path = (map signals <$> segments, map digits <$> segments)
    where
        segments = map words . lines <$> readFile path
        indexOfPipe :: [String] -> Int
        indexOfPipe xs = case elemIndex "|" xs of
            Just n -> n
            Nothing -> error "| not in list"
        signals xs = take (indexOfPipe xs) xs
        digits xs = drop (indexOfPipe xs + 1) xs

identifyLengthOf5 :: CharMap -> String -> Char
identifyLengthOf5 charMap xs
    | (charMap Map.! 'c') `elem` xs && (charMap Map.! 'f') `elem` xs = '3'
    | (charMap Map.! 'b') `elem` xs && (charMap Map.! 'd') `elem` xs = '5'
    | otherwise = '2'

identifyLengthOf6 :: CharMap -> String -> Char
identifyLengthOf6 charMap xs
    | (charMap Map.! 'c') `notElem` xs || (charMap Map.! 'f') `notElem` xs = '6'
    | (charMap Map.! 'b') `elem` xs && (charMap Map.! 'd') `elem` xs = '9'
    | otherwise = '0'

makeCharMap :: [String] -> CharMap
makeCharMap xs = Map.fromList charPairs
    where
        word1 = case find (\x -> length x == 2) xs of
           Just x -> x
           Nothing -> error "err in word1"

        word4 = case find (\x -> length x == 4) xs of
           Just x -> x
           Nothing -> error "err in word1"
        charPairs = zip "cf" word1  ++ zip "bd" (filter (`notElem` word1) word4)

decodeDigits :: CharMap -> [String] -> Int
decodeDigits charMap xs = read $ map decodeDigit xs
    where
        digitMap = Map.fromList [(2,'1'), (3,'7'), (4,'4'), (7,'8')]
        decodeDigit ys
            | length ys == 5 = identifyLengthOf5 charMap ys
            | length ys == 6 = identifyLengthOf6 charMap ys
            | otherwise = case Map.lookup (length ys) digitMap of
                Just n -> n
                Nothing -> error "err"

totalDecoded :: [[String]] -> [[String]] -> Int
totalDecoded signalsList digitsList = sum $ zipWith (curry f) signalsList digitsList
    where
        f :: ([String], [String]) -> Int
        f (signals, digits) = decodeDigits (makeCharMap signals) digits

findEasyOccurences :: [String] -> Int
findEasyOccurences digits = sum $ map eval digits
    where
        digitMap = Map.fromList [(2,1), (3,7), (4,4), (7,8)]
        eval digit = case Map.lookup (length digit) digitMap of
            Just _ -> 1
            Nothing -> 0

totalEasyOccurences :: [[String]] -> Int
totalEasyOccurences xs = sum $ map findEasyOccurences xs
