import System.IO
import Debug.Trace
import Data.List


readPositions :: FilePath -> IO [Int]
readPositions path = (map read <$> words) . commasToSpaces <$> readFile path
    where
        commasToSpaces :: String -> String
        commasToSpaces = map (\x -> if x == ',' then ' ' else x)


average :: [Int] -> Int
average xs = div (sum xs) (length xs)

median :: [Int] -> Int
median xs = if odd n then
                ys !! mid
            else
                round $ fromIntegral (ys !! (mid-1) + ys !! mid) / 2.0
    where
        ys = sort xs
        n = length xs
        mid = n `div` 2


fuelCost :: Int -> [Int] -> Int
fuelCost n xs = sum (map (\x -> abs (x - n)) xs)

triangular :: Int -> Int
triangular n = n*(n+1) `div` 2

weightedFuelCost :: Int -> [Int] -> Int
weightedFuelCost n xs = sum (map (\x -> triangular $ abs (x - n)) xs)

solveMinFuel :: [Int] -> Int
solveMinFuel xs = fuelCost (median xs) xs

solveWeightedMinFuel :: [Int] -> Int
solveWeightedMinFuel xs = minimum $ map f (enumFromTo (minimum xs) (maximum xs))
    where
        f x = weightedFuelCost x xs
