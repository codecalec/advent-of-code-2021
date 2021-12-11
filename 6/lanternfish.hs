import System.IO
import Data.Char
import Data.List
import Debug.Trace

type FishAges = [Int]

getFishAges :: FilePath -> IO FishAges
getFishAges path = f <$> readFile path
    where
        f :: String -> FishAges
        f (x:xs) = if isDigit x then digitToInt x:f xs else f xs
        f "" = []

iterateDay :: FishAges -> FishAges
iterateDay (x:xs) =
    if x==0 then 8:6:iterateDay xs
    else x-1:iterateDay xs
iterateDay [] = []

fishOnDay :: Int -> FishAges -> FishAges
fishOnDay n fishes = iterate iterateDay fishes !! n

type FishAgesMap = [(Int,Int)]

findInMap :: Int -> FishAgesMap -> Maybe Int
findInMap n ((a,b):xs) = if n == a then Just b  else findInMap n xs
findInMap n [] = Nothing

getFishMap :: FishAges -> FishAgesMap
getFishMap ages = f $ group $ sort ages
    where
        f = map (\a -> (head a, length a))

iterateFishMap :: FishAgesMap -> FishAgesMap
iterateFishMap ((0,n):xs) = (8,n):(6,n):iterateFishMap xs
iterateFishMap ((m,n):xs) = (m-1,n):iterateFishMap (sort xs)
iterateFishMap [] = []


combineMap :: FishAgesMap -> FishAgesMap
combineMap [] = []
combineMap agesMap@((6,_):xs) = (6,value):combineMap ys
    where
        ys = filter (\(x,y) -> x /= 6) xs
        value = totalFishInMap $ filter (\(x,y) -> x == 6) agesMap

combineMap agesMap@((8,_):xs) = (8,value):combineMap ys
    where
        ys = filter (\(x,y) -> x /= 8) xs
        value = totalFishInMap $ filter (\(x,y) -> x == 8) agesMap
combineMap (x:xs) = x:combineMap xs


fishMapOnDay :: Int -> FishAgesMap -> FishAgesMap
fishMapOnDay n fishes = iterate (combineMap . iterateFishMap) fishes !! n

totalFishInMap :: FishAgesMap -> Int
totalFishInMap [] = 0
totalFishInMap xs = foldr f 0 xs
    where
        f (a,b) y = b + y
