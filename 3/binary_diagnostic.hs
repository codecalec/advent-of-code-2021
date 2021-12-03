import Data.List
import Data.Char
import Debug.Trace

getCodes :: FilePath -> IO [String]
getCodes path = lines <$> readFile path

findMostCommon :: String -> Char
findMostCommon xs = if ones >= zeroes then '1' else '0'
    where
        tupleLengths (a,b) = (length a, length b)
        (zeroes, ones) = tupleLengths $ partition (== '0') xs

findLeastCommon :: String -> Char
findLeastCommon xs = if ones >= zeroes then '0' else '1'
    where
        tupleLengths (a,b) = (length a, length b)
        (zeroes, ones) = tupleLengths $ partition (== '0') xs


flipCodes :: [String] -> [String]
flipCodes = transpose

calcPower :: [String] -> Int
calcPower codes = binaryToInt gamCodes * binaryToInt epsCodes
    where
        gamCodes = map (digitToInt . findMostCommon) $ flipCodes codes
        epsCodes = map bitFlip gamCodes

bitFlip :: Int -> Int
bitFlip 0 = 1
bitFlip 1 = 0

binaryToInt :: [Int] -> Int
binaryToInt list = go (reverse list) 0
    where
        go [] _ = 0
        go (x:xs) n = x*(2^n) + go xs (n+1)

calcLifeRating :: [String] -> Int
calcLifeRating codes = calcOxygen codes * calcCO2 codes

calcOxygen :: [String] -> Int
calcOxygen codes = binaryToInt $ map digitToInt (head $ f 0 codes)
    where
        getCommon :: Int -> [String] -> Int
        getCommon n xs = digitToInt $ findMostCommon (flipCodes xs !! max 0 n)

        filterCommon :: Int -> Int -> [String] -> [String]
        filterCommon n common xs = filter (\x -> digitToInt (x !! max 0 n) == common) xs

        f :: Int -> [String] -> [String]
        f _ [x] = [x]
        f _ [] = error "help"
        f n list@(x:xs) = f (n+1) $ filterCommon n common list
            where common = getCommon n list

calcCO2 :: [String] -> Int
calcCO2 codes = binaryToInt $ map digitToInt (head $ f 0 codes)
    where
        getCommon :: Int -> [String] -> Int
        getCommon n xs = digitToInt $ findLeastCommon (flipCodes xs !! max 0 n)

        filterCommon :: Int -> Int -> [String] -> [String]
        filterCommon n common xs = filter (\x -> digitToInt (x !! max 0 n) == common) xs

        f :: Int -> [String] -> [String]
        f _ [x] = [x]
        f _ [] = error "help"
        f n list@(x:xs) = f (n+1) $ filterCommon n common list
            where common = getCommon n list
