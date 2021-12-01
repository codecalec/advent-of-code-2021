import System.IO

countincreases :: [Int] -> Int
countincreases depthpoints = sum [fromEnum (x > y) | (x,y) <- zip xs ys]
    where
        xs = drop 1 depthpoints
        ys = take (length depthpoints - 1) depthpoints


calculatewindows :: [Int] -> [Int]
calculatewindows depthpoints = [sum $ take 3 $ drop i depthpoints | i <- [0..(n-2)]]
    where
        n = length depthpoints


convertToInt :: [String] -> [Int]
convertToInt = map (read::String->Int)

readdepths :: FilePath -> IO [Int]
readdepths path = convertToInt <$> fmap lines (readFile path)
