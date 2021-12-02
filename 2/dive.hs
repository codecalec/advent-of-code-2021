import Data.Char
import Debug.Trace

data Direction = Up | Down | Forward deriving (Show)
type Command = (Direction, Int)

getCommands :: FilePath -> IO [(Direction, Int)]
getCommands path = map getInstructions . lines <$> readFile path

getInstructions :: String -> (Direction, Int)
getInstructions line = (direction, read ys)
    where
        (xs, ys) = break isNumber line
        direction =
            case head xs of
                'f' -> Forward
                'u' -> Up
                'd' -> Down

isVertical :: Direction -> Bool
isVertical Down = True
isVertical Up = True
isVertical Forward = False

directionValue :: Command -> Int
directionValue (Up, val) = -val
directionValue (Down, val) = val
directionValue (Forward, val) = val

locationResult :: [(Direction, Int)] -> Int
locationResult commands = solveVertical xs * solveHorizontal ys
    where
        xs = filter (isVertical . fst) commands
        ys = filter (not . isVertical . fst) commands
        solveVertical = sum . map directionValue
        solveHorizontal = sum . map directionValue

aimResult :: [Command] -> Int
aimResult commands = horizontal * vertical
    where
        (_, _, vertical, horizontal) = aimResultInner commands 0 0 0

aimResultInner :: [Command] -> Int -> Int -> Int -> ([Command], Int, Int, Int)
aimResultInner (x:xs) aim vertical horizontal
    | isVertical (fst x)  = aimResultInner xs (aim + directionValue x) vertical horizontal
    | otherwise = aimResultInner xs aim (vertical + aim * directionValue x) (horizontal + directionValue x)

aimResultInner [] aim vertical horizontal = ([], aim, vertical, horizontal)
