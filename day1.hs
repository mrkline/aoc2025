{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [Text]
inputLines = T.lines <$> T.readFile "input/day1.txt"

toTurn :: Text -> Int
toTurn l = sig * val where
    (d, v) = T.splitAt 1 l
    sig = case d of
        "L" -> -1
        "R" -> 1
        wut -> error $ "odd direction: " <> T.unpack wut
    val = read $ T.unpack v

-- Given a list of turns, turn the dial starting at 50 and collect each stop
stops :: [Int] -> [Int]
stops ts = 50 : turn' 50 ts

turn' :: Int -> [Int] -> [Int]
turn' _ [] = []
turn' prev (t:ts) = next : turn' next ts
    where next = (prev + t) `mod` 100

countZeroes :: [Int] -> Int
countZeroes = length . filter (== 0)

-- I give up at modulo math. Why do one turn when you can do many unit turn?
explode :: [Int] -> [Int]
explode [] = []
explode (t:ts) = exploded ++ explode ts where
    exploded = if t < 0
        then replicate (abs t) (-1)
        else replicate t 1

main :: IO ()
main = do
    inputs <- fmap toTurn <$> inputLines
    -- Part 1
    print $ countZeroes $ stops inputs
    -- Part 2 (sad)
    print $ countZeroes $ stops $ explode inputs
