{-# LANGUAGE OverloadedStrings #-}
import Data.Interval
import Data.IntervalSet
import Data.IntervalSet qualified as IS
import Data.List.Split qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [Text]
inputLines = T.lines <$> T.readFile "input/day5.txt"

toInterval :: Text -> Interval Int
toInterval t = Finite least <=..<= Finite most where
    (least', most') = T.breakOn "-" t
    (least, most) = (read (T.unpack least'), read (T.unpack $ T.drop 1 most')) :: (Int, Int)

ranges :: [Text] -> IntervalSet Int
ranges = foldr (IS.insert . toInterval) IS.empty

main :: IO ()
main = do
    inputs <- inputLines
    let (rangeLines : rest) = L.splitWhen (== "") inputs
    let fresh = ranges rangeLines
        ingredients = read . T.unpack <$> concat rest :: [Int]
    -- Part 1
    let isFresh i = i `IS.member` fresh
    print . length $ filter isFresh ingredients
    -- Part 2
    print . sum $ (\w -> width w + 1) <$> toList fresh
