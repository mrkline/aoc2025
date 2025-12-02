{-# LANGUAGE OverloadedStrings #-}
import Data.Bifunctor
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputText :: IO Text
inputText = T.strip <$> T.readFile "input/day2.txt"

inputPairs :: Text -> [Text]
inputPairs = T.splitOn ","

inputPair :: Text -> (Int, Int)
inputPair = bimap parse (parse . T.drop 1) . T.breakOn "-" where
    parse = read . T.unpack

rangeOf :: (Int, Int) -> [Int]
rangeOf (a, b) = [a .. b]

isInvalid :: Int -> Bool
isInvalid i = firstHalf == secondHalf where
    digits = show i
    (firstHalf, secondHalf) = splitAt (length digits `div` 2) digits

isInvalid2 :: Int -> Bool
isInvalid2 i = any (digits `isPrefixOf`) patterns where
    digits = show i
    -- Haha laziness: Check if our digits are the prefix to any infinite patterns
    -- made from sequences of said digits.
    patterns = cycle <$> sequences
    -- Skip first tails, which is just digits, and the last, which is [],
    -- e.g. sequences 123123 -> [23123, 3123, 123, 23, 3]
    sequences = tail $ init $ tails digits

main :: IO ()
main = do
    inputs <- fmap inputPair . inputPairs <$> inputText
    let ranges = rangeOf <$> inputs

    -- Part 1
    let invalids = filter isInvalid <$> ranges
    print $ sum $ concat invalids

    -- Part 2
    let invalids2 = filter isInvalid2 <$> ranges
    print $ sum $ concat invalids2
