{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [Text]
inputLines = T.lines <$> T.readFile "input/day3.txt"

largestN :: Int -> Text -> Int
largestN n whole
    | n <= 0 = error "need a natural number of digits"
    | otherwise = read $ digits (n - 1) whole where
        digits i t = if i < 0
            then []
            else let (next, rest) = selectLeaving i t in next : digits (i - 1) rest

-- Leaving n digits at the end, select the largest digit and the string that remains after it.
selectLeaving :: Int -> Text -> (Char, Text)
selectLeaving n t = let
    choices = T.dropEnd n t
    li = largestIndex $ T.unpack choices
    choiceAndRest = T.drop li t
    in (T.head choiceAndRest, T.tail choiceAndRest)

-- Find the index of the largest char (digit) in the list
largestIndex :: [Char] -> Int
largestIndex cs = fst $ maxof ics where
    -- maximumBy biases to the rightmost, we want the leftmost
    maxof :: [(a, Char)] -> (a, Char)
    maxof [] = error "no largest index of empty string"
    maxof [x] = x
    maxof (x:xs) = let rest = maxof xs in if snd x >= snd rest then x else rest
    ics = zip [0..] cs

part1Line :: Text -> Int
part1Line = largestN 2

part2Line :: Text -> Int
part2Line = largestN 12

main :: IO ()
main = do
    inputs <- inputLines
    print . sum $ part1Line <$> inputs
    print . sum $ part2Line <$> inputs
