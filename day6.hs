{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [Text]
inputLines = T.lines <$> T.readFile "input/day6.txt"

parse :: [Text] -> ([Int], Text)
parse problem = (vals, op) where
    (tvals, op) = fromJust $ unsnoc problem
    vals = read . T.unpack <$> tvals

solve :: ([Int], Text) -> Int
solve (vals, op) = case op of
    "*" -> foldl' (*) 1 vals
    "+" -> foldl' (+) 0 vals
    wut -> error $ "odd op " <> T.unpack wut

-- Cleanup after reading right-to-left, top-to-bottom
unceph :: String -> [String]
unceph s = digits : maybeOp where
    stripped = filter (not . isSpace) s
    digits = takeWhile isDigit stripped
    -- Last token might be our operator
    ad = drop (length digits) stripped
    maybeOp = case uncons ad of
        Just ('*', _) -> ["*"]
        Just ('+', _) -> ["+"]
        _ -> []

main :: IO ()
main = do
    inputs <- inputLines

    -- Part 1
    let wordLines = T.words <$> inputs
        problems = transpose wordLines
        parsed = parse <$> problems
    print . sum $ solve <$> parsed
    -- Part 2
    let rtl = T.reverse <$> inputs
        cephd = transpose $ T.unpack <$> rtl
    let uncephed = concatMap unceph cephd
        -- Problems are now split by an empty string
        repacked = splitOn [""] $ T.pack <$> uncephed
    print . sum $ solve . parse <$> repacked
