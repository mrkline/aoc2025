{-# LANGUAGE OverloadedStrings #-}
import Data.Bifunctor
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [Text]
inputLines = T.lines <$> T.readFile "input/day4.txt"

type Coord = (Int, Int)

adjacents :: Coord -> [Coord]
adjacents (x, y) = [
    -- Above
    (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    -- Left and right
    (x - 1, y),
    (x + 1, y),
    -- Under
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
    ]

type Grid = HashSet Coord

accessible :: Grid -> Coord -> Bool
accessible rollMap loc = neighbors < 4 where
    neighbors = sum $ fromEnum . (`HS.member` rollMap) <$> adjacents loc

removeRolls :: Grid -> Grid
removeRolls prev = HS.filter (not . accessible prev) prev

main :: IO ()
main = do
    inputs <- inputLines
    -- Work our way towards (y, (x, char)) to put in a sparse map.
    let indexedLines = zip [0..] inputs :: [(Int, Text)]
        indexedChars = zip [0..] . T.unpack :: Text -> [(Int, Char)]
        coordinateChars = second indexedChars <$> indexedLines :: [(Int, [(Int, Char)])]
        flattened = concatMap (\(y, c) -> fmap (y,) c) coordinateChars :: [(Int, (Int, Char))]
        rolls = filter (\(_, (_, c)) -> c == '@') flattened
        rollMap = HS.fromList $ (\(y, (x, _)) -> (x, y)) <$> rolls

    -- Part 1
    print $ length rollMap - length (removeRolls rollMap)
    -- Part 2
    let rounds = iterate removeRolls rollMap
        removedEachRound = (\(before, after) -> length before - length after) <$> zip rounds (tail rounds)
        usefulRounds = takeWhile (/= 0) removedEachRound
    print $ sum usefulRounds
