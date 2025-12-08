{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.ST
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.List
import Data.STRef
import Data.Text qualified as T
import Data.Text.IO qualified as T

inputLines :: IO [String]
inputLines = fmap T.unpack . T.lines <$> T.readFile "input/day7.txt"

firstLine :: [Char] -> Vector Bool
firstLine = V.fromList . map (== 'S')

isSplitter :: [Char] -> Vector Bool
isSplitter = V.fromList . map (== '^')

split :: Vector Bool -> Vector Bool -> (Vector Bool, Int)
split beams splitters = (newBeams, numSplits) where
    splits = V.zipWith (&&) beams splitters :: Vector Bool
    numSplits = V.length $ V.filter id splits
    -- We get a new beam if we're next to a splitter, or if we're a beam and NOT a splitter.
    isBeam i = splits V.!? (i - 1) == Just True ||
        splits V.!? (i + 1) == Just True ||
        (beams V.! i && not (splits V.! i))
    newBeams = V.map isBeam $ V.generate (V.length beams) id

-- Given the index of the starting beam, count the number of paths we take
splitTime :: Int -> [Vector Bool] -> Int
splitTime x ss = runST $ do
    sm <- newSTRef mempty
    splitTime' sm 0 x ss

-- It's memoization time! Keep a map of (y, x) to paths that passed through there so far.
splitTime' :: STRef s (HashMap (Int, Int) Int) -> Int -> Int -> [Vector Bool] -> ST s Int
splitTime' _ _ _ [] = pure 1 -- We made it to the bottom, don't bother memoizing
splitTime' sm y x (s : ss) = do
    mem <- readSTRef sm
    case mem HM.!? (y, x) of
        Just known -> pure known -- We've been here.
        Nothing -> do
            -- Walk each path, adding their result
            paths <- if s V.! x
                then do
                    lpath <- splitTime' sm (y + 1) (x - 1) ss
                    rpath <- splitTime' sm (y + 1) (x + 1) ss
                    pure $ lpath + rpath
                else splitTime' sm (y + 1) x ss
            -- NB: We'll pass through a coordinate multiple times. Add, don't overwrite.
            modifySTRef sm $ HM.insertWith (+) (y, x) paths
            pure paths


main :: IO ()
main = do
    inputs <- inputLines
    let start = firstLine $ head inputs
        splits = isSplitter <$> tail inputs
    -- Part 1
    let foldRow (beams, splitsSoFar) splitters = let
            (newBeams, newSplits) = split beams splitters
            in (newBeams, newSplits + splitsSoFar)
    print . snd $ foldl' foldRow (start, 0) splits
    -- Part 2
    let startIndex = fromJust $ V.findIndex id start
    print $ splitTime startIndex splits
