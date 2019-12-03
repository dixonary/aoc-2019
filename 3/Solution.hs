-- Control flow
import Control.Monad
import Data.Function ((&), on)
import Data.Functor ((<&>))
import System.Environment
import System.IO.Unsafe

-- Data Structures
import qualified Data.List as List
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Bool

import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.List.Split (splitOn) -- from split

import Debug.Trace

--------------------------------------------------------------------------------
-- IO Wrangling

type Point = (Int, Int)
type Segment = (Point, Point)
type Wire = [Segment]

-- The initial data structure to parse the data into.
type Input = (Wire, Wire)
-- The final data structure to write the data out to.
type Output = Int


parseInput :: String -> Input
parseInput inputRaw = let
    [wireA, wireB] = lines inputRaw
        <&> splitOn ","
        <&> (`parseMoves` [(0,0)])
        <&> (\w -> zip w (tail w))

    in (wireA, wireB)


parseMoves :: [String] -> [Point] -> [Point]
parseMoves [] w = w
parseMoves (m:ms) (w:ws) = parseMoves ms $ parseMove w m : w : ws


parseMove :: Point -> String -> Point
parseMove (x,y) (d:v) = 
    let 
        n = read v 
    in case d of
            'U' -> (x, y+n)
            'D' -> (x, y-n)
            'L' -> (x-n, y)
            'R' -> (x+n, y)

--------------------------------------------------------------------------------
-- Solution

meet :: Segment -> Segment -> Maybe Point
meet s s' = 
    let 
        (h', v') = if y1 s == y2 s then (s,s') else (s',s)
        (h, v) = (order h', order v')
    in
        if x1 h <= x1 v && x2 h >= x2 v && y1 v <= y1 h && y2 v >= y2 h
        then Just (x1 v, y1 h)
        else Nothing


-- Sort a segment so that the second point is after the first in reading order
order s = if x1 s < x2 s || y1 s < y2 s then s else swap s

x1 = fst.fst
y1 = snd.fst
x2 = fst.snd
y2 = snd.snd

md :: Point -> Int
md (x,y) = abs x + abs y

signalDistance :: Wire -> Point -> Int
signalDistance w = sd (reverse w)
    where
        sd (s:ss) p@(px, py)
            | isJust (s `meet` (p, p)) = abs (x2 s - px) + abs (y2 s - py)
            | otherwise = abs (x2 s - x1 s) + abs (y2 s - y1 s) + sd ss p

dist :: Wire -> Wire -> Point -> Int
dist w1 w2 p = signalDistance w1 p + signalDistance w2 p

solA :: Input -> Output
solA (w1,w2) = 
    catMaybes [s1 `meet` s2 | s1 <- w1, s2 <- w2] \\ [(0,0)]
    & fmap md
    & minimum


solB :: Input -> Output
solB (w1,w2) =
    catMaybes [s1 `meet` s2 | s1 <- w1, s2 <- w2] \\ [(0,0)]
    & fmap (dist w1 w2)
    & minimum


--------------------------------------------------------------------------------
-- Scaffold
main :: IO ()
main = do
    inputRaw <- readFile "input"
    let input = parseInput inputRaw

    when (read (unsafePerformIO (getEnv "DO_A")) == 1) $ do
        putStrLn "PART A:"
        print $ solA input
        putStrLn ""

    when (read (unsafePerformIO (getEnv "DO_B")) == 1) $ do
        putStrLn "PART B:"
        print $ solB input
        putStrLn ""

