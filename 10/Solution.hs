-- Control flow
import Control.Monad
import Data.Function ((&), on)
import Data.Functor ((<&>))
import System.Environment
import System.IO.Unsafe

-- Data Structures
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Bool

import Data.Ord
import Data.Ratio

--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = [Position]
-- The final data structure to write the data out to.
type Output = Integer

parseInput :: String -> Input
parseInput inputRaw = let
        input :: [(Integer, [(Integer, Char)])]
        input = zip [0..] $ zip [0..] <$> lines inputRaw

        in flip concatMap input
            $ \(y, ls) -> catMaybes $ flip fmap ls
                $ \(x, v) -> case v of
                    '#' -> Just (x,y)
                    _   -> Nothing

type Position = (Integer, Integer)

--------------------------------------------------------------------------------
-- Solution

data RelPos = RelPos Integer Integer

instance Eq RelPos  where (==) = (==) `on` at
instance Ord RelPos where compare = compare `on` at

-- Get the arctangent of a relative position.
-- In range [-pi*3/2, pi/2]
at :: RealFloat a => RelPos -> a
at (RelPos dx dy) = 
    let 
        x = (atan2 `on` fromIntegral) dy dx
    in 
        if x >= pi / 2 then x - pi*2 else x

-- Distance metric for relative positions.
dm :: RelPos -> Integer
dm (RelPos dx dy) = dx * dx + dy * dy

-- Number of visible locations from the given location.
numVis :: [Position] -> Position -> Integer
numVis positions a
    = List.delete a positions
    & fmap (rel a)
    & List.sort
    & List.group
    & length
    & fromIntegral


solA :: Input -> Output
solA positions = List.maximum $ numVis positions <$> positions

rel :: Position -> Position -> RelPos
rel (xa,ya) (xb,yb) = RelPos (xb - xa) (yb - ya)

-- Get the full order of destroyed positions based on given structure.
blast :: [[(Position, RelPos)]] -> [(Position, RelPos)]
blast asteroids = let
    unconsings = List.uncons <$> asteroids
    blasted    = catMaybes $ fmap fst <$> unconsings 
    in if null blasted 
        then []
        else blasted ++ blast (maybe [] snd <$> unconsings)

        
solB :: Input -> Integer
solB positions = let
        base = List.maximumBy (comparing (numVis positions)) positions

        withRel p = (p, p `rel` base)

        allBySweep = positions
                   & fmap withRel
                   & List.sortOn snd
                   & List.groupBy ((==) `on` snd)
                   & fmap (List.sortOn (dm . snd)) 

        ((x,y),_) = blast allBySweep !! 199

    in x * 100 + y


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

