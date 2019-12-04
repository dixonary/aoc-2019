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

import Data.List.Split (splitOn)

--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = (Int, Int)
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput inputRaw = let [l,h] = read <$> splitOn "-" inputRaw in (l,h)


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA (l,h) = length $ combos l h

combos :: Int -> Int -> [Int]
combos l h = 
    [ x
    | a <- [1..9]
    , b <- [a..9]
    , c <- [b..9]
    , d <- [c..9]
    , e <- [d..9]
    , f <- [e..9]
    , a==b || b==c || c==d || d==e || e==f
    , let x = 100000*a + 10000*b + 1000*c + 100*d + 10*e + f
    , x >= l
    , x <= h
    ]


combos' :: Int -> Int -> [Int]
combos' l h = 
    [ x
    | a <- [1..9]
    , b <- [a..9]
    , c <- [b..9]
    , d <- [c..9]
    , e <- [d..9]
    , f <- [e..9]
    ,      (a==b&& b /= c) 
        || (a /= b && b==c && c /= d)
        || (b /= c && c==d && d /= e)
        || (c /= d && d==e && e /= f)
        || (d /= e && e==f)
    , let x = 100000*a + 10000*b + 1000*c + 100*d + 10*e + f
    , x >= l
    , x <= h
    ]

solB :: Input -> Output
solB (l,h) = length $ combos' l h


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

