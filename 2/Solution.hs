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

import Data.Vector (Vector, (!)) -- from vector
import qualified Data.Vector as Vector -- from vector
import qualified Data.Vector.Mutable as MV -- from vector
import Data.List.Split -- from split

import Control.Arrow ((>>>))

import Debug.Trace

--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = Vector Int
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput = Vector.fromList . fmap read . splitOn ","


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA = replaceRun 12 02

solB :: Input -> Output
solB vec = head
    [ 100 * n + v 
    | n <- [0..100]
    , v <- [0..100]
    , replaceRun n v vec == 19690720
    ]


replaceRun :: Int -> Int -> Vector Int -> Int
replaceRun noun verb 
    =   1 #= noun 
    >>> 2 #= verb 
    >>> eval 0 
    >>> Vector.head


-- Dereference pointer
infixl 5 #!
v #! i = v ! (v ! i)

-- Modify at location
infixl 5 #=
i #= n = Vector.modify (\v -> MV.write v i n)

-- Run program
eval :: Int -> Vector Int -> Vector Int
eval ctr vec = do
    let op (%) = 
            let
                in1 = vec #! (ctr + 1)
                in2 = vec #! (ctr + 2)
                out = vec  ! (ctr + 3)
            in out #= in1 % in2

    case vec ! ctr of
        1  -> vec & op (+) & eval (ctr+4)
        2  -> vec & op (*) & eval (ctr+4)
        99 -> vec



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

