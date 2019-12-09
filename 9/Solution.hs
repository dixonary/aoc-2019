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

import Data.List.Split (splitOn) -- from split
import Control.Monad.RWS -- from mtl

import Intcode -- shared


--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = Program
-- The final data structure to write the data out to.
type Output = [Integer]

parseInput :: String -> Input
parseInput = makeProgram . Map.fromList . zip [0..] . fmap read . splitOn ","


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> IO Output
solA program = do
    vals <- fmap read . lines <$> getContents
    let p' = program { input = vals }
    let (ret, pfinal, output) = runRWS eval () p'
    return $ output ++ [ret]

solB :: Input -> IO Output
solB = solA


--------------------------------------------------------------------------------
-- Scaffold
main :: IO ()
main = do
    inputRaw <- readFile "input"
    let input = parseInput inputRaw

    when (read (unsafePerformIO (getEnv "DO_A")) == 1) $ do
        putStrLn "PART A:"
        print =<< solA input
        putStrLn ""

    when (read (unsafePerformIO (getEnv "DO_B")) == 1) $ do
        putStrLn "PART B:"
        print =<< solB input
        putStrLn ""

