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


--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = [Int]
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput inputRaw = inputRaw & lines & fmap read


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA = sum . fmap calcFuel

solB :: Input -> Output
solB = sum . fmap cfR

calcFuel :: Int -> Int
calcFuel = subtract 2 . flip div 3

cfR :: Int -> Int
cfR = sum . tail . takeWhile (>0) . iterate calcFuel


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

