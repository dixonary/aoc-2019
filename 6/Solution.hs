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

import Data.Tree (Tree) -- from containers
import qualified Data.Tree as Tree -- from contianers

import Data.List.Split (splitOn) -- from split

--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = Tree String
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput inputRaw = let
    pairs = (\[x,y] -> (x,y)) . splitOn ")" <$> lines inputRaw
    findChildren o = (o, snd <$> filter (\x -> fst x == o) pairs)
    in Tree.unfoldTree findChildren "COM"

--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA tree = sum $ zipWith (*) [0..] (length <$> Tree.levels tree)

pathTo :: Eq a => a -> Tree a -> Maybe [a]
pathTo x (Tree.Node label ts) = let
    ts' = catMaybes $ pathTo x <$> ts
    in if label == x 
        then Just []
        else case ts' of
        [  ] -> Nothing
        [ls] -> Just (label : ls)

solB :: Input -> Output
solB tree = let
        Just pYou = pathTo "YOU" tree
        Just pSan = pathTo "SAN" tree 

        sp (x:xs) (y:ys) | x == y = [x] : sp xs ys
                         | otherwise = []

    in length pYou + length pSan - (length (sp pYou pSan) * 2) 


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

