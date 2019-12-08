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

import Data.List.Split (chunksOf) -- from split
import Data.Ord (comparing)

import Codec.Picture -- from JuicyPixels


--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = [[[Int]]]
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput = fmap (chunksOf 25) . chunksOf (25 * 6) . fmap (read . pure) . init


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA input = let
        countOf :: Int -> [[Int]] -> Int
        countOf x = sum . fmap (length . filter (== x))

    in List.minimumBy (comparing (countOf 0)) input
            & \l -> countOf 1 l * countOf 2 l


solB :: Input -> DynamicImage
solB input = ImageRGB8 $ generateImage (getPixelStack input) 25 6

getPixelStack :: [[[Int]]] -> Int -> Int -> PixelRGB8
getPixelStack (top:rest) x y = 
    case top !! y !! x of
        0 -> PixelRGB8 0 0 0
        1 -> PixelRGB8 255 255 255
        2 -> getPixelStack rest x y


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
        savePngImage "output.png" $ solB input
        putStrLn "...written to file."
        putStrLn ""

