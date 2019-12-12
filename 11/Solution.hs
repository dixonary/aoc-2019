{-# LANGUAGE LambdaCase, RecordWildCards #-}

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

import Data.Bifunctor (second)

import Control.Monad.Writer -- from mtl
import Control.Monad.RWS -- from mtl

import Data.List.Split -- from split
import Intcode -- shared

import Debug.Trace

--------------------------------------------------------------------------------
-- IO Wrangling

-- The initial data structure to parse the data into.
type Input = Program
-- The final data structure to write the data out to.
type Output = Int

parseInput :: String -> Input
parseInput = makeProgram . Map.fromList . zip [0..] . fmap read . splitOn ","



--------------------------------------------------------------------------------
-- Solution

data Color = White | Black
type Hull  = Map Integer (Map Integer Color)
type Position = (Integer, Integer)
data Direction = N | E | S | W

updateHull :: Position -> Color -> Hull -> Hull
updateHull (x,y) col = Map.insertWith Map.union y $ Map.fromList [(x, col)]

lookupHull :: Position -> Hull -> Color
lookupHull (x,y) hull = fromMaybe Black $ Map.lookup y hull >>= Map.lookup x

left :: Direction -> Direction
left N = W
left E = N
left S = E
left W = S

right :: Direction -> Direction
right N = E
right E = S
right S = W
right W = N

move :: Direction -> Position -> Position
move N (x,y) = (x, y-1)
move E (x,y) = (x+1, y)
move S (x,y) = (x, y+1)
move W (x,y) = (x-1, y)


data RobotState = RobotState
    { brain :: Program
    , position :: (Integer, Integer)
    , direction :: Direction
    , commands :: [Integer]
    , hull :: Hull
    } 


robot :: RWS () [Position] RobotState ()
robot = do
    RobotState{..} <- get

    let currentSquare = lookupHull position hull & \case 
            White -> 1
            Black -> 0

        -- Run one computer step, setting the input to the value of the current
        -- square (in case the computer wants to read it)
        (ret, brain', out) = runRWS step () brain{ input = [currentSquare] }

    case commands ++ out of 
        [paint, turn] -> do
            let
                color  = case paint of
                    0 -> Black
                    1 -> White

                hull'  = updateHull position color hull

                direction' = direction & case turn of
                    0 -> left
                    1 -> right

                -- Move one step
                position' = move direction' position

            -- Record the painted position
            tell $ pure position

            -- Update all the robot data
            modify $ \r -> r 
                { direction = direction'
                , position = position'
                , brain = brain'
                , hull = hull'
                , commands = []
                }
            robot

        -- We are not ready to execute a step just yet
        _ -> case ret of
            -- The brain is not finished - repeat with new brainstate
            Nothing -> do
                modify $ \r -> r { brain = brain', commands = commands ++ out }
                robot
            -- The brain is finished
            Just _  -> return ()


solA :: Input -> Output
solA input = let
    initialState = RobotState
        { brain = input
        , position = (0,0)
        , direction = N
        , commands = []
        , hull = mempty
        }
    ((), finalState, paintedSpots) = runRWS robot () initialState
    in length $ List.nub paintedSpots


solB :: Input -> String
solB input = let
    initialState = RobotState
        { brain = input
        , position = (0,0)
        , direction = N
        , commands = []
        , hull = updateHull (0,0) White mempty
        }
    ((), RobotState{..}, paintedSpots) = runRWS robot () initialState

    valToChar = \case
        White -> '█'
        Black -> ' '

    in print2DMap valToChar hull 



print2DMap :: (Ord j, Ord k, Enum j, Enum k) 
           => (a -> Char) -> Map j (Map k a) -> String
print2DMap f map = let
    minY = minimum $ Map.keys map
    maxY = maximum $ Map.keys map
    minX = minimum $ minimum . Map.keys <$> map
    maxX = maximum $ maximum . Map.keys <$> map
 
    in unlines 
        [
            [ case Map.lookup y map >>= Map.lookup x of
                Nothing -> ' '
                Just v  -> f v
            | x <- [minX .. maxX]
            ]
        | y <- [minY .. maxY]
        ]



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
        putStrLn $ solB input
        putStrLn ""

