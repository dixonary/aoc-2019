{-# LANGUAGE RecordWildCards #-}

-- Control flow
import Control.Monad
import Data.Function ((&), on)
import Data.Functor ((<&>))
import System.Environment
import System.IO.Unsafe

-- Data Structures
import qualified Data.List as List
import Data.Map (Map, (!?))
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
import Control.Monad.State
import Control.Monad.Loops (iterateUntil) -- from monad-loops

--------------------------------------------------------------------------------
-- Data types

type Input  = Program -- The initial data structure to parse the data into.
type Output = Int     -- The final data structure to write the data out to.


-- A program has an instruction pointer and a memory vector.
data Program = Program
    { ip :: Int
    , mem :: Vector Int
    }

-- Read data into a program.
parseInput :: String -> Input
parseInput = makeProgram . Vector.fromList . fmap read . splitOn ","

makeProgram :: Vector Int -> Program
makeProgram = Program 0
    

-- A machine describes the implemented operations.
type Machine   = Map Int Operation

day2Machine :: Machine
day2Machine = Map.fromList 
    [ (1, binop (+))
    , (2, binop (*))
    , (99, terminate)
    ]


-- An operation is a stateful modification of a program,
--  which optionally ends the program with a return code.
type Operation = State Program (Maybe Int)

-- Move the instruction pointer by some amount.
advancePointer :: Int -> State Program ()
advancePointer n = modify $ \p@Program{..} -> p { ip = ip + n }

-- Get the memory value at some address.
getAt :: Int -> State Program Int
getAt ix = gets $ (! ix) . mem

-- Get the memory value pointed to by a pointer stored at some address.
getAtP :: Int -> State Program Int
getAtP = getAt >=> getAt

-- Set the memory value at some address.
setAt :: Int -> Int -> State Program ()
setAt ptr val = modify $ \p@Program{..} -> p { mem = mem Vector.// [(ptr, val)] }

-- Get the current value of the instruction pointer.
getIp :: State Program Int
getIp = gets ip

-- Get the current instruction to run.
getInstruction :: State Program Int
getInstruction = gets ip >>= getAt

-- A binary operation reads from the next two values,
-- applies an operation, stores the value in the third, 
-- and moves the instruction pointer forward four places.
binop :: (Int -> Int -> Int) -> Operation
binop op = do
    ptr <- getIp
    x   <- getAtP $ ptr + 1
    y   <- getAtP $ ptr + 2
    loc <- getAt  $ ptr + 3
    setAt loc $ x `op` y
    advancePointer 4
    return Nothing

-- Applies the next operation according to the rules of some machine.
step :: Machine -> Operation
step machine = do
    instruction <- getInstruction
    fromMaybe (error $ "Invalid opcode: " ++ show instruction) 
        $ machine !? instruction

-- The return value is the first value in memory.
terminate :: Operation
terminate = do
    headval <- gets (Vector.head . mem)
    return $ Just headval

-- Set the noun and verb and then evaluate.
replaceRun :: Int -> Int -> State Program Int
replaceRun noun verb = do
    setAt 1 noun 
    setAt 2 verb 
    eval

-- Run program until a return code (terminate) is hit.
eval :: State Program Int
eval = fromJust <$> iterateUntil isJust (step day2Machine)

--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA = evalState (replaceRun 12 02)

solB :: Input -> Output
solB vec = head
    [ 100 * n + v 
    | n <- [0..100]
    , v <- [0..100]
    , evalState (replaceRun n v) vec == 19690720
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
        print $ solB input
        putStrLn ""

