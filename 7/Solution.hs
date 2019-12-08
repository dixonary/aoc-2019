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
import Control.Monad.RWS.Lazy
import Control.Monad.Loops (iterateUntil) -- from monad-loops

import Debug.Trace

--------------------------------------------------------------------------------
-- Data types

type Input  = Program -- The initial data structure to parse the data into.
type Output = Int     -- The final data structure to write the data out to.

type PState = RWS () [Int] Program

-- A program has an instruction pointer and a memory vector.
data Program = Program
    { machine :: Machine
    , ip      :: Int
    , input   :: [Int]
    , mem     :: Vector Int
    }
instance Show Program where show p = show (input p, mem p)

-- Read data into a program.
parseInput :: String -> Input
parseInput = makeProgram . Vector.fromList . fmap read . splitOn ","

makeProgram :: Vector Int -> Program
makeProgram = Program day5Machine 0 []
    
getMachine :: PState Machine
getMachine = gets machine

-- A machine describes the implemented operations.
type Machine   = Map Int Operation

day5Machine :: Machine
day5Machine = Map.fromList 
    [ (1 , binop (+))
    , (2 , binop (*))
    , (3 , takeInput)
    , (4 , writeOutput)
    , (5 , jumpIf (/= 0))
    , (6 , jumpIf (== 0))
    , (7 , binop (\a b -> if a <  b then 1 else 0))
    , (8 , binop (\a b -> if a == b then 1 else 0))
    , (99, terminate)
    ]

-- An operation is a stateful modification of a program,
--  which optionally ends the program with a return code.
type Operation = PState (Maybe Int)


--------------------------------------------------------------------------------
-- Intcode Utilities

-- Move the instruction pointer by some amount.
advancePointer :: Int -> PState ()
advancePointer n = modify $ \p@Program{..} -> p { ip = ip + n }

-- Set the pointer to a specific value.
setPointer :: Int -> PState ()
setPointer n = modify $ \p@Program{..} -> p { ip = n }

-- Get the memory value at some address.
getAt :: Int -> PState Int
getAt ix = gets mem <&> (! ix)

-- Get the current value of the instruction pointer.
getIp :: PState Int
getIp = gets ip

-- Get the current instruction to run.
getOpCode :: PState Int
getOpCode = getIp >>= getAt <&> (`mod` 100)

-- Get a numbered parameter which may or may not be immediate.
getParam :: Int -> PState Int
getParam i = do
    modes <- (getIp >>= getAt)
            <&> ( show
                >>> reverse
                >>> drop 2
                >>> fmap (pure >>> read)
                )

    case listToMaybe $ drop (i-1) modes of
        Just 1 -> getParamImmediate i
        _      -> getParamPosition  i

-- Set the memory value at some address (direct or indirect)
getParamImmediate :: Int -> PState Int
getParamImmediate i = getIp >>= ((+ i) >>> getAt)
getParamPosition  :: Int -> PState Int
getParamPosition  i = getIp >>= ((+ i) >>> getAt) >>= getAt

-- Set the memory value at some address.
setAt :: Int -> Int -> PState ()
setAt ptr val = modify $ \p@Program{..} -> p { mem = mem Vector.// [(ptr, val)] }


--------------------------------------------------------------------------------
-- I/O

popInput :: PState Int
popInput = do
    ii <- gets input
    case List.uncons ii of
        Nothing -> error "Empty input!"
        Just (i, is) -> do
            modify $ \p -> p { input = is }
            return i

pushOutput :: Int -> PState ()
pushOutput = tell . pure


--------------------------------------------------------------------------------
-- Operations

-- A binary operation reads from the next two values,
-- applies an operation, stores the value in the third, 
-- and moves the instruction pointer forward four places.
binop :: (Int -> Int -> Int) -> Operation
binop op = do
    x   <- getParam 1
    y   <- getParam 2
    loc <- getParamImmediate 3
    setAt loc $ x `op` y
    advancePointer 4
    return Nothing

jumpIf :: (Int -> Bool) -> Operation
jumpIf pred = do
    x <- getParam 1
    y <- getParam 2
    if pred x 
        then setPointer y
        else advancePointer 3
    return Nothing


-- The return value is the first value in memory.
terminate :: Operation
terminate = gets (Just . Vector.head . mem)


takeInput :: Operation
takeInput = do
    i   <- popInput
    p   <- getParamImmediate 1
    setAt p i
    advancePointer 2
    return Nothing


writeOutput :: Operation
writeOutput = do
    val <- getParam 1
    pushOutput val
    advancePointer 2
    return Nothing


--------------------------------------------------------------------------------
-- Evaluation


-- Finds the next operation according to the rules of some machine.
step :: Operation
step = do
    machine     <- getMachine
    opcode      <- getOpCode
    fromMaybe (error $ "Invalid opcode: " ++ show opcode) 
        $ machine !? opcode

-- Run program until a return code (terminate) is hit.
eval :: PState Int
eval = fromJust <$> iterateUntil isJust step


--------------------------------------------------------------------------------
-- Solution

solA :: Input -> Output
solA program = List.maximum $ runPattern program <$> List.permutations [0..4]

runPattern :: Program -> [Int] -> Output
runPattern program [a,b,c,d,e] = let
        amp x v = last o
            where  (ret, p, o) = runRWS eval () program{ input = [x,v] }
    in amp a 0 & amp b & amp c & amp d & amp e

solB :: Input -> Output
solB program = List.maximum $ runPatternFeedback program <$> List.permutations [5..9]

runPatternFeedback :: Program -> [Int] -> Output
runPatternFeedback program [a,b,c,d,e] = let
        amp x vs = o 
            where (ret, p, o) = runRWS eval () program{ input = x:vs }

        resA = amp a (0:resE)
        resB = amp b resA
        resC = amp c resB
        resD = amp d resC
        resE = amp e resD
    in last resE



--------------------------------------------------------------------------------
-- Scaffold
main :: IO ()
main = do
    inputRaw <- readFile "input"
    let input = parseInput inputRaw

    when (read (unsafePerformIO (getEnv "DO_A")) == 1) $ do
        putStrLn "PART A:"
        solA input & print
        putStrLn "DONE"
        putStrLn ""

    when (read (unsafePerformIO (getEnv "DO_B")) == 1) $ do
        putStrLn "PART B:"
        solB input & print
        putStrLn ""

