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

import Debug.Trace

--------------------------------------------------------------------------------
-- Data types

type Input  = Program -- The initial data structure to parse the data into.
type Output = [Int]     -- The final data structure to write the data out to.


-- A program has an instruction pointer and a memory vector.
data Program = Program
    { machine :: Machine
    , ip      :: Int
    , input   :: [Int]
    , output  :: [Int]
    , mem     :: Vector Int
    }
instance Show Program where show = show . mem

-- Read data into a program.
parseInput :: String -> Input
parseInput = makeProgram . Vector.fromList . fmap read . splitOn ","

makeProgram :: Vector Int -> Program
makeProgram = Program day5Machine 0 [] []
    
getMachine :: State Program Machine
getMachine = gets machine

-- A machine describes the implemented operations.
type Machine   = Map Int Operation

day5Machine :: Machine
day5Machine = Map.fromList 
    [ (1 , binop (+))
    , (2 , binop (*))
    , (3 , takeInput)
    , (4 , writeOutput)
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
getAt ix = do
    m <- gets mem 
    case m Vector.!? ix of
        Just v -> return v
        Nothing -> do
            ip <- getIp
            error $ show ix 
                ++ " is out of range!\n"
                ++ "Thrown from instruction at position " 
                ++ show ip

-- Get the memory value pointed to by a pointer stored at some address.
getAtP :: Int -> State Program Int
getAtP = getAt >=> getAt

-- Set the memory value at some address.
setAt :: Int -> Int -> State Program ()
setAt ptr val = modify $ \p@Program{..} -> p { mem = mem Vector.// [(ptr, val)] }

-- Get the current value of the instruction pointer.
getIp :: State Program Int
getIp = gets ip

data Mode     = Position | Immediate
    deriving (Eq, Show)

-- Get the mode for a parameter.
getMode :: Int -> State Program Mode
getMode i = do
    modes <- (getIp >>= getAt)
            <&> ( show
                >>> reverse
                >>> drop 2
                >>> fmap (pure >>> read)
                )

    case listToMaybe $ drop (i-1) modes of
        Just 1 -> return Immediate
        _      -> return Position


-- Get a numbered parameter.
getParam :: Int -> State Program Int
getParam i = do
    ptr    <- getIp
    mode   <- getMode i
    case mode of
        Immediate -> getAt  $ ptr + i
        Position  -> getAtP $ ptr + i


-- A binary operation reads from the next two values,
-- applies an operation, stores the value in the third, 
-- and moves the instruction pointer forward four places.
binop :: (Int -> Int -> Int) -> Operation
binop op = do
    ptr <- getIp
    x   <- getParam 1
    y   <- getParam 2
    loc <- getAt $ ptr + 3
    setAt loc $ x `op` y
    advancePointer 4
    return Nothing


-- The return value is the first value in memory.
terminate :: Operation
terminate = gets (Just . Vector.head . mem)


takeInput :: Operation
takeInput = do
    i   <- popInput
    ptr <- getIp
    p   <- getAt $ ptr + 1
    setAt p i
    advancePointer 2
    return Nothing


writeOutput :: Operation
writeOutput = do
    ptr <- getIp
    val <- getAt $ ptr + 1
    
    pushOutput val

    advancePointer 2
    return Nothing


-- Get the current instruction to run.
getOpCode :: State Program Int
getOpCode = do
    ptr  <- gets ip 
    ival <- getAt ptr 
    return $ ival `mod` 100


-- Finds the next operation according to the rules of some machine.
step :: Operation
step = do
    machine     <- getMachine
    opcode      <- getOpCode
    fromMaybe (error $ "Invalid opcode: " ++ show opcode) 
        $ machine !? opcode


-- Set the noun and verb and then evaluate.
replaceRun :: Int -> Int -> State Program Int
replaceRun noun verb = do
    setAt 1 noun 
    setAt 2 verb 
    eval

-- Run program until a return code (terminate) is hit.
eval :: State Program Int
eval = fromJust <$> iterateUntil isJust step

popInput :: State Program Int
popInput = do
    ii <- gets input
    case List.uncons ii of
        Nothing -> error "Empty input!"
        Just (i, is) -> do
            modify $ \p -> p { input = is }
            return i

pushOutput :: Int -> State Program ()
pushOutput i = do
    o <- gets output
    modify $ \p -> p { output = o ++ [i] }

--------------------------------------------------------------------------------
-- Solution

solA :: Input -> IO Output
solA program = do
    vals <- fmap read . lines <$> getContents
    let p' = program { input = vals }
    let (ret, pfinal) = runState eval p'
    return $ output pfinal ++ [ret]

solB :: Input -> IO Output
solB vec = undefined


--------------------------------------------------------------------------------
-- Scaffold
main :: IO ()
main = do
    inputRaw <- readFile "input"
    let input = parseInput inputRaw

    when (read (unsafePerformIO (getEnv "DO_A")) == 1) $ do
        putStrLn "PART A:"
        solA input >>= print
        putStrLn "DONE"
        putStrLn ""

    when (read (unsafePerformIO (getEnv "DO_B")) == 1) $ do
        putStrLn "PART B:"
        solB input >>= print
        putStrLn ""

