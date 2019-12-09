{-# LANGUAGE RecordWildCards #-}

module Intcode where

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
import Control.Monad.RWS.Lazy -- from mtl
import Control.Monad.Loops (iterateUntil) -- from monad-loops

import Debug.Trace

--------------------------------------------------------------------------------
-- Data types

type PState = RWS () [Integer] Program
type Memory = Map Integer Integer

-- A program has an instruction pointer and a memory vector.
data Program = Program
    { machine :: !Machine
    , ip      :: !Integer
    , relP    :: !Integer
    , input   :: [Integer]
    , mem     :: !Memory
    }
instance Show Program where show p = show (input p, mem p)


makeProgram :: Memory -> Program
makeProgram = Program day7Machine 0 0 []
    
getMachine :: PState Machine
getMachine = gets machine

-- A machine describes the implemented operations.
type Machine   = Map Integer Operation

day7Machine :: Machine
day7Machine = Map.fromList 
    [ (1 , binop (+))
    , (2 , binop (*))
    , (3 , takeInput)
    , (4 , writeOutput)
    , (5 , jumpIf (/= 0))
    , (6 , jumpIf (== 0))
    , (7 , binop (\a b -> if a <  b then 1 else 0))
    , (8 , binop (\a b -> if a == b then 1 else 0))
    , (9 , adjustRelP)
    , (99, terminate)
    ]

-- An operation is a stateful modification of a program,
--  which optionally ends the program with a return code.
type Operation = PState (Maybe Integer)


--------------------------------------------------------------------------------
-- Intcode Utilities

-- Move the instruction pointer by some amount.
advancePointer :: Integer -> PState ()
advancePointer n = modify $ \p@Program{..} -> p { ip = ip + n }

-- Set the pointer to a specific value.
setPointer :: Integer -> PState ()
setPointer n = modify $ \p@Program{..} -> p { ip = n }

-- Move the instruction pointer by some amount.
advanceRelP :: Integer -> PState ()
advanceRelP n = modify $ \p@Program{..} -> p { relP = relP + n }

-- Get the memory value at some address.
getAt :: Integer -> PState Integer
getAt ix = gets mem <&> Map.findWithDefault 0 ix

-- Get the current value of the instruction pointer.
getIp :: PState Integer
getIp = gets ip

-- Get the current instruction to run.
getOpCode :: PState Integer
getOpCode = getIp >>= getAt <&> (`mod` 100)

-- Get a numbered parameter which may or may not be immediate.
getParam :: Integer -> PState Integer
getParam i = do
    modes <- (getIp >>= getAt)
            <&> ( show
                >>> reverse
                >>> drop 2
                >>> fmap (pure >>> read)
                )

    case listToMaybe $ drop (fromIntegral i - 1) modes of
        Just 1 -> getParamImmediate i
        Just 2 -> getParamRelative  i
        _      -> getParamPosition  i

getParamWrite :: Integer -> PState Integer
getParamWrite i = do
    modes <- (getIp >>= getAt)
        <&> ( show
            >>> reverse
            >>> drop 2
            >>> fmap (pure >>> read)
            )
    relP <- gets relP
    paramVal <- getParamImmediate i

    return $ case listToMaybe $ drop (fromIntegral i - 1) modes of
        Just 2 -> paramVal + relP
        _      -> paramVal

-- Set the memory value at some address (direct or indirect)
getParamImmediate :: Integer -> PState Integer
getParamImmediate i = getIp >>= ((+ i) >>> getAt)
getParamPosition  :: Integer -> PState Integer
getParamPosition  i = getIp >>= ((+ i) >>> getAt) >>= getAt
getParamRelative  :: Integer -> PState Integer
getParamRelative i = do 
    relP  <- gets relP
    param <- getParamImmediate i
    getAt $ param + relP

-- Set the memory value at some address.
setAt :: Integer -> Integer -> PState ()
setAt ptr val = modify $ \p@Program{..} -> p { mem = Map.insert ptr val mem }


--------------------------------------------------------------------------------
-- I/O

popInput :: PState Integer
popInput = do
    ii <- gets input
    case List.uncons ii of
        Nothing -> error "Empty input!"
        Just (i, is) -> do
            modify $ \p -> p { input = is }
            return i

pushOutput :: Integer -> PState ()
pushOutput = tell . pure


--------------------------------------------------------------------------------
-- Operations

-- A binary operation reads from the next two values,
-- applies an operation, stores the value in the third, 
-- and moves the instruction pointer forward four places.
binop :: (Integer -> Integer -> Integer) -> Operation
binop op = do
    x   <- getParam 1
    y   <- getParam 2
    loc <- getParamWrite 3
    setAt loc $ x `op` y
    advancePointer 4
    return Nothing

jumpIf :: (Integer -> Bool) -> Operation
jumpIf pred = do
    x <- getParam 1
    y <- getParam 2
    if pred x 
        then setPointer y
        else advancePointer 3
    return Nothing


-- The return value is the first value in memory.
terminate :: Operation
terminate = gets (Just . (Map.! 0) . mem)


takeInput :: Operation
takeInput = do
    i   <- popInput
    p   <- getParamWrite 1
    setAt p i
    advancePointer 2
    return Nothing


writeOutput :: Operation
writeOutput = do
    val <- getParam 1
    pushOutput val
    advancePointer 2
    return Nothing


-- Adjust the relative pointer
adjustRelP :: Operation
adjustRelP = do
    val <- getParam 1
    advanceRelP val
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
eval :: PState Integer
eval = fromJust <$> iterateUntil isJust step