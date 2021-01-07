{-# LANGUAGE GeneralisedNewtypeDeriving, TypeFamilies, RankNTypes #-}
module Evaluator where

import Prelude hiding (read)
import Control.Monad.State as S
import Control.Monad.ST
import Data.IntSet as S
import Data.Vector.Mutable as V

import Program

data ProgramState = ProgramState
    { program :: Program
    , visitedInstructions :: IntSet
    , instructionCounter :: Int
    , accumulator :: Int
    }
    deriving (Show)

newtype Eval a = Eval {
    runEval :: State ProgramState a
} deriving (Functor, Applicative, Monad, MonadState ProgramState)

data Termination
    = Success ProgramState
    | Loop ProgramState

runProgram :: Eval Termination -> Program -> Termination
runProgram strategy program =
    flip evalState initState . runEval $ strategy
    where
        initState = 
            ProgramState
                { program = program
                , visitedInstructions = S.empty
                , instructionCounter = 0
                , accumulator = 0
                }


modifyIc :: (Int -> Int) -> Eval ()
modifyIc f =
    S.modify $ \programState ->
        programState { instructionCounter = f (instructionCounter programState)}

modifyAccumulator :: (Int -> Int) -> Eval ()
modifyAccumulator f =
    S.modify $ \programState ->
        programState { accumulator = f (accumulator programState)}

getInstructionCounter :: Eval Int
getInstructionCounter =
    gets instructionCounter

getWasVisited :: Eval Bool
getWasVisited = do
    ic <- getInstructionCounter
    gets $ S.member ic . visitedInstructions

setWasVisited :: Eval ()
setWasVisited = do
    ic <- getInstructionCounter
    S.modify $ \state ->
        state {
            visitedInstructions = S.insert ic (visitedInstructions state)
        }

endOfInput :: Eval Bool
endOfInput = do
    ic <- getInstructionCounter
    prog <- gets program
    return $ ic >= numInstructions prog

evalCurrInstruction :: Eval ()
evalCurrInstruction = do
    ic <- getInstructionCounter
    instruction <- gets (getInstruction ic . program)
    setWasVisited
    case instruction of
        NoOp _ ->
            modifyIc (+ 1)
        IncAccum val -> do
            modifyAccumulator (+ val)
            modifyIc (+ 1)
        Jump offset ->
            modifyIc (+ offset) 

untilLoop :: Eval Termination
untilLoop = do
    isEnd <- endOfInput
    visited <- getWasVisited
    if isEnd
        then gets Success 
        else 
            if visited
                then gets Loop
                else do
                    evalCurrInstruction 
                    untilLoop 