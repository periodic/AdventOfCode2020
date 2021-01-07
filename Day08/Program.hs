module Program where

import Data.Maybe (mapMaybe)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

data Instruction
  = IncAccum Int
  | Jump Int
  | NoOp Int
  deriving (Show, Eq)

newtype Program = Program
  { instructions :: Vector Instruction
  }
  deriving (Show)

makeProgram :: [Instruction] -> Program
makeProgram =
  Program . V.fromList

getInstruction :: Int -> Program -> Instruction
getInstruction index program =
  instructions program ! index

numInstructions :: Program -> Int
numInstructions =
  V.length . instructions

mutations :: Program -> [Program]
mutations program =
  mapMaybe mutateAt [1 .. (numInstructions program)]
  where
    mutateAt index =
      case getInstruction index program of
        NoOp val ->
          Just . Program $ instructions program // [(index, Jump val)]
        Jump val ->
          Just . Program $ instructions program // [(index, NoOp val)]
        IncAccum _ ->
          Nothing