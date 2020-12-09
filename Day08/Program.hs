module Program where

import Data.Vector as V

data Instruction
    = IncAccum Int
    | Jump Int
    | NoOp Int
    deriving (Show, Eq)

newtype Program = Program {
    instructions :: Vector Instruction
} deriving (Show)

makeProgram :: [Instruction] -> Program
makeProgram =
    Program . fromList

getInstruction :: Int -> Program -> Instruction
getInstruction index program =
    instructions program ! index

numInstructions :: Program -> Int
numInstructions =
    V.length . instructions

mutations :: Program -> [Program]
mutations program = 
    V.toList . V.map mutateAt . indexed . instructions $ program
    where
        mutateAt (index, _) =
            case getInstruction index program of
                NoOp val ->
                    Program $ instructions program // [(index, Jump val)]
                Jump val ->
                    Program $ instructions program // [(index, NoOp val)]
                IncAccum _ ->
                    program