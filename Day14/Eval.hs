module Eval where

import Data.Bits
import Data.Function
import Data.List as L
import Data.IntMap as M
import Types

newtype Mask = Mask (Int, Int)

instance Show Mask where
  show (Mask values) = L.concatMap show values

data EvalState = EvalState {mask :: !Mask, memory :: !(IntMap Int)}
  deriving (Show)

-- For this question, all numbers are 36-bit
-- oneBits = 2 ^ 37 - 1

initialState = EvalState (Mask (0, 0)) M.empty

makeInt :: [Bool] -> Int
makeInt =
  L.foldl (setBit) zeroBits . L.map fst . L.filter snd . zip [0 ..] . reverse

makeMask :: [MaskValue] -> Mask
makeMask maskValues =
  let indexedValues = zip [0 ..] . L.reverse $ maskValues
      ones = L.foldl (\v -> setBit v . fst) 0 . L.filter ((== MaskOne) . snd) $ indexedValues
      zeroes = L.foldl (\v -> setBit v . fst) 0 . L.filter ((== MaskZero) . snd) $ indexedValues
   in Mask (ones, zeroes)

sumMemory :: IntMap Int -> Int
sumMemory = M.foldr (+) 0

memorySize :: IntMap Int -> Int
memorySize = M.size

applyValueMask :: Mask -> Int -> Int
applyValueMask (Mask (ones, zeroes)) value =
  (value .|. ones) .&. complement zeroes

evalV1 :: Instruction -> EvalState -> EvalState
evalV1 inst@(SetMask maskValue) (EvalState mask memory) =
  EvalState (makeMask maskValue) memory
evalV1 inst@(Store (MemLocation location) value) (EvalState mask memory) =
  let maskedValue = applyValueMask mask value
      newMemory = M.insert location maskedValue memory
   in EvalState mask newMemory

evalProgramV1 :: Program -> IntMap Int
evalProgramV1 = memory . L.foldl (flip evalV1) initialState . instructions

applyLocationMask :: Mask -> Int -> [Int]
applyLocationMask (Mask (ones, zeroes)) addr =
  let newAddr = addr .|. ones
   in L.foldr (\bit prev -> prev >>= expand bit) [newAddr] [0 .. 35]
  where
    floating = complement $ ones .|. zeroes
    expand index loc =
      if floating `testBit` index
        then
          [ loc `setBit` index,
            loc `clearBit` index
          ]
        else [loc]

evalV2 :: Instruction -> EvalState -> EvalState
evalV2 inst@(SetMask maskValue) (EvalState mask memory) =
  EvalState (makeMask maskValue) memory
evalV2 inst@(Store (MemLocation location) value) (EvalState mask memory) =
  let maskedLocations = applyLocationMask mask location
      newMemory = L.foldr (`M.insert` value) memory maskedLocations
   in EvalState mask newMemory

evalProgramV2 :: Program -> IntMap Int
evalProgramV2 = memory . L.foldl (flip evalV2) initialState . instructions
