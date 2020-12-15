module Eval where

import           Data.Bits
import           Data.List as L
import           Data.Map  as M
import           Types

newtype Mask = Mask [MaskValue]

instance Show Mask where
  show (Mask values) = L.concatMap show values

data EvalState = EvalState {mask :: Mask, memory :: Map MemLocation Int}
  deriving (Show)

-- For this question, all numbers are 36-bit
oneBits = 2 ^ 37 - 1

initialState = EvalState (Mask []) M.empty

makeInt :: [Bool] -> Int
makeInt =
  L.foldl (setBit) zeroBits . L.map fst . L.filter snd . zip [0 ..] . reverse

makeMask :: [MaskValue] -> Mask
makeMask = Mask

sumMemory :: Map MemLocation Int -> Int
sumMemory = M.foldr (+) 0

applyValueMask :: Mask -> Int -> Int
applyValueMask (Mask maskValues) value =
  L.foldr (uncurry applyBit) value . zip [0 ..] . L.reverse $ maskValues
  where
    applyBit _ MaskUnset value    = value
    applyBit index MaskOne value  = setBit value index
    applyBit index MaskZero value = clearBit value index

evalV1 :: Instruction -> EvalState -> EvalState
evalV1 inst@(SetMask maskValue) (EvalState mask memory) =
  EvalState (makeMask maskValue) memory
evalV1 inst@(Store location value) (EvalState mask memory) =
  let maskedValue = applyValueMask mask value
      newMemory = M.insert location maskedValue memory
   in EvalState mask newMemory

evalProgramV1 :: Program -> Map MemLocation Int
evalProgramV1 = memory . L.foldl (flip evalV1) initialState . instructions

applyLocationMask :: Mask -> MemLocation -> [MemLocation]
applyLocationMask (Mask maskValues) memLocation =
  L.foldr (\bit prev -> prev >>= uncurry applyBit bit) [memLocation]
    . zip [0 ..]
    . L.reverse
    $ maskValues
  where
    applyBit _ MaskZero memLocation = [memLocation]
    applyBit index MaskOne (MemLocation value) =
      [MemLocation $ value `setBit` index]
    applyBit index MaskUnset (MemLocation value) =
      [ MemLocation $ value `setBit` index,
        MemLocation $ value `clearBit` index
      ]

evalV2 :: Instruction -> EvalState -> EvalState
evalV2 inst@(SetMask maskValue) (EvalState mask memory) =
  EvalState (makeMask maskValue) memory
evalV2 inst@(Store location value) (EvalState mask memory) =
  let maskedLocations = applyLocationMask mask location
      newMemory = L.foldr (`M.insert` value) memory maskedLocations
   in EvalState mask newMemory

evalProgramV2 :: Program -> Map MemLocation Int
evalProgramV2 = memory . L.foldl (flip evalV2) initialState . instructions
