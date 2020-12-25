module Main where

import qualified Data.List as L
import Text.Printf

newtype PublicKeyPair = PublicKeyPair (Key, Key) deriving Show

ringSize :: Int
ringSize =
  20201227
newtype Key = Key Int deriving (Eq, Ord)

instance Show Key where
  show (Key a) = show a

instance PrintfArg Key where
  formatArg (Key a) =
    formatArg a

instance Num Key where
  (Key a) + (Key b) =
    Key $ (a + b) `mod` ringSize
  (Key a) * (Key b) =
    Key $ (a * b) `mod` ringSize
  abs (Key a) =
    Key a
  negate (Key a) =
    Key $ negate a `mod` ringSize
  signum (Key a) =
    Key $ signum a
  fromInteger = 
    Key . (`mod` ringSize) . fromInteger


transform :: Key -> Key -> Key
transform subject k =
  subject * k

loop :: Int -> Key -> Key
loop n subject =
  (!! n) . iterate (transform subject) $ 1

findLoopSize :: Key -> Maybe Int
findLoopSize k =
  fmap fst . L.find ((== k) . snd) . zip [0..] . iterate (transform 7) $ 1

partOne :: IO ()
partOne =
  let
    (PublicKeyPair (k1, k2)) = inputKeys
    maybeLoopSize1 = findLoopSize k1
    maybeLoopSize2 = findLoopSize k2
  in
  case (maybeLoopSize1, maybeLoopSize2) of
    (Just n1, Just n2) -> do
      printf "Loop size of %d: %d\n" k1 n1
      printf "Loop size of %d: %d\n" k2 n2
      printf "Encryption key: %d\n" (loop n1 k2)
    _ ->
      putStrLn "Could not calculate the encryption key."
    

exampleKeys :: PublicKeyPair
exampleKeys =
  PublicKeyPair (17807724, 5764801)

inputKeys :: PublicKeyPair
inputKeys =
  PublicKeyPair (10705932, 12301431)

main :: IO ()
main =
  partOne