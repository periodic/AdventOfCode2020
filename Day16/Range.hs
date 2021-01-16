module Range where

import qualified Data.List as L

data Range
  = Range Int Int
  | RangeUnion [Range]
  | EmptyRange
  deriving (Show)

addRanges :: Range -> Range -> Range
addRanges a b =
  RangeUnion [a, b]

instance Semigroup Range where
  (<>) = addRanges

instance Monoid Range where
  mempty = EmptyRange

inRange :: Int -> Range -> Bool
inRange x (Range lower upper) =
  x >= lower && x <= upper
inRange x (RangeUnion ranges) =
  any (inRange x) ranges
inRange x EmptyRange =
  False

simplifyRange :: Range -> Range
simplifyRange EmptyRange = EmptyRange
simplifyRange range@(Range _ _) = range
simplifyRange range =
  buildRange . mergeIntervals . L.sort . getIntervals $ range
  where
    getIntervals (Range a b) = [(a, b)]
    getIntervals EmptyRange = []
    getIntervals (RangeUnion ranges) = L.concatMap getIntervals ranges
    
    mergeIntervals ((l1, r1) : (l2, r2) : rest) =
      if l2 <= r1
        then mergeIntervals $ (l1, r2) : rest
        else (l1, r1) : mergeIntervals ((l2, r2) : rest)
    mergeIntervals rest =
      rest
    
    buildRange =
      RangeUnion . map (uncurry Range)