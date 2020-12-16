module Range where

data Range
  = Range Int Int
  | RangeUnion Range Range
  | EmptyRange
  deriving (Show)


addRanges :: Range -> Range -> Range
addRanges = RangeUnion

instance Semigroup Range where
  (<>) = addRanges

instance Monoid Range where
  mempty = EmptyRange

inRange :: Int -> Range -> Bool
inRange x (Range lower upper) =
  x >= lower && x <= upper
inRange x (RangeUnion left right) =
  x `inRange` left || x `inRange` right
inRange x EmptyRange =
  False
