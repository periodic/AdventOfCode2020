module Types where

import Data.Text

data Bag = Bag
    { modifier :: Text
    , color :: Text
    } deriving (Show, Eq, Ord)

data BagRule = BagRule
    { container :: Bag
    , contents :: [(Int, Bag)]
    } deriving (Show)
