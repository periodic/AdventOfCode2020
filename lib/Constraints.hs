module Constraints where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S

solve :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
solve options =
  let (key, values) = M.findMin options
      remainingOptions = M.delete key options
   in if M.null options
        then Just M.empty
        else getFirst . mconcat . map First $ do
          value <- S.toList values
          let filteredOptions = M.map (S.delete value) remainingOptions
          let subSolution = solve filteredOptions
          return $ M.insert key value <$> subSolution