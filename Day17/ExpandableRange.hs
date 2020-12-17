module ExpandableRange where

import Vector3 as V3
import Vector4 as V4

class ExpandableRange a where
  expandToInclude :: a -> (a, a) -> (a, a)
  expand :: (a, a) -> (a, a)

instance ExpandableRange Vector3 where
  expand (lower, upper) =
    (lower <> Vector3 (-1) (-1) (-1), upper <> Vector3 1 1 1)
  expandToInclude =
    V3.expandBounds

instance ExpandableRange Vector4 where
  expand (lower, upper) =
    (lower <> Vector4 (-1) (-1) (-1) (-1), upper <> Vector4 1 1 1 1)
  expandToInclude =
    V4.expandBounds