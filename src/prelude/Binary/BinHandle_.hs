module Binary
  ( {-type-} BinHandle(..)
  ) where

import PreludeBuiltin (ForeignObj)

newtype BinHandle = BH ForeignObj
