module IOExtras
  ( IOArray(..)
  , Vector
  ) where

import PreludeBuiltin(Vector)

data (Ix ix) =>
     IOArray ix elt = MkIOArray (ix,ix) (Vector elt)
