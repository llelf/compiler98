module IOExtras
  ( newIOArray
  ) where

import Ix
import DIOArray
import _E

foreign import primNewVectorC :: Int -> _E a -> IO (Vector a)

newIOArray :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
newIOArray bounds elt = do
    v <- primNewVectorC (rangeSize bounds) (_E elt)
    return (MkIOArray bounds v)
