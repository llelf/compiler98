module IOExtras
  ( readIOArray
  ) where

import Ix
import DIOArray

readIOArray :: Ix ix => IOArray ix elt -> ix -> IO elt
readIOArray (MkIOArray b v) ix =
    primVectorIndexC v (index b ix)

foreign import primVectorIndexC :: Vector a -> Int -> IO a
