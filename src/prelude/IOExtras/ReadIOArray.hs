module IOExtras
  ( readIOArray
  ) where

import Ix
import DIOArray

readIOArray :: Ix ix => IOArray ix elt -> ix -> IO elt
readIOArray (MkIOArray b v) ix =
    let i = index b ix
    in
    return (i `seq` primIndex v i)

-- primIndex is used identically for Array and IOArray
primIndex primitive 2 :: Vector a -> Int -> a
