module IOExtras
  ( writeIOArray
  ) where

import FFI
import Ix
import DIOArray
import _E

writeIOArray :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
writeIOArray (MkIOArray b v) ix elt =
    primUpdateVectorC (index b ix) (_E elt) v

foreign import primUpdateVectorC :: Int -> _E a -> Vector a -> IO ()
