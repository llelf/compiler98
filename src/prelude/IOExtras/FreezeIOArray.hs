module IOExtras
  ( freezeIOArray
  ) where

import FFI
import Ix
import DIOArray
import DArray

foreign import primCopyVectorC :: Vector a -> Vector a

freezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
freezeIOArray (MkIOArray b st) =
    let v = primCopyVectorC st
    in 
    v `seq` return (MkArray b v)
