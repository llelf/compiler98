module IOExtras
  ( freezeIOArray
  ) where

import FFI
import Ix
import DIOArray
import DArray

primCopyVector primitive 1 :: Vector a -> Vector a

freezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
freezeIOArray (MkIOArray b st) =
    let v = primCopyVector st
    in 
    v `seq` return (MkArray b v)
