module IOExtras
  ( freezeIOArray
  ) where

import FFI
import Ix
import DIOArray
import DArray

#if !defined(TRACING)
foreign import primCopyVectorC :: Vector a -> Vector a

#else
_tprim_copyVector primitive 2 :: Trace -> R (Vector a) -> R (Vector a)
primCopyVectorC v = _prim _tprim_copyVector v

#endif

freezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
freezeIOArray (MkIOArray b st) =
    let v = primCopyVectorC st
    in 
    v `seq` return (MkArray b v)

