module IOExtras
  ( writeIOArray
  ) where

#if 0

import FFI
import Ix
import DIOArray
import _E

#if !defined(TRACING)
foreign import primUpdateVectorC :: Int -> _E a -> Vector a -> IO ()

#else
_tprim_updateVector primitive 4
                    :: Trace -> R Int -> R (_E a) -> R (Vector a) -> R ()
updateVector i val vec = _prim _tprim_updateVector i val vec
primUpdateVectorC = _mkIOok3 updateVector

#endif

writeIOArray :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
writeIOArray (MkIOArray b v) ix elt =
    primUpdateVectorC (index b ix) (_E elt) v

#else

import DIOArray
import Ix
import LowVector
import _E

writeIOArray :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
writeIOArray (MkIOArray b v) ix elt =
    primUpdateVectorC (index b ix) (_E elt) v

#endif
