module IOExtras
  ( readIOArray
  ) where

import Ix
import DIOArray

#if !defined(TRACING)
foreign import primVectorIndexC :: Vector a -> Int -> IO a

#else
_tprim_indexVector primitive 3 :: Trace -> R (Vector a) -> R Int -> R a
indexVector v i = _prim _tprim_indexVector v i
primVectorIndexC = _mkIOok2 indexVector

#endif

readIOArray :: Ix ix => IOArray ix elt -> ix -> IO elt
readIOArray (MkIOArray b v) ix =
    primVectorIndexC v (index b ix)

