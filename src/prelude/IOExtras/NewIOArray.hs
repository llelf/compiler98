module IOExtras
  ( newIOArray
  ) where

#if 0

import Ix
import DIOArray
import _E

#if !defined(TRACING)
foreign import primNewVectorC :: Int -> _E a -> IO (Vector a)

#else
_tprim_newVector primitive 3 :: Trace -> R Int -> R (_E a) -> R (Vector a)
newVectorIO size val = _prim _tprim_newVector size val
primNewVectorC = _mkIOok2 newVectorIO

#endif

newIOArray :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
newIOArray bounds elt = do
    v <- primNewVectorC (rangeSize bounds) (_E elt)
    return (MkIOArray bounds v)

#else

import Ix
import DIOArray
import LowVector
import _E

newIOArray :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
newIOArray bounds elt = do
    v <- primNewVectorC (rangeSize bounds) (_E elt)
    return (MkIOArray bounds v)

#endif
