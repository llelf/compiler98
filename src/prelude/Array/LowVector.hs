module Array
  ( primCopyVectorC
  , primNewVectorC
  , primVectorIndexC
  , primUpdateVectorC
  ) where

import PreludeBuiltin(Vector)
import _E

#if !defined(TRACING)
foreign import ccall primCopyVectorC :: Vector a -> IO (Vector a)
foreign import ccall primNewVectorC :: Int -> _E a -> IO (Vector a)
foreign import ccall primVectorIndexC :: Vector a -> Int -> IO a
foreign import ccall primUpdateVectorC :: Int -> _E a -> Vector a -> IO ()


#else
_tprim_copyVector primitive 2 :: Trace -> R (Vector a) -> R (Vector a)
copyVector v = _prim _tprim_copyVector v
primCopyVectorC = _mkIOok1 copyVector

_tprim_newVector primitive 3 :: Trace -> R Int -> R (_E a) -> R (Vector a)
newVectorIO size val = _prim _tprim_newVector size val
primNewVectorC = _mkIOok2 newVectorIO

_tprim_indexVector primitive 3 :: Trace -> R (Vector a) -> R Int -> R a
indexVector v i = _prim _tprim_indexVector v i
primVectorIndexC = _mkIOok2 indexVector

_tprim_updateVector primitive 4
                    :: Trace -> R Int -> R (_E a) -> R (Vector a) -> R ()
updateVector i val vec = _prim _tprim_updateVector i val vec
primUpdateVectorC = _mkIOok3 updateVector


#endif

