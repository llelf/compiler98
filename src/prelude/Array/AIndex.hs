module Array where

import Ix
import DArray
import NHC.IOExtras

infixl 9  !

#if !defined(TRACING)
-- primIndex primitive 2 :: Vector a -> Int -> a
foreign import ccall "primVectorIndexC" primIndex :: Vector a -> Int -> a

#else
_tprim_indexVector primitive 3 :: Trace -> R (Vector a) -> R Int -> R a
primIndex v i = _prim _tprim_indexVector v i

#endif

(!)                   :: (Ix a) => Array a b -> a -> b
(!) (MkArray b v) i   =  primIndex v (index b i)

