module Prelude where

_fromEnum:: a -> Int

#if !defined(TRACING)
_fromEnum a = _fromEnum a -- MAGIC
#else
_fromEnum a = _prim _tprim_FromEnum a
_tprim_FromEnum primitive 2 :: Trace -> R a -> R Int
#endif
