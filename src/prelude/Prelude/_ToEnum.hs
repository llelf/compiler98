module Prelude where

_toEnum:: Int -> a
#if !defined(TRACING)
_toEnum a = _toEnum a -- MAGIC
#else
_toEnum a = _prim _tprim_ToEnum a
_tprim_ToEnum primitive 2 :: Trace -> R Int -> R a
#endif
