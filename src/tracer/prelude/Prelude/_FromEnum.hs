module Prelude where

_fromEnum:: a -> Int
_fromEnum a = _prim _tprim_FromEnum a -- _fromEnum a -- MAGIC

_tprim_FromEnum primitive 2 :: Trace -> R a -> R Int
