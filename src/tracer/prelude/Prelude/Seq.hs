module Prelude(seq) where

infixr 0 `seq`

seq :: a -> b -> b
seq a b = _prim _tprim_seq a b -- seq a b      -- MAGIC

_tprim_seq primitive 3 :: Trace -> R a -> R b -> R b
