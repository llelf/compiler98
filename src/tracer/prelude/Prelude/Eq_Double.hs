module Prelude(Eq(..)) where

instance Eq Double where
  a == b = _prim _tprim_EqDouble a b -- a == b    --- MAGIC
  a /= b = _prim _tprim_NEqDouble a b -- a /= b    --- MAGIC

_tprim_EqDouble primitive 3 :: Trace -> R Double -> R Double -> R Bool
_tprim_NEqDouble primitive 3 :: Trace -> R Double -> R Double -> R Bool
