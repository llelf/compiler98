module Prelude(seq) where

infixr 0 `seq`

seq         :: a -> b -> b

#if !defined(TRACING)
seq a b      = _seq a b      -- MAGIC (converted to a single bytecode)
#else
seq a b = _prim _tprim_seq a b

_tprim_seq primitive 3 :: Trace -> R a -> R b -> R b
#endif
