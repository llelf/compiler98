module Prelude(Num(..)) where

#if !defined(TRACING)
import PrimDoubleFromInteger

instance Num Double where
 a + b    = a + b       -- MAGIC
 a - b    = a - b       -- MAGIC
 a * b    = a * b       -- MAGIC
 
 negate a = negate a    -- MAGIC
 abs    a = abs    a    -- MAGIC
 signum a = signum a    -- MAGIC
 
 fromInteger i = primDoubleFromInteger i

#else

instance Num Double where
 a + b    = _prim _tprim_DoublePlus a b -- a + b       -- MAGIC
 a - b    = _prim _tprim_DoubleMinus a b -- a - b       -- MAGIC
 a * b    = _prim _tprim_DoubleTimes a b -- a * b       -- MAGIC
 
 negate a = _prim _tprim_DoubleNegate a -- negate a    -- MAGIC
 abs    a = _prim _tprim_DoubleAbs a -- abs    a    -- MAGIC
 signum a = _prim _tprim_DoubleSignum a -- signum a    -- MAGIC
 
 fromInteger i = _prim _tprim_DoubleFromInteger i

_tprim_DoublePlus primitive 3 :: Trace -> R Double -> R Double -> R Double
_tprim_DoubleMinus primitive 3 :: Trace -> R Double -> R Double -> R Double
_tprim_DoubleTimes primitive 3 :: Trace -> R Double -> R Double -> R Double

_tprim_DoubleNegate primitive 2 :: Trace -> R Double -> R Double
_tprim_DoubleAbs primitive 2 :: Trace -> R Double -> R Double
_tprim_DoubleSignum primitive 2 :: Trace -> R Double -> R Double

_tprim_DoubleFromInteger primitive 2 :: Trace -> R Integer -> R Double

#endif
