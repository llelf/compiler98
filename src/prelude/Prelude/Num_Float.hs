module Prelude(Num(..)) where

import PrimFloatFromInteger
#if defined(TRACING)
import PrimsFloat
#endif

instance Num Float where
#if !defined(TRACING)
 a + b    = a + b 	-- MAGIC
 a - b    = a - b 	-- MAGIC
 a * b    = a * b 	-- MAGIC

 negate a = negate a 	-- MAGIC
 abs    a = abs    a    -- MAGIC
 signum a = signum a    -- MAGIC 

 fromInteger i = primFloatFromInteger i
#else
 a + b    = primFloatAdd a b
 a - b    = primFloatSub a b
 a * b    = primFloatMul a b

 negate a = 0 - a
 abs    a = primFloatAbs a
 signum a = primFloatSignum a

 fromInteger i = primFloatFromIntegerC i
#endif

#if 0
instance Num Float where
 a + b    = _prim _tprim_FloatPlus a b -- a + b       -- MAGIC
 a - b    = _prim _tprim_FloatMinus a b -- a - b       -- MAGIC
 a * b    = _prim _tprim_FloatTimes a b -- a * b       -- MAGIC
 
 negate a = _prim _tprim_FloatNegate a -- negate a    -- MAGIC
 abs    a = _prim _tprim_FloatAbs a -- abs    a    -- MAGIC
 signum a = _prim _tprim_FloatSignum a -- signum a    -- MAGIC
 
 fromInteger i = primFloatFromIntegerC i

_tprim_FloatPlus primitive 3 :: Trace -> R Float -> R Float -> R Float
_tprim_FloatMinus primitive 3 :: Trace -> R Float -> R Float -> R Float
_tprim_FloatTimes primitive 3 :: Trace -> R Float -> R Float -> R Float

_tprim_FloatNegate primitive 2 :: Trace -> R Float -> R Float
_tprim_FloatAbs primitive 2 :: Trace -> R Float -> R Float
_tprim_FloatSignum primitive 2 :: Trace -> R Float -> R Float

_tprim_FloatFromInteger primitive 2 :: Trace -> R Integer -> R Float

#endif
