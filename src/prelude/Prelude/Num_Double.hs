module Prelude(Num(..)) where

import PrimDoubleFromInteger
#if defined(TRACING)
import PrimsDouble
#endif

instance Num Double where

#if !defined(TRACING)
 a + b    = a + b       -- MAGIC
 a - b    = a - b       -- MAGIC
 a * b    = a * b       -- MAGIC
 
 negate a = negate a    -- MAGIC
 abs    a = abs    a    -- MAGIC
 signum a = signum a    -- MAGIC
 
 fromInteger i = primDoubleFromInteger i
#else
 a + b    = primDoubleAdd a b
 a - b    = primDoubleSub a b
 a * b    = primDoubleMul a b
 
 negate a = (0 - a)
 abs    a = primDoubleAbs a
 signum a = primDoubleSignum a
 
 fromInteger i = primDoubleFromIntegerC i
#endif

