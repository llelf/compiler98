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

