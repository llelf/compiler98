module Prelude(Eq(..)) where

#if defined(TRACING)
import PrimsFloat
#endif


instance Eq Float where
#if !defined(TRACING)
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC
#else
  a == b = primFloatEq a b
  a /= b = primFloatNe a b
#endif
