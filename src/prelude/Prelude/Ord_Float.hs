module Prelude(Ord(..)) where

#if defined(TRACING)
import PrimsFloat
#endif

instance Ord Float where
#if !defined(TRACING)
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC
#else
  a <  b = primFloatLt a b
  a <= b = primFloatLe a b
  a >= b = primFloatGe a b
  a >  b = primFloatGt a b
#endif
