module Prelude(Ord(..)) where

#if defined(TRACING)
import PrimsDouble
#endif

instance Ord Double where
#if !defined(TRACING)
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC
#else
  a <  b = primDoubleLt a b
  a <= b = primDoubleLe a b
  a >= b = primDoubleGe a b
  a >  b = primDoubleGt a b
#endif
