module Prelude(Eq(..)) where

#if defined(TRACING)
import PrimsDouble
#endif

instance Eq Double where
#if !defined(TRACING)
  a == b = a == b    --- MAGIC
  a /= b = a /= b    --- MAGIC
#else
  a == b = primDoubleEq a b
  a /= b = primDoubleNe a b
#endif
