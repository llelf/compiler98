module Prelude(Ord(..)) where

instance Ord Float where
#if !defined(TRACING)
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC
#endif
