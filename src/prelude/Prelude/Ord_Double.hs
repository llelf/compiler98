module Prelude(Ord(..)) where

instance Ord Double where
#if !defined(TRACING)
  a <  b = a <  b -- MAGIC
  a <= b = a <= b -- MAGIC
  a >= b = a >= b -- MAGIC
  a >  b = a >  b -- MAGIC
#endif
