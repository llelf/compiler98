module Prelude where

data World = World

#if !defined(TRACING)
newtype IO a = IO ( World -> Either IOError a)
#else
data    IO a = IO ( World -> Either IOError a)
             | IOPrim a
#endif


