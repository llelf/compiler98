module Prelude(Real(..)) where

import Ratio((%))

instance Real Int where
#if !defined(TRACING)
  toRational i = (toInteger i) % 1
#else
  toRational i = error "Real_Int.toRational not yet implemented"
#endif

