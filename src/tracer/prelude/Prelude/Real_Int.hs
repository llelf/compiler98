module Prelude(Real(..)) where

--import Ratio((%))

instance Real Int where
  toRational i = error "Real_Int.toRational not yet implemented" --(toInteger i) % 1

