module Prelude (Show(..)) where

import Numeric(showFloat)

instance Show Double where
  showsPrec p = showFloat
  showsType a = showString "Double"

