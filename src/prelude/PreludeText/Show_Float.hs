module Prelude (Show(..)) where

import Numeric(showFloat)

instance Show Float where
  showsPrec p = showFloat
  showsType a = showString "Float"

