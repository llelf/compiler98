module Prelude (Show(..)) where

#if 1

import Numeric(showFloat)

instance Show Double where
  showsPrec p = showFloat
  showsType a = showString "Double"

#else

instance Show Double where
  showsPrec p x = showString "<<Double>>"
  showsType a = showString "Double"

#endif
