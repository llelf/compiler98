module Prelude (Show(..)) where

#if 1

import Numeric(showSigned,showFloat)

instance Show Double where
  showsPrec = showSigned showFloat

  showsType a = showString "Double"

#else

instance Show Double where
  showsPrec p x = showString "<<Double>>"
  showsType a = showString "Double"

#endif
