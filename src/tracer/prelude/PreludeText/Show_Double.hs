module Prelude(Show(..)) where

import Text(showSigned,showFloat)

instance Show Double where
  showsPrec = showSigned showFloat

  showsType a = showString "Double"
