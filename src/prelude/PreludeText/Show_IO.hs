module Prelude (Show(..)) where

import DIO

instance (Show a) => Show (IO a) where
  showsPrec d a = showString "<<IO action>>"

  showsType a = showString "(IO " . showsType ta . showChar ')'
		   where (IO fta) = a
                         (Right ta) = fta World 
