module Prelude(Eq(..)) where

import DIOError
import DErrNo

instance Eq IOError where
  (IOErrorUser str)      == (IOErrorUser str')	=  str == str'
  (IOErrorC _ _ e)	 == (IOErrorC _ _ e')	=    e == e'
  _ == _ = False
