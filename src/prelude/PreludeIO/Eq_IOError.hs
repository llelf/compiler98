module Prelude(Eq(..)) where

import DIOError
import DErrNo

instance Eq IOError where
  (IOErrorUser str)      == (IOErrorUser str')	=  str == str'
  (IOErrorC e)		 == (IOErrorC e')	=    e == e'
  _ == _ = False
