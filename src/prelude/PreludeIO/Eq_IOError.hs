module Prelude(Eq(..)) where

import DIOError

instance Eq IOError where
  IOErrorUser str      == IOErrorUser str' = str == str'
  _ == _ = False
