module Prelude where

import DIOError

userError :: String -> IOError
userError s = IOErrorUser s
 
