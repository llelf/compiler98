{-# OPTIONS_COMPILE -prelude #-}
module IOExtras
  ( IORef(..)
  ) where

import FFI
import PreludeBuiltin (Vector)
import IO (Handle)	-- hack to avoid compiler error with -T

newtype IORef a = IORef (Vector a)	-- vector contains only one element!
