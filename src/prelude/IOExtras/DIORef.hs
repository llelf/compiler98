{-# OPTIONS_COMPILE -prelude #-}
module IOExtras
  ( IORef(..)
  ) where

import FFI
import PreludeBuiltin(Vector)

newtype IORef a = IORef (Vector a)	-- vector contains only one element!
