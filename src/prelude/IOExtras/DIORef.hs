module IOExtras
  ( IORef(..)
  ) where

import FFI
import DIOArray

newtype IORef a = IORef (IOArray Int a)	-- contains only one element!

