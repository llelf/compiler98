module IOExtras
  ( IORef(..)
  ) where

import FFI

newtype IORef a = IORef (StablePtr a)
