module IOExtras
  ( readIORef
  ) where

import FFI
import DIORef

readIORef :: IORef a -> IO a
readIORef (IORef r) = deRefStablePtr r
