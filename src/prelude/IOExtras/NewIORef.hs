module IOExtras
  ( newIORef
  ) where

import FFI
import DIORef

newIORef :: a -> IO (IORef a)
newIORef a = makeStablePtr a >>= return . IORef
