module IOExtras
  ( writeIORef
  ) where

import FFI
import DIORef

foreign import "stableCopy"
        overwriteStablePtr :: StablePtr a -> StablePtr a -> IO ()

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef r) a = do
    s <- makeStablePtr a
    overwriteStablePtr r s
    freeStablePtr s

