module IOExtras
  ( readIORef
  ) where

import DIORef
import ReadIOArray

readIORef :: IORef a -> IO a
readIORef (IORef a) = do
    readIOArray a 0
