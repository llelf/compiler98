module IOExtras
  ( newIORef
  ) where

import Ix
import DIORef
import NewIOArray

newIORef :: a -> IO (IORef a)
newIORef a = do a <- newIOArray (0,0) a
                return (IORef a)
