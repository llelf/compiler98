module IOExtras
  ( writeIORef
  ) where

import Ix
import DIORef
import WriteIOArray

writeIORef :: IORef a -> a -> IO ()
writeIORef (IORef f) a = do
    writeIOArray f 0 a

