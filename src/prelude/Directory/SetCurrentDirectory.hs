module Directory (setCurrentDirectory) where

import FFI
import Monad (when)

foreign import chdir :: PackedString -> IO Int

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory fp = do
  err <- chdir (toCString fp)
  when (err == -1)
       ( do errno <- getErrNo
            throwIOError "setCurrentDirectory" (Just fp) Nothing errno)
