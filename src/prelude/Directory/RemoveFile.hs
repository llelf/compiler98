module Directory (removeFile) where

import FFI
import Monad (when)

foreign import unlink :: CString -> IO Int

removeFile :: FilePath -> IO ()
removeFile fp = do
  err <- unlink (toCString fp)
  when (err == -1)
       (do errno <- getErrNo
           throwIOError "removeFile" (Just fp) Nothing errno)
