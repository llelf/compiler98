module Directory (getCurrentDirectory) where

import FFI
import Monad

foreign import getcwd        :: Addr -> Int -> IO Addr
foreign cast   addrToCString :: Addr -> CString

getCurrentDirectory :: IO FilePath
getCurrentDirectory = do
  a <- getcwd nullAddr 1024
  when (a == nullAddr)
       (do errno <- getErrNo
           throwIOError "getCurrentDirectory" (Just (show a)) Nothing errno)
  return (fromCString (addrToCString a))
