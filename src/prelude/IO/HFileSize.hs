module IO (hFileSize) where

import DHandle
import DIOError
import HGetFileName
import FFI

#if !defined(TRACING)
foreign import primHFileSizeC :: Handle -> IO Integer

hFileSize :: Handle -> IO Integer
hFileSize h = do
    i <- primHFileSizeC h
    if i == -1 then do
        errno <- getErrNo
        mkIOError "hFileSize" (hGetFileName h) (Just h) errno
      else
        return i

#else
foreign import primHFileSizeC :: ForeignObj -> IO Integer

hFileSize :: Handle -> IO Integer
hFileSize (Handle h) = do
    i <- primHFileSizeC h
    if i == -1 then do
        errno <- getErrNo
        mkIOError "hFileSize" (hGetFileName h) (Just (Handle h)) errno
      else
        return i
#endif
