module IO (hFlush) where

import DHandle
import DIOError
import HGetFileName
import FFI

#if !defined(TRACING)
foreign import hFlushC :: Handle -> IO Int

hFlush :: Handle -> IO ()
hFlush h = do
    i <- hFlushC h
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hFlush" (hGetFileName h) (Just h) errno
      else
        return ()

#else
foreign import hFlushC :: ForeignObj -> IO Int

hFlush :: Handle -> IO ()
hFlush (Handle h) = do
    i <- hFlushC h
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hFlush" (hGetFileName h) (Just (Handle h)) errno
      else
        return ()

#endif
