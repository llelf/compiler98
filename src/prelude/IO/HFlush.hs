module IO (hFlush) where

import DHandle
import FFI

-- #if !defined(TRACING)
#if 1
foreign import hFlushC :: Handle -> IO Int

hFlush :: Handle -> IO ()
hFlush h = do
    i <- hFlushC h
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hFlush" Nothing (Just h) errno
      else
        return ()

#else
foreign import hFlushC :: ForeignObj -> IO Int

hFlush :: Handle -> IO ()
hFlush (Handle h) = do
    i <- hFlushC h
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hFlush" Nothing (Just (Handle h)) errno
      else
        return ()

#endif
