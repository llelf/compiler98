module IO (hGetPosn) where

import DHandle
import DHandlePosn
import FFI

#if !defined(TRACING)
foreign import hGetPosnC :: Handle -> IO Addr

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn h = do
    a <- hGetPosnC h
    if a==nullAddr then do
        errno <- getErrNo
        throwIOError "hGetPosn" Nothing (Just h) errno
      else do
        f <- newForeignObj a (free (Ptr a))
        return (HandlePosn h f)

#else
foreign import hGetPosnC :: ForeignObj -> IO Addr

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn h@(Handle hfo) = do
    a <- hGetPosnC hfo
    if a==nullAddr then do
        errno <- getErrNo
        throwIOError "hGetPosn" Nothing (Just h) errno
      else do
        f <- newForeignObj a (free (Ptr a))
        return (HandlePosn h f)

#endif
