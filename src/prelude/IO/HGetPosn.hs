module IO (hGetPosn) where

import DHandle
import DHandlePosn
import NHC.FFI

-- #if !defined(TRACING)
#if 1
foreign import ccall hGetPosnC :: Handle -> IO (Ptr ())

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn h = do
    p <- hGetPosnC h
    if p==nullPtr then do
        errno <- getErrNo
        throwIOError "hGetPosn" Nothing (Just h) errno
      else do
        f <- newForeignPtr p finalizerFree	-- (free p)  -- nullFunPtr
        return (HandlePosn h f)

#else
foreign import ccall hGetPosnC :: ForeignObj -> IO Addr

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
