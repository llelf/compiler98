module IO (hSetPosn) where

import DHandle
import DHandlePosn
import FFI

-- #if !defined(TRACING)
#if 1

foreign import hSetPosnC :: Handle -> ForeignObj -> IO Int

hSetPosn              :: HandlePosn -> IO () 
hSetPosn (HandlePosn h p) = do
    i <- hSetPosnC h p
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hSetPosn" Nothing (Just h) errno
      else
        return ()

#else

foreign import hSetPosnC :: ForeignObj -> ForeignObj -> IO Int

hSetPosn              :: HandlePosn -> IO () 
hSetPosn (HandlePosn h@(Handle hfo) p) = do
    i <- hSetPosnC hfo p
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hSetPosn" Nothing (Just h) errno
      else
        return ()

#endif
