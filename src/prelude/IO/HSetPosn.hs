module IO (hSetPosn) where

import DHandle
import DHandlePosn
import FFI

#if !defined(TRACING)

foreign import hSetPosnC :: Handle -> HandlePosn -> IO Int

hSetPosn              :: Handle -> HandlePosn -> IO () 
hSetPosn h p = do
    i <- hSetPosnC h p
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hSetPosn" Nothing (Just h) errno
      else
        return ()

#else

foreign import hSetPosnC :: ForeignObj -> ForeignObj -> IO Int

hSetPosn              :: Handle -> HandlePosn -> IO () 
hSetPosn (Handle h) (HandlePosn p) = do
    i <- hSetPosnC h p
    if i/=0 then do
        errno <- getErrNo
        throwIOError "hSetPosn" Nothing (Just (Handle h)) errno
      else
        return ()

#endif
