module IO (hSetBuffering) where

import DHandle
import BufferMode
import NHC.FFI

-- #if !defined(TRACING)
#if 1

foreign import ccall hSetBufferingC :: Handle -> BufferMode -> IO Int

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering h b = do
    x <- hSetBufferingC h b
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSetBuffering "++show b) Nothing (Just h) errno
      else
        return ()

#else

foreign import ccall hSetBufferingC :: ForeignObj -> BufferMode -> IO Int

		-- BufferMode not translated yet
hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering (Handle h) b = do
    x <- hSetBufferingC h b
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSetBuffering "++show b) Nothing
                                             (Just (Handle h)) errno
      else
        return ()

#endif
