module IO (hSetBuffering) where

import DHandle
import BufferMode
import DIOError
import HGetFileName
import FFI

#if !defined(TRACING)

foreign import hSetBufferingC :: Handle -> BufferMode -> IO Int

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering h b = do
    x <- hSetBufferingC h b
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSetBuffering "++show b) (hGetFileName h) (Just h) errno
      else
        return ()

#else

foreign import hSetBufferingC :: ForeignObj -> BufferMode -> IO Int

		-- BufferMode not translated yet
hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering (Handle h) b = do
    x <- hSetBufferingC h b
    if x/=0 then do
        errno <- getErrNo
        throwIOError ("hSetBuffering "++show b) (hGetFileName h)
                                             (Just (Handle h)) errno
      else
        return ()

#endif
