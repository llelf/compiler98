module IO (hSetBuffering) where

import DHandle
import BufferMode
import DIOError
import HGetFileName

#if !defined(TRACING)

foreign import hSetBufferingC :: Handle -> BufferMode -> Either Int ()

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering h b = _mkIOwf2 (IOErrorC ("hSetBuffering "++show b)
                                       (hGetFileName h) . toEnum)
                        hSetBufferingC h b

#else

foreign import hSetBufferingC :: ForeignObj -> BufferMode -> Either Int ()

		-- BufferMode not translated yet
hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering (Handle h) b = _mkIOwf2 (IOErrorC ("hSetBuffering "++show b)
                                                (hGetFileName h) . toEnum)
                                 hSetBufferingC h b

#endif
