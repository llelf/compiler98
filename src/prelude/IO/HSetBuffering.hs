module IO where

import IO
import LowIO(primHSetBuffering)

hSetBuffering         :: Handle  -> BufferMode -> IO ()
hSetBuffering h bm     = primHSetBuffering h bm
