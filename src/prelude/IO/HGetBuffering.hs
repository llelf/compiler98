module IO where

import IO
import LowIO(primHGetBuffering)

hGetBuffering         :: Handle  -> IO BufferMode
hGetBuffering h        = primHGetBuffering h
