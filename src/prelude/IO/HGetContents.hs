module IO where

import IO
import LowIO(primHGetContents)

hGetContents          :: Handle -> IO String
hGetContents h         = primHGetContents h
