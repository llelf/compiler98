module IO where

import DIO
import PreludeBuiltin(Handle)
import LowIO(primHGetContents)


hGetContents          :: Handle -> IO String
hGetContents h         = primHGetContents h
