module IO where

import IO
import LowIO(primHGetPosn)

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn h             = primHGetPosn h
