module IO where

import IO
import LowIO(primHSetPosn)

hSetPosn              :: Handle -> HandlePosn -> IO () 
hSetPosn h hp          = primHSetPosn h hp

