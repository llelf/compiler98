module IO where

import IO
import LowIO(primHSeek)

hSeek                 :: Handle -> SeekMode -> Integer -> IO () 
hSeek h seekmode i     = primHSeek h seekmode i
