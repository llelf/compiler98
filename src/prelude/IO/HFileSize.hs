module IO where

import IO
import LowIO(primHFileSize)

hFileSize             :: Handle -> IO Integer
hFileSize h            = primHFileSize h

