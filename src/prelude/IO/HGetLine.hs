module IO where

import IO
import HGetChar

hGetLine              :: Handle -> IO String
hGetLine h             = do c  <- hGetChar h
                            cs <- hGetLine h
                            return (c:cs)
