module IO where

import IO
import DIOError

ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOErrorUser             str)       = str
ioeGetErrorString (IOErrorSystem           str errno) = str
ioeGetErrorString (IOErrorOpen       file mode errno) = "openFile "++file
ioeGetErrorString (IOErrorEOF           handle op)    = "EOF"
ioeGetErrorString (IOErrorHIsEOF        handle errno) = "hIsEOF"
ioeGetErrorString (IOErrorHFileSize     handle errno) = "hFileSize"
ioeGetErrorString (IOErrorHFlush        handle errno) = "hFlush"
ioeGetErrorString (IOErrorHSeek         handle errno) = "hSeek"
ioeGetErrorString (IOErrorHGetPosn      handle errno) = "hGetPosn"
ioeGetErrorString (IOErrorHSetPosn      handle errno) = "hSetPosn"
ioeGetErrorString (IOErrorHGetBuffering handle errno) = "hGetBuffering"
ioeGetErrorString (IOErrorHSetBuffering handle errno) = "hSetBuffering"
ioeGetErrorString _ = "unusual IO error"

