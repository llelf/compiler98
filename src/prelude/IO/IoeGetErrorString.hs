module IO (ioeGetErrorString) where

import IO
import DIOError
import PackedString
import DErrNo

ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOErrorUser             str)       = str
ioeGetErrorString (IOErrorSystem           str errno) = unpackPS str
ioeGetErrorString (IOErrorOpen       file mode errno) = "openFile "++unpackPS file
ioeGetErrorString (IOErrorEOF           handle op)    = "EOF"
ioeGetErrorString (IOErrorHIsEOF        handle errno) = "hIsEOF"
ioeGetErrorString (IOErrorHFileSize     handle errno) = "hFileSize"
ioeGetErrorString (IOErrorHFlush        handle errno) = "hFlush"
ioeGetErrorString (IOErrorHSeek         handle errno) = "hSeek"
ioeGetErrorString (IOErrorHGetPosn      handle errno) = "hGetPosn"
ioeGetErrorString (IOErrorHSetPosn      handle errno) = "hSetPosn"
ioeGetErrorString (IOErrorHGetBuffering handle errno) = "hGetBuffering"
ioeGetErrorString (IOErrorHSetBuffering handle errno) = "hSetBuffering"
ioeGetErrorString (IOErrorC errno)                    = show errno
ioeGetErrorString _ = "unusual IO error"

