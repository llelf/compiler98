module Prelude where

import PackedString(PackedString)
import PreludeBuiltin(Handle)

data IOError 
        = IOErrorUser      String 
        | IOErrorSystem    PackedString Int
        | IOErrorOpen      PackedString PackedString Int
	| IOErrorEOF       Handle String
        | IOErrorHIsEOF    Handle Int
        | IOErrorHFileSize Handle Int
        | IOErrorHFlush    Handle Int
        | IOErrorHSeek     Handle Int
        | IOErrorHGetPosn  Handle Int
        | IOErrorHSetPosn  Handle Int
	| IOErrorHGetBuffering Handle Int        
	| IOErrorHSetBuffering Handle Int        


