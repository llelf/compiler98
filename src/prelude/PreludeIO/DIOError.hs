module Prelude where

import PackedString
import IO
import DErrNo

data IOError 
        = IOErrorUser      String 
        | IOErrorSystem    PackedString Int	-- error in system() call
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
        | IOErrorC         String (Maybe String) ErrNo	-- error taken from C's errno


